#!/usr/bin/env python
'''Correlaion example'''

import sys
import math
import time
import random
sys.path.append('../gen-py')

from com.maxeler.correlation import correlationService
from com.maxeler.correlation.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

correlation_maxNumVariables = 6000
correlation_maxNum_timeseries = 6000
correlation_numTopScores = 10
correlation_numPipes = 12
correlation_PCIE_ALIGNMENT = 16
correlation_numVectorsPerBurst = 2

def random_data(num_timeseries, size_timeseries):
    '''Generates random data.'''
    return [[random.random() for _ in range(size_timeseries)]
            for _ in range(num_timeseries)]

def calc_num_bursts(num_timeseries):
    '''Calculate number of bursts for initializing LMem.'''
    num_vectors = 0
    for i in range(1, num_timeseries + 1):
        num_vectors += ((i + (correlation_numPipes - 1)) /
                        correlation_numPipes)
    return ((num_vectors + (correlation_numVectorsPerBurst-1)) /
            correlation_numVectorsPerBurst)

def prepare_data_for_dfe(data, size_timeseries, num_timeseries, num_timesteps,
                         window_size, precalculations, data_pairs):
    '''Precalculates and reorders data for the DFE.'''
    if num_timeseries > correlation_maxNum_timeseries:
        sys.stderr.write(
            'Number of Time series should be less or equal to ' +
            str(correlation_maxNum_timeseries) +
            '. Terminating!')
        sys.stderr.flush()
        sys.exit(-1)
    elif window_size < 2:
        sys.stderr.write(
            'Window size must be equal or greater than 2. Terminating!')
        sys.stderr.flush()
        sys.exit(-1)
    elif num_timesteps > size_timeseries:
        sys.stderr.write(
            'Number of Time steps should be less or equal to' +
            ' size of Time series. Terminating!')
        sys.stderr.flush()
        sys.exit(-1)

    sums = [[0.0] * num_timeseries] * num_timesteps
    sums_sq = [[0.0] * num_timeseries] * num_timesteps
    inv = [[0.0] * num_timeseries] * num_timesteps

    # 2 DFE input streams: precalculations and data pairs
    for i in range(num_timesteps):
        for j in range(num_timeseries):
            old = 0.0
            if i >= window_size:
                old = data[j][i - window_size]
            new = data[j][i]

            if i == 0:
                sums[i][j] = new
                sums_sq[i][j] = new * new
            else:
                sums[i][j] = sums[i-1][j] + new - old
                sums_sq[i][j] = sums_sq[i-1][j] + new * new - old * old

            inv[i][j] = 1 / math.sqrt(window_size * sums_sq[i][j] -
                                      sums[i][j] * sums[i][j])

            # Precalculations REORDERED in DFE ORDER
            precalculations.append(sums[i][j])
            precalculations.append(inv[i][j])

            # Data pairs REORDERED in DFE ORDER
            data_pairs.append(new)
            data_pairs.append(old)

def calc_num_correlations(num_timeseries):
    '''Calculates number  of correlations.'''
    return (num_timeseries * (num_timeseries - 1)) / 2

def calc_index(i, j):
    '''Calculates index of correlation between i and j'''
    if i == j:
        print 'i and j must not be the same!'
        return -1
    if i < j:
        i, j = j, i

    return (i*(i-1))/2+j

def correlate_dfe(data, size_timeseries, num_timeseries, correlations):
    '''Calculates correlations on DFE.'''
    try:
        # Make socket
        transport = TSocket.TSocket('localhost', 9090)

        # Buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(transport)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = correlationService.Client(protocol)

        # Connect!
        transport.open()

        # Initialize maxfile
        max_file = client.correlation_init();

        # Load DFE
        engine = client.max_load(max_file, "*");

        start_time = time.time()

        num_timesteps = size_timeseries
        window_size = size_timeseries

        num_bursts = calc_num_bursts(num_timeseries)
        loop_length = client.correlation_get_CorrelationKernel_loopLength()

        precalculations = []
        data_pairs = []

        burst_size = 384 / 2  # for anything other than ISCA this should be 384
        in_mem_load = [0] * (num_bursts * burst_size)

        prepare_data_for_dfe(data, size_timeseries,
                             num_timeseries, num_timesteps,
                             window_size, precalculations, data_pairs)

        reorder_time = time.time() - start_time
        print 'Data reordering time:       %.5lfs' % reorder_time

        start_time = time.time()

        # Allocate and send input streams to server
        address_loop_length = client.malloc_int32_t(1)
        client.send_data_int32_t(address_loop_length, [loop_length])

        address_in_mem_load = client.malloc_int32_t(num_bursts * burst_size)
        client.send_data_int32_t(address_in_mem_load, in_mem_load)

        address_precalculations = client.malloc_double(
            2 * num_timeseries * num_timesteps)
        client.send_data_double(address_precalculations, precalculations)

        address_data_pairs = client.malloc_double(
            2 * num_timeseries * num_timesteps)
        client.send_data_double(address_data_pairs, data_pairs)

        # Allocate memory for output stream on server
        address_out_correlation = client.malloc_double(
            num_timesteps * loop_length * correlation_numTopScores *
            correlation_numPipes + num_bursts * 24)
        address_out_indices = client.malloc_int32_t(
            2 * num_timesteps * loop_length * correlation_numTopScores *
            correlation_numPipes)
        
        actions = client.max_actions_init(max_file, "loadLMem")
        client.max_set_param_uint64t(actions, "numBursts", num_bursts)
        client.max_get_offset_auto_loop_size(actions, "CorrelationKernel", "loopLength")
        client.max_queue_input(actions, "in_memLoad", address_in_mem_load, num_bursts * burst_size)
        client.max_ignore_scalar(actions, "LMemCommandsKernel", "run_cycle_count" )
        client.max_ignore_scalar(actions, "CorrelationKernel", "run_cycle_count" )

        client.max_run(engine, actions)
        print 'LMem initialized!'

        actions = client.max_actions_init(max_file, "default")
        client.max_set_param_uint64t(actions, "numBursts", num_bursts)
        client.max_set_param_uint64t(actions, "numSteps", num_timesteps)
        client.max_set_param_uint64t(actions, "numVariables", num_timeseries)
        client.max_set_param_uint64t(actions, "outputLastStep", 1)
        client.max_set_param_double(actions, "windowSize", window_size)
        client.max_queue_input(actions, "in_precalculations", address_precalculations,
                               2 * num_timeseries * num_timesteps * 8)
        client.max_queue_input(actions, "in_variable_pair", address_data_pairs,
                               2 * num_timeseries * num_timesteps * 8)
        client.max_queue_output(actions, "out_correlation", address_out_correlation,
                            (num_timesteps * loop_length * correlation_numTopScores * correlation_numPipes +
                             num_bursts * 24) * 8)  # for anything other than ISCA 48 should be instead of 24
        client.max_queue_output(actions, "out_indices", address_out_indices,
                                2 * num_timesteps * loop_length *
                                 correlation_numTopScores * correlation_numPipes * 4)
    

        client.max_run(engine, actions)
        client.free(actions)

        # Unload DFE
        client.max_unload(engine)

        # Get output stream from server
        out_correlation = client.receive_data_double(
            address_out_correlation, num_timesteps * loop_length *
            correlation_numTopScores * correlation_numPipes + num_bursts * 48)
        out_indices = client.receive_data_int32_t(
            address_out_indices, 2 * num_timesteps * loop_length *
            correlation_numTopScores * correlation_numPipes)
        loop_length = client.receive_data_int32_t(address_loop_length, 1)
        
        # Free allocated memory for streams on server
        client.free(address_loop_length)
        client.free(address_in_mem_load)
        client.free(address_precalculations)
        client.free(address_data_pairs)
        client.free(address_out_correlation)
        client.free(address_out_indices)

        # Free allocated maxfile data
        client.correlation_free()

        # Close!
        transport.close()

        dfe_time = time.time() - start_time
        print 'DFE execution time:         %.5lfs' % dfe_time

        start_time = time.time()

        position = 0
        index = 0
        start = ((num_timesteps-1) * loop_length[0] *
                 correlation_numTopScores * correlation_numPipes)

        for i in range(num_timeseries):
            for j in range(i):
                correlations[index+j] = out_correlation[start+position+j]
            index += i
            position += ((i/12)+1)*12

        store_time = time.time() - start_time
        print 'Storing time:               %.5lfs' % store_time

    except Thrift.TException, thrift_exception:
        print '%s' % (thrift_exception.message)
        sys.exit(-1)

def correlate_cpu(data, num_timeseries, window_size,
                  num_timesteps, correlations_cpu, indices_step):
    '''Calculates correlations on CPU.'''
    sums = [0.0] * num_timeseries
    sums_sq = [0.0] * num_timeseries
    sums_xy = [0.0] * calc_num_correlations(num_timeseries)
    for k in range(num_timesteps):
        index_correlation = 0
        for i in range(num_timeseries):
            old = 0.0
            if k >= window_size:
                old = data[i][k - window_size]
            new = data[i][k]
            sums[i] += new - old
            sums_sq[i] += new * new - old * old
        for i in range(num_timeseries):
            old_x = 0.0
            if k >= window_size:
                old_x = data[i][k - window_size]
            new_x = data[i][k]
            for j in range(i):
                old_y = 0.0
                if k >= window_size:
                    old_y = data[j][k - window_size]
                new_y = data[j][k]
                sums_xy[index_correlation] += new_x * new_y - old_x * old_y
                numerator = (window_size * sums_xy[index_correlation] -
                             sums[i] * sums[j])
                denominator = ((1 / math.sqrt(window_size * sums_sq[i] -
                                              sums[i] * sums[i])) *
                               (1 / math.sqrt(window_size * sums_sq[j] -
                                              sums[j] * sums[j])))
                correlations_cpu[index_correlation] = numerator * denominator
                indices_step[2 * index_correlation] = j
                indices_step[2 * index_correlation + 1] = i
                index_correlation += 1

def check(correlations_dfe, correlations_cpu, num_timeseries, indices_step):
    '''Checks if correlations_dfe and correlations_cpu are the same.'''
    failed = 0
    for i in range(num_timeseries * (num_timeseries - 1) / 2):
        j = calc_index(indices_step[2 * i], indices_step[2 * i + 1])
        if correlations_dfe[j] != correlations_cpu[i]:
            failed += 1
            print('correlationCPU[%d]\t=  %.20f' %
                  (i, correlations_cpu[i]))
            print('correlationDFE[%d]\t=  %.20f' %
                  (j, correlations_dfe[j]))

    if not failed:
        print 'Test passed!'
    else:
        print 'Test failed %d times.' % failed
        sys.exit(-1)

def test():
    '''
    Calls correlate_dfe and correlate_cpu
    and checks if they return the same result.
    '''
    size_timeseries = 100
    num_timeseries = 250

    data = random_data(num_timeseries, size_timeseries)
    correlations_dfe = [0.0] * calc_num_correlations(num_timeseries)
    correlations_cpu = [0.0] * calc_num_correlations(num_timeseries)
    indices_step = [0.0] * calc_num_correlations(num_timeseries) * 2

    start_time = time.time()
    correlate_dfe(data, size_timeseries, num_timeseries, correlations_dfe)
    dfe_total = time.time() - start_time
    print 'DFE correlation total time: %.5lfs' % dfe_total

    start_time = time.time()
    correlate_cpu(data, num_timeseries, size_timeseries,
                  size_timeseries, correlations_cpu, indices_step)
    cpu_total = time.time() - start_time
    print 'CPU correlation total time: %.5lfs' % cpu_total

    check(correlations_dfe, correlations_cpu, num_timeseries, indices_step)

if __name__ == '__main__':
    test()
