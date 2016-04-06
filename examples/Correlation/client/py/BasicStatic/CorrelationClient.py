#!/usr/bin/env python
"""Correlaion example"""

import sys
import math
import time
import random
sys.path.append('../gen-py')

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from com.maxeler.correlation import correlationService

CORRELATION_MAX_NUM_VARIABLES = 6000
CORRELATION_MAX_NUM_TIMESERIES = 6000
CORRELATION_NUM_TOP_SCORES = 10
CORRELATION_NUM_PIPES = 12
CORRELATION_PCIE_ALIGNMENT = 16
CORRELATION_NUM_VECTORS_PER_BURST = 2

def random_data(num_timeseries, size_timeseries):
    """Generates random data."""
    return [[random.random() for _ in range(size_timeseries)]
            for _ in range(num_timeseries)]

def calc_num_bursts(num_timeseries):
    """Calculate number of bursts for initializing LMem."""
    num_vectors = 0
    for i in range(1, num_timeseries + 1):
        num_vectors += ((i + (CORRELATION_NUM_PIPES - 1)) /
                        CORRELATION_NUM_PIPES)
    return ((num_vectors + (CORRELATION_NUM_VECTORS_PER_BURST-1)) /
            CORRELATION_NUM_VECTORS_PER_BURST)

def prepare_data_for_dfe(data, size_timeseries, num_timeseries):
    """Precalculates and reorders data for the DFE."""
    num_timesteps = size_timeseries
    window_size = size_timeseries

    if num_timeseries > CORRELATION_MAX_NUM_TIMESERIES:
        sys.stderr.write(
            'Number of Time series should be less or equal to ' +
            str(CORRELATION_MAX_NUM_TIMESERIES) +
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

    precalculations = []
    data_pairs = []

    sums = [[0.0] * num_timeseries] * num_timesteps
    sums_sq = [[0.0] * num_timeseries] * num_timesteps
    inv = [[0.0] * num_timeseries] * num_timesteps

    # 2 DFE input streams: precalculations and data pairs
    for i in range(num_timesteps):
        for j in range(num_timeseries):
            old = 0.0 if i < window_size else data[j][i - window_size]
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

    return (precalculations, data_pairs)

def calc_num_correlations(num_timeseries):
    """Calculates number  of correlations."""
    return (num_timeseries * (num_timeseries - 1)) / 2

def calc_index(i, j):
    """Calculates index of correlation between i and j"""
    if i == j:
        print 'i and j must not be the same!'
        return -1
    if i < j:
        i, j = j, i

    return (i*(i-1))/2+j

def correlate_dfe(data, size_timeseries, num_timeseries, correlations):
    """Calculates correlations on DFE."""
    start_time = time.time()

    # Make socket
    socket = TSocket.TSocket('localhost', 9090)

    # Buffering is critical. Raw sockets are very slow
    transport = TTransport.TBufferedTransport(socket)

    # Wrap in a protocol
    protocol = TBinaryProtocol.TBinaryProtocol(transport)

    # Create a client to use the protocol encoder
    client = correlationService.Client(protocol)

    current_time = time.time() - start_time
    print 'Creating a client:\t\t\t\t%.5lfs' % current_time

    try:
        # Connect!
        start_time = time.time()
        transport.open()
        current_time = time.time() - start_time
        print 'Opening connection:\t\t\t\t%.5lfs' % current_time

        num_timesteps = size_timeseries
        window_size = size_timeseries
        num_bursts = calc_num_bursts(num_timeseries)

        # Get loop length
        start_time = time.time()
        loop_length = client.correlation_get_CorrelationKernel_loopLength()
        current_time = time.time() - start_time
        print 'Getting Correlation Kernel loopLength:\t\t%.5lfs' % current_time

        # Prepare data for DFE
        start_time = time.time()

        burst_size = 384 / 2  # for anything other than ISCA this should be 384
        in_mem_load = [0] * (num_bursts * burst_size)

        precalculations, data_pairs = prepare_data_for_dfe(data,
                                                           size_timeseries,
                                                           num_timeseries)

        current_time = time.time() - start_time
        print 'Data reordering time:\t\t\t\t%.5lfs' % current_time

        # Allocate and send input streams to server
        start_time = time.time()
        loop_length_size = 1
        address_loop_length = client.malloc_int32_t(loop_length_size)
        client.send_data_int32_t(address_loop_length, [loop_length])
        loop_length_time = time.time() - start_time
        print ('\tSending LoopLength:\t\t(size = %d bit)\t\t%.5lfs' %
               (32 * loop_length_size, loop_length_time))

        start_time = time.time()
        in_mem_load_size = num_bursts * burst_size
        address_in_mem_load = client.malloc_int32_t(in_mem_load_size)
        client.send_data_int32_t(address_in_mem_load, in_mem_load)
        in_mem_load_time = time.time() - start_time
        print ('\tSending InMemLoad:\t\t(size = %d bit)\t%.5lfs' %
               (32 * in_mem_load_size, in_mem_load_time))

        start_time = time.time()
        precalculations_size = 2 * num_timeseries * num_timesteps
        address_precalculations = client.malloc_double(precalculations_size)
        client.send_data_double(address_precalculations, precalculations)
        precalculations_time = time.time() - start_time
        print ('\tSending Precalculations:\t(size = %d bit)\t%.5lfs' %
               (64 * precalculations_size, precalculations_time))

        start_time = time.time()
        data_pairs_size = 2 * num_timeseries * num_timesteps
        address_data_pairs = client.malloc_double(data_pairs_size)
        client.send_data_double(address_data_pairs, data_pairs)
        data_pairs_time = time.time() - start_time
        print ('\tSending DataPairs:\t\t(size = %d bit)\t%.5lfs' %
               (64 * data_pairs_size, data_pairs_time))

        current_time = (loop_length_time + in_mem_load_time +
                        precalculations_time + data_pairs_time)
        speed = ((32 * loop_length_size + 32 * in_mem_load_size +
                  64 * precalculations_size + 64 * data_pairs_size) /
                 (current_time * 1000000))
        print ('Sending input streams to server total time:\t%.5lfs' %
               current_time + '\t(average speed = %.5lfMb/s)' % (speed))

        # Allocate memory for output stream on server
        start_time = time.time()
        out_correlation_size = (num_timesteps * CORRELATION_NUM_TOP_SCORES *
                                CORRELATION_NUM_PIPES * loop_length +
                                num_bursts * 24) # for anything other than ISCA
                                                 # 48 should be instead of 24
        address_out_correlation = client.malloc_double(out_correlation_size)
        out_indices_size = (2 * num_timesteps * loop_length *
                            CORRELATION_NUM_TOP_SCORES * CORRELATION_NUM_PIPES)
        address_out_indices = client.malloc_int32_t(out_indices_size)
        current_time = time.time() - start_time
        print ('Allocating memory for output stream on server:\t%.5lfs' %
               current_time)

        # Initialize LMem
        start_time = time.time()
        client.correlation_loadLMem(
            num_bursts, address_loop_length, address_in_mem_load)
        current_time = time.time() - start_time
        print 'LMem initialization:\t\t\t\t%.5lfs' % current_time

        # Executing correlation action
        start_time = time.time()
        client.correlation(
            num_bursts,              # scalar input
            num_timesteps,           # scalar input
            num_timeseries,          # scalar input
            1,                       # scalar input
            float(window_size),      # scalar input
            address_precalculations, # streaming reordered input
            address_data_pairs,      # streaming reordered input
            address_out_correlation, # streaming unordered output
            address_out_indices)     # streaming unordered output
        current_time = time.time() - start_time
        print 'Correlation time:\t\t\t\t%.5lfs' % current_time

        # Get output stream from server
        start_time = time.time()
        out_correlation = client.receive_data_double(address_out_correlation,
                                                     out_correlation_size)
        out_correlation_time = time.time() - start_time
        print ('\tGet output stream Correlation:\t(size = %d bit)\t%.5lfs' %
               (64 * out_correlation_size, out_correlation_time))

        start_time = time.time()
        _ = client.receive_data_int32_t(address_out_indices, out_indices_size)
        out_indices_time = time.time() - start_time
        print ('\tGet output stream outIndices:\t(size = %d bit)\t%.5lfs' %
               (32 * out_indices_size, out_indices_time))

        start_time = time.time()
        loop_length_size = 1
        loop_length = client.receive_data_int32_t(address_loop_length,
                                                  loop_length_size)
        loop_length_time = time.time() - start_time
        print ('\tGet output stream loopLength:\t(size = %d bit)\t\t%.5lfs' %
               (32 * loop_length_size, loop_length_time))

        current_time = (out_indices_time + out_correlation_time +
                        loop_length_time)
        speed = ((32 * loop_length_size + 32 * out_indices_time +
                  64 * out_correlation_size) / (current_time * 1000000))
        print ('Getting output stream from server total time:\t%.5lfs' %
               current_time + '\t(average speed = %.5lfMb/s)' % (speed))

        # Free allocated memory for streams on server
        start_time = time.time()
        client.free(address_loop_length)
        client.free(address_in_mem_load)
        client.free(address_precalculations)
        client.free(address_data_pairs)
        client.free(address_out_correlation)
        client.free(address_out_indices)
        current_time = time.time() - start_time
        print ('Freeing allocated memory for streams on server:\t%.5lfs' %
               current_time)

        # Close!
        start_time = time.time()
        transport.close()
        current_time = time.time() - start_time
        print 'Closing connection:\t\t\t\t%.5lfs' % current_time

        # Store data
        start_time = time.time()

        position = 0
        index = 0
        start = ((num_timesteps-1) * loop_length[0] *
                 CORRELATION_NUM_TOP_SCORES * CORRELATION_NUM_PIPES)

        for i in range(num_timeseries):
            for j in range(i):
                correlations[index+j] = out_correlation[start+position+j]
            index += i
            position += ((i/12)+1)*12

        current_time = time.time() - start_time
        print 'Storing time:\t\t\t\t\t%.5lfs' % current_time

    except Thrift.TException, thrift_exception:
        print '%s' % (thrift_exception.message)
        sys.exit(-1)

def correlate_cpu(data, num_timeseries, size_timeseries,
                  correlations_cpu, indices_step):
    """Calculates correlations on CPU."""
    num_timesteps = size_timeseries
    window_size = size_timeseries

    sums = [0.0] * num_timeseries
    sums_sq = [0.0] * num_timeseries
    sums_xy = [0.0] * calc_num_correlations(num_timeseries)

    for k in range(num_timesteps):
        index_correlation = 0

        for i in range(num_timeseries):
            old = 0.0 if k < window_size else data[i][k - window_size]
            new = data[i][k]
            sums[i] += new - old
            sums_sq[i] += new * new - old * old

        for i in range(num_timeseries):
            old_x = 0.0 if k < window_size else data[i][k - window_size]
            new_x = data[i][k]

            for j in range(i):
                old_y = 0.0 if k < window_size else data[j][k - window_size]
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
    """Checks if correlations_dfe and correlations_cpu are the same."""
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

def test(size_timeseries, num_timeseries):
    """
    Calls correlate_dfe and correlate_cpu
    and checks if they return the same result.
    """
    data = random_data(num_timeseries, size_timeseries)
    correlations_dfe = [0.0] * calc_num_correlations(num_timeseries)
    correlations_cpu = [0.0] * calc_num_correlations(num_timeseries)
    indices_step = [0.0] * calc_num_correlations(num_timeseries) * 2

    start_time = time.time()
    correlate_dfe(data, size_timeseries, num_timeseries, correlations_dfe)
    dfe_total = time.time() - start_time
    print 'DFE correlation total time:\t%.5lfs' % dfe_total

    start_time = time.time()
    correlate_cpu(data, num_timeseries, size_timeseries,
                  correlations_cpu, indices_step)
    cpu_total = time.time() - start_time
    print 'CPU correlation total time:\t%.5lfs' % cpu_total

    check(correlations_dfe, correlations_cpu, num_timeseries, indices_step)

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print 'Usage: CorrelationClient.py <stream size> <number of streams>'
        sys.exit(-1)

    test(int(sys.argv[1]), int(sys.argv[2]))

