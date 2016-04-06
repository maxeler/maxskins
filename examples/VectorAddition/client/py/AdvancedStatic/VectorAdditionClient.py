#!/usr/bin/env python
"""VectorAddition example data_out[i] = in_a[i] + in_b[i] + scalar"""

import sys
import time
import random
sys.path.append('../gen-py')

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from com.maxeler.VectorAddition import VectorAdditionService
from com.maxeler.VectorAddition.ttypes import (
    VectorAddition_writeLMem_actions_t_struct,
    VectorAddition_actions_t_struct)

def check(data_out_dfe, data_out_cpu, size):
    """Check if data_out is as expected."""
    status = 0
    for i in range(size):
        if data_out_dfe[i] != data_out_cpu[i]:
            print str(data_out_dfe[i]) + " != " + str(data_out_cpu[i])
            status = status + 1
    return status

def vector_addition_cpu(size, scalar, in_a, in_b):
    """VectorAddition CPU implementation."""
    return [in_a[i] + in_b[i] + scalar for i in range(size)]

def vector_addition_dfe(size, scalar, in_a, in_b):
    """VectorAddition DFE implementation."""
    try:
        start_time = time.time()
        # Make socket
        socket = TSocket.TSocket('localhost', 9090)

        # Buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(socket)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = VectorAdditionService.Client(protocol)
        print ('Creating a client:\t\t\t\t%.5lfs' %
       	       (time.time() - start_time))

        # Connect!
        start_time = time.time()
        transport.open()
        print ('Opening connection:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Initialize maxfile
        start_time = time.time()
        max_file = client.VectorAddition_init()
        print ('Initializing maxfile:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Load DFE
        start_time = time.time()
        max_engine = client.max_load(max_file, '*')
        print ('Loading DFE:\t\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Allocate and send input streams to server
        start_time = time.time()
        address_in_a = client.malloc_int32_t(size)
        client.send_data_int32_t(address_in_a, in_a)

        address_in_b = client.malloc_int32_t(size)
        client.send_data_int32_t(address_in_b, in_b)
        print ('Sending input data:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Allocate memory for output stream on server
        start_time = time.time()
        address_data_out = client.malloc_int32_t(size)
        print ('Allocating memory for output stream on server:\t%.5lfs'%
               (time.time() - start_time))

        # Write vector a to LMem
        start_time = time.time()
        actions_lmem = VectorAddition_writeLMem_actions_t_struct(0, size * 4,
                                                                 address_in_a)
        address_actions_lmem = (
            client.send_VectorAddition_writeLMem_actions_t(actions_lmem))
        client.VectorAddition_writeLMem_run_nonblock(max_engine,
                                                     address_actions_lmem)
        print ('Writing to LMem:\t\t\t\t%.5lfs'%
               (time.time() - start_time))

        # Action default
        start_time = time.time()
        actions = VectorAddition_actions_t_struct(scalar, size, address_in_b,
                                                  address_data_out)
        address_actions = client.send_VectorAddition_actions_t(actions)
        client.VectorAddition_run_nonblock(max_engine, address_actions)
        print ('Vector addition time:\t\t\t\t%.5lfs'%
               (time.time() - start_time))

        # Unload DFE
        start_time = time.time()
        client.max_unload(max_engine)
        print ('Unloading DFE:\t\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Get output stream from server
        start_time = time.time()
        data_out = client.receive_data_int32_t(address_data_out, size)
        print ('Getting output stream:\t(size = %d bit)\t%.5lfs' %
               ((size * 32), (time.time() - start_time)))

        # Free allocated memory for streams on server
        start_time = time.time()
        client.free(address_in_a)
        client.free(address_in_b)
        client.free(address_data_out)
        client.free(address_actions)
        client.free(address_actions_lmem)
        print ('Freeing allocated memory for streams on server:\t%.5lfs' %
               (time.time() - start_time))

        # Free allocated maxfile data
        start_time = time.time()
        client.VectorAddition_free()
        print ('Freeing allocated maxfile data:\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Close!
        start_time = time.time()
        transport.close()
        print ('Closing connection:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

    except Thrift.TException, thrift_exceptiion:
        print '%s' % (thrift_exceptiion.message)
        sys.exit(-1)

    return data_out

def test():
    """
    Calls VectorAdditionDFE and VectorAdditionCPU
    and checks if they return the same result.
    """
    # Input
    start_time = time.time()
    size = 384
    scalar = 3

    in_a = random.sample(xrange(1000), size)
    in_b = random.sample(xrange(1000), size)
    print ('Generating input data:\t\t\t\t%.5lfs' %
           (time.time() - start_time))

    # DFE Output
    start_time = time.time()
    data_out_dfe = vector_addition_dfe(size, scalar, in_a, in_b)
    print ('DFE Vector addition total time:\t\t\t%.5lfs' %
           (time.time() - start_time))

    # CPU Output
    start_time = time.time()
    data_out_cpu = vector_addition_cpu(size, scalar, in_a, in_b)
    print ('CPU Vector addition total time:\t\t\t%.5lfs' %
           (time.time() - start_time))

    # Checking results
    start_time = time.time()
    status = check(data_out_dfe, data_out_cpu, size)
    print ('Checking results:\t\t\t\t%.5lfs' %
           (time.time() - start_time))

    if status == 0:
        print "Test passed!"
    else:
        print "Test failed %.5d times!" % (status)
        sys.exit(-1)

if __name__ == '__main__':
    test()

