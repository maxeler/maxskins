#!/usr/bin/env python
"""LMemLoopback example"""

import sys
import time
sys.path.append('../gen-py')

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from com.maxeler.LMemLoopback import LMemLoopbackService

def check(data_out_dfe, data_out_cpu, size):
    """Check if data_out_dfe and data_out_cpu match."""
    status = 0
    for i in range(size):
        if data_out_dfe[i] != data_out_cpu[i]:
            print str(data_out_dfe[i]) + " != " + str(data_out_cpu[i])
            status = status + 1
    return status

def lmem_loopback_cpu(size, in_a, in_b):
    """LMemLoopback CPU implementation."""
    return [in_a[i] + in_b[i] for i in range(size)]

def lmem_loopback_dfe(size, in_a, in_b):
    """LMemLoopback DFE implementation."""
    try:
        start_time = time.time()
        # Make socket
        socket = TSocket.TSocket('localhost', 9090)

        # _buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(socket)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = LMemLoopbackService.Client(protocol)
        print ('Creating a client:\t\t\t\t%.5lfs' %
       	       (time.time() - start_time))

        # Connect!
        start_time = time.time()
        transport.open()
        print ('Opening connection:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        size_bytes = size * 4

        # Initialize maxfile
        start_time = time.time()
        max_file = client.LMemLoopback_init()
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
        address_out_data = client.malloc_int32_t(size)
        print ('Allocating memory for output stream on server:\t%.5lfs'%
               (time.time() - start_time))

        # Write to LMem
        start_time = time.time()

        actions = client.max_actions_init(max_file, "writeLMem")
        client.max_set_param_uint64t(actions, "address", 0)
        client.max_set_param_uint64t(actions, "nbytes", size_bytes)
        client.max_queue_input(actions, "cpu_to_lmem", address_in_a,
                               size_bytes)

        client.max_run(max_engine, actions)

        actions = client.max_actions_init(max_file, "writeLMem")
        client.max_set_param_uint64t(actions, "address", size_bytes)
        client.max_set_param_uint64t(actions, "nbytes", size_bytes)
        client.max_queue_input(actions, "cpu_to_lmem", address_in_b,
                               size_bytes)

        client.max_run(max_engine, actions)
        print ('Writing to LMem:\t\t\t\t%.5lfs'%
               (time.time() - start_time))

	# Action default
        start_time = time.time()

        actions = client.max_actions_init(max_file, "default")
        client.max_set_param_uint64t(actions, "N", size)

        client.max_run(max_engine, actions)
        print ('LMemLoopback time:\t\t\t\t%.5lfs'%
               (time.time() - start_time))

        # Reading from LMem
        start_time = time.time()

        actions = client.max_actions_init(max_file, "readLMem")
        client.max_set_param_uint64t(actions, "address", 2 * size_bytes)
        client.max_set_param_uint64t(actions, "nbytes", size_bytes)
        client.max_queue_output(actions, "lmem_to_cpu", address_out_data,
                                size_bytes)

        client.max_run(max_engine, actions)
        client.free(actions)
        print ('Reading from LMem:\t\t\t\t%.5lfs'%
               (time.time() - start_time))

        # Unload DFE
        start_time = time.time()
        client.max_unload(max_engine)
        print ('Unloading DFE:\t\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Get output stream from server
        start_time = time.time()
        out_data = client.receive_data_int32_t(address_out_data, size)
        print ('Getting output stream:\t(size = %d bit)\t%.5lfs' %
               ((size * 32), (time.time() - start_time)))

        # Free allocated memory for streams on server
        start_time = time.time()
        client.free(address_in_a)
        client.free(address_in_b)
        client.free(address_out_data)
        client.free(actions)
        print ('Freeing allocated memory for streams on server:\t%.5lfs' %
               (time.time() - start_time))

        # Free allocated maxfile data
        start_time = time.time()
        client.LMemLoopback_free()
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

    return out_data

def test():
    """
    Calls LMemLoopbackDFE and LMemLoopbackCPU
    and checks if they return the same result.
    """
    # Input
    start_time = time.time()
    size = 384

    in_a = [i for i in range(size)]
    in_b = [size - i for i in range(size)]
    print ('Generating input data:\t\t\t\t%.5lfs' %
           (time.time() - start_time))

    # DFE Output
    start_time = time.time()
    data_out_dfe = lmem_loopback_dfe(size, in_a, in_b)
    print ('DFE LMemLoopback total time:\t\t\t%.5lfs' %
           (time.time() - start_time))

    # CPU Output
    start_time = time.time()
    data_out_cpu = lmem_loopback_cpu(size, in_a, in_b)
    print ('CPU LMemLoopback total time:\t\t\t%.5lfs' %
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

