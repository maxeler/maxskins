#!/usr/bin/env python

import sys, glob
import random
sys.path.append('../gen-py')

from com.maxeler.LMemLoopback import LMemLoopbackService

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

def check(size, out_data, in_a, in_b):
    """Check if out_data = in_a + in_b"""
    status = 0
    for i in range(0, size):
        if out_data[i] != in_a[i] + in_b[i]:
            print (str(out_data[i]) + " != " + str(in_a[i]) +
                   " + " + str(in_b[i]))
            status = 1
    return status

def LMemLoopback_dfe(size, in_a, in_b):
    """LMemLoopback DFE implementation."""
    try:

        # Make socket
        transport = TSocket.TSocket('localhost', 9090)

        # _buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(transport)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = LMemLoopbackService.Client(protocol)

        # Connect!
        transport.open()

        size_bytes = size * 4

        # Initialize maxfile
        max_file = client.LMemLoopback_init()

        # Load DFE
        max_engine = client.max_load (max_file, '*')    

        # Allocate and send input streams to server
        address_in_a = client.malloc_int32_t(size)
        client.send_data_int32_t(address_in_a, in_a)

        address_in_b = client.malloc_int32_t(size)
        client.send_data_int32_t(address_in_b, in_b)

        # Allocate memory for output stream on server
        address_out_data = client.malloc_int32_t(size)

        print "Loading DFE memory."

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

        print "Running DFE."

        actions = client.max_actions_init(max_file, "default")
        client.max_set_param_uint64t(actions, "N", size)

        client.max_run(max_engine, actions)

        print "Reading DFE memory."

        actions = client.max_actions_init(max_file, "readLMem")
        client.max_set_param_uint64t(actions, "address", 2 * size_bytes)
        client.max_set_param_uint64t(actions, "nbytes", size_bytes)
        client.max_queue_output(actions, "lmem_to_cpu", address_out_data,
                                size_bytes)	

        client.max_run(max_engine, actions)
        client.free(actions)

        # Unload DFE
        client.max_unload(max_engine)

        # Get output stream from server
        out_data = client.receive_data_int32_t(address_out_data, size)

        # Free allocated memory for streams on server
        client.free(address_in_a)
        client.free(address_in_b)
        client.free(address_out_data)

        # Free allocated maxfile data
        client.LMemLoopback_free()

        # Close!
        transport.close()

    except Thrift.TException, thrift_exceptiion:
        print '%s' % (thrift_exceptiion.message)
        sys.exit(-1)

    return out_data

def test():
    '''
    Calls LMemLoopbackDFE and LMemLoopbackCPU
    and checks if they return the same result.
    '''
    # Input
    size = 384

    in_a = [i for i in range(size)]
    in_b = [size - i for i in range(size)]

    # DFE Output
    out_data = LMemLoopback_dfe(size, in_a, in_b)

    # Checking results
    if check(size, out_data, in_a, in_b):
        print "Test failed."
        sys.exit(-1)
    else:
        print "Test passed!"

if __name__ == '__main__':
    test()

