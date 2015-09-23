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

        # Allocate and send input streams to server
        address_in_a = client.malloc_int32_t(size)
        client.send_data_int32_t(address_in_a, in_a)

        address_in_b = client.malloc_int32_t(size)
        client.send_data_int32_t(address_in_b, in_b)

        # Allocate memory for output stream on server
        address_out_data = client.malloc_int32_t(size)

        print "Loading DFE memory."
        client.LMemLoopback_writeLMem(0, size_bytes, address_in_a)
        client.LMemLoopback_writeLMem(size_bytes, size_bytes, address_in_b)

        print "Running DFE."
        client.LMemLoopback(size)

        print "Reading DFE memory."
        client.LMemLoopback_readLMem(2 * size_bytes, size_bytes,
                                     address_out_data)

        # Get output stream from server
        out_data = client.receive_data_int32_t(address_out_data, size)

        # Free allocated memory for streams on server
        client.free(address_in_a)
        client.free(address_in_b)
        client.free(address_out_data)

        # Close!
        transport.close()

    except Thrift.TException, thrift_exceptiion:
        print '%s' % (thrift_exceptiion.message)

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
    else:
        print "Test passed!"

if __name__ == '__main__':
    test()

