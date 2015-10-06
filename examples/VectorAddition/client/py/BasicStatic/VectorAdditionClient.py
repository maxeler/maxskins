#!/usr/bin/env python
"""VectorAddition example c[i] = a[i] + b[i] + scalar"""

import sys
import random
sys.path.append('../gen-py')

from com.maxeler.VectorAddition import VectorAdditionService
from com.maxeler.VectorAddition.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

def check(a, b, c, scalar, size):
    for i in range(size):
        if (c[i]!= a[i] + b[i] + scalar):
            print 'Test failed!'
            sys.exit(-1)

def vector_addition_dfe(size, scalar, a, b):
    """VectorAddition DFE implementation."""
    try:
        # Make socket
        transport = TSocket.TSocket('localhost', 9090)

        # Buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(transport)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = VectorAdditionService.Client(protocol)

        # Connect!
        transport.open()

        # Allocate and send input streams to server
        address_a = client.malloc_int32_t(size)
        client.send_data_int32_t(address_a, a)

        address_b = client.malloc_int32_t(size)
        client.send_data_int32_t(address_b, b)

        # Allocate memory for output stream on server
        address_c = client.malloc_int32_t(size)

        # Write vector a to LMem
        print 'Writing to LMem.'
        client.VectorAddition_writeLMem(0,size*4, address_a)

        # Add two vectors and a scalar
        print 'Running on DFE.'
        client.VectorAddition(scalar, size, address_b, address_c)

        # Get output stream from server
        c = client.receive_data_int32_t(address_c, size)

        # Free allocated memory for streams on server
        client.free(address_a)
        client.free(address_b)
        client.free(address_c)

        # Close!
        transport.close()

    except Thrift.TException, thrift_exceptiion:
        print '%s' % (thrift_exceptiion.message)
        sys.exit(-1)

    return c

def test():
    '''
    Calls VectorAdditionDFE and VectorAdditionCPU
    and checks if they return the same result.
    '''
    # Input
    size = 384
    scalar = 3

    a = random.sample(xrange(1000), size)
    b = random.sample(xrange(1000), size)

    # DFE Output
    c = vector_addition_dfe(size, scalar, a, b)

    # Checking results
    if check(a, b, c, scalar, size):
        print "Test failed."
    else:
        print "Test passed!"

if __name__ == '__main__':
    test()

