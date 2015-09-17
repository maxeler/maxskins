#!/usr/bin/env python

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

    print 'Test passed!'

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

    # Scalar inputs 
    size = 384
    scalar = 3

    # Generate two random vectors
    a = random.sample(xrange(1000), size)
    b = random.sample(xrange(1000), size)

    # Allocate and send input streams to server
    address_a = client.malloc_int32_t(size)
    client.send_data_int32_t(address_a, a)

    address_b = client.malloc_int32_t(size)
    client.send_data_int32_t(address_b, b)

    # Allocate memory for output stream on server
    address_c = client.malloc_int32_t(size)

    # Initialize maxfile
    max_file = client.VectorAddition_init()
    
    # Load DFE
    max_engine = client.max_load (max_file, '*')

    # Write vector a to LMem
    print 'Writing to LMem.'
    actions_lmem = VectorAddition_writeLMem_actions_t_struct(0, size*4, address_a)
    address_actions_lmem = client.send_VectorAddition_writeLMem_actions_t(actions_lmem)
    client.VectorAddition_writeLMem_run_nonblock(max_engine, address_actions_lmem)

    # Add two vectors and a scalar
    print 'Running on DFE.'
    actions = VectorAddition_actions_t_struct(scalar, size, address_b, address_c)
    address_actions = client.send_VectorAddition_actions_t(actions)
    client.VectorAddition_run_nonblock(max_engine, address_actions)

    # Unload DFE
    client.max_unload(max_engine)

    # Get output stream from server
    c = client.receive_data_int32_t(address_c, size)

    # Free allocated memory for streams on server
    client.free(address_actions)
    client.free(address_actions_lmem)
    client.free(address_a)
    client.free(address_b)
    client.free(address_c)

    # Free allocated maxfile data
    client.VectorAddition_free()

    # Close!
    transport.close()

    # Checking results
    check(a, b, c, scalar, size)

except Thrift.TException, tx:
    print '%s' % (tx.message)
