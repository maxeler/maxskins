#!/usr/bin/env python

import sys
import random
import array
sys.path.append('../gen-py')

from com.maxeler.MovingAverage import MovingAverageService
from com.maxeler.MovingAverage.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

def check(dataIn, dataOut):
    dataInFloat = array.array('f', dataIn)
    for i in range(1, size - 1):
        result = array.array('f', [0.0])
        result[0] = (dataInFloat[i - 1]  + dataInFloat[i] + dataInFloat[i + 1]) / 3
        if (dataOut[i] != result[0]):
            print "Test failed! %.12lf != %.12lf    %.12lf  %d" % (dataOut[i], result[0], ((dataInFloat[i - 1] +dataInFloat[i] + dataInFloat[i + 1]) / 3), i)
            sys.exit(-1)

    print "Test passed!"

try:

    # Make socket
    transport = TSocket.TSocket('localhost', 9090)

    # Buffering is critical. Raw sockets are very slow
    transport = TTransport.TBufferedTransport(transport)

    # Wrap in a protocol
    protocol = TBinaryProtocol.TBinaryProtocol(transport)

    # Create a client to use the protocol encoder
    client = MovingAverageService.Client(protocol)

    # Connect!
    transport.open()

    # Scalar inputs 
    size = 384

    # Generate two random vectors
    dataIn = array.array('f',[float(int(random.uniform(0.0, 1000.0))) for _ in range(size)])

    # Allocate and send input streams to server
    address_dataIn = client.malloc_float(size)
    client.send_data_float(address_dataIn, dataIn)

    # Allocate memory for output stream on server
    address_dataOut = client.malloc_float(size)

    print "Running DFE.";
    client.MovingAverage(size, address_dataIn, address_dataOut)

    # Get output stream from server
    dataOut = client.receive_data_float(address_dataOut, size)

    # Free allocated memory for streams on server
    client.free(address_dataIn)
    client.free(address_dataOut)

    # Close!
    transport.close()

    # Checking results
    check(dataIn, dataOut)

except Thrift.TException, tx:
    print '%s' % (tx.message)
