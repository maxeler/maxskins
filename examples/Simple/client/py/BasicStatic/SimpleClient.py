#!/usr/bin/env python
"""Simple example data_out[n] = data_in[n] * data_in[n] + data_in[n]"""

import sys
sys.path.append('../gen-py')

from com.maxeler.Simple import SimpleService

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

def check(data_out_dfe, data_out_cpu, size):
    """Check if data_out is same as expected."""
    status = 0
    for i in range(size):
        if data_out_dfe[i] != data_out_cpu[i]:
            print str(data_out_dfe[i]) + " != " + str(data_out_cpu[i])
            status = 1
    return status

def simple_cpu(size, data_in):
    """Simple CPU implementation."""
    return [data_in[i] * data_in[i] + data_in[i] for i in range(size)]

def simple_dfe(size, data_in):
    """Simple DFE implementation."""
    data_out = []
    
    try:
        # Make socket
        transport = TSocket.TSocket('localhost', 9090)

        # Buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(transport)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = SimpleService.Client(protocol)

        # Connect!
        transport.open()

        # Initialize maxfile
        max_file = client.Simple_init()

        # Allocate and send input streams to server
        address_data_in = client.malloc_float(size)
        client.send_data_float(address_data_in, data_in)

        # Allocate memory for output stream on server
        address_data_out = client.malloc_float(size)

        print "Running DFE."
        client.Simple(size, address_data_in, address_data_out)

        # Get output stream from server
        data_out = client.receive_data_float(address_data_out, size)

        # Free allocated memory for streams on server
        client.free(address_data_in)
        client.free(address_data_out)

        # Free allocated maxfile data
        client.Simple_free()

        # Close!
        transport.close()

    except Thrift.TException, thrift_exceptiion:
        print '%s' % (thrift_exceptiion.message)
        sys.exit(-1)

    return data_out

def test():
    '''
    Calls SimpleDFE and SimpleCPU
    and checks if they return the same result.
    '''
    # Input
    size = 384
    data_in = [i + 1 for i in range(size)]

    # DFE Output
    data_out_dfe = simple_dfe(size, data_in)

    # CPU Output
    data_out_cpu = simple_cpu(size, data_in)

    # Checking results
    if check(data_out_dfe, data_out_cpu, size):
        print "Test failed."
        sys.exit(-1)
    else:
        print "Test passed!"

if __name__ == '__main__':
    test()

