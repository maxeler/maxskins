#!/usr/bin/env python
"""PassThrough example data_out[n] = data_in[n]"""

import sys
sys.path.append('../gen-py')

from com.maxeler.PassThrough import PassThroughService

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

def pass_through_cpu(size, data_in):
    """PassThrough CPU implementation."""
    return [data_in[i] for i in range(size)]

def pass_through_dfe(size, data_in):
    """PassThrough DFE implementation."""
    try:
        # Make socket
        transport = TSocket.TSocket('localhost', 9090)

        # Buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(transport)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = PassThroughService.Client(protocol)

        # Connect!
        transport.open()

        size_bytes = 4 * size

        # Initialize maxfile
        max_file = client.PassThrough_init()

        # Load DFE
        max_engine = client.max_load(max_file, '*')

        # Allocate and send input streams to server
        address_data_in = client.malloc_float(size)
        client.send_data_float(address_data_in, data_in)

        # Allocate memory for output stream on server
        address_data_out = client.malloc_float(size)

        print "Running DFE."

        actions = client.max_actions_init(max_file, "default")

        client.max_set_param_uint64t(actions, "N", size)
        client.max_queue_input(actions, "x", address_data_in, size_bytes)
        client.max_queue_output(actions, "y", address_data_out, size_bytes)

        client.max_run(max_engine, actions)

        # Unload DFE
        client.max_unload(max_engine)

        # Get output stream from server
        data_out = client.receive_data_float(address_data_out, size)

        # Free allocated memory for streams on server
        client.free(address_data_in)
        client.free(address_data_out)

        # Free allocated maxfile data
        client.PassThrough_free()

        # Close!
        transport.close()

    except Thrift.TException, thrift_exceptiion:
        print '%s' % (thrift_exceptiion.message)
        sys.exit(-1)

    return data_out

def test():
    '''
    Calls PassThroughDFE and PassThroughCPU
    and checks if they return the same result.
    '''
    # Input
    size = 384
    data_in = [i + 1 for i in range(size)]

    # DFE Output
    data_out_dfe = pass_through_dfe(size, data_in)

    # CPU Output
    data_out_cpu = pass_through_cpu(size, data_in)

    # Checking results
    if check(data_out_dfe, data_out_cpu, size):
        print "Test failed."
        sys.exit(-1)
    else:
        print "Test passed!"

if __name__ == '__main__':
    test()

