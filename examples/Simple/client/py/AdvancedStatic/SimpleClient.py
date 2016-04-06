#!/usr/bin/env python
"""Simple example data_out[n] = data_in[n] * data_in[n] + data_in[n]"""

import sys
import time
sys.path.append('../gen-py')

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from com.maxeler.Simple import SimpleService
from com.maxeler.Simple.ttypes import Simple_actions_t_struct

def check(data_out_dfe, data_out_cpu, size):
    """Check if data_out is as expected."""
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
    try:
        start_time = time.time()

        # Make socket
        socket = TSocket.TSocket('localhost', 9090)

        # Buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(socket)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = SimpleService.Client(protocol)
        print ('Creating a client:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Connect!
        start_time = time.time()
        transport.open()
        print ('Opening connection:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Initialize maxfile
        start_time = time.time()
        max_file = client.Simple_init()
        print ('Initializing maxfile:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Load DFE
        start_time = time.time()
        max_engine = client.max_load(max_file, '*')
        print ('Loading DFE:\t\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Allocate and send input streams to server
        start_time = time.time()
        address_data_in = client.malloc_float(size)
        client.send_data_float(address_data_in, data_in)
        print ('Sending input data:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Allocate memory for output stream on server
        start_time = time.time()
        address_data_out = client.malloc_float(size)
        print ('Allocating memory for output stream on server:\t%.5lfs'%
               (time.time() - start_time))

	# Action default
        start_time = time.time()
        actions = Simple_actions_t_struct(size, address_data_in,
                                          address_data_out)
        address_actions = client.send_Simple_actions_t(actions)
        client.Simple_run(max_engine, address_actions)
        print ('Simple time:\t\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Unload DFE
        start_time = time.time()
        client.max_unload(max_engine)
        print ('Unloading DFE:\t\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Get output stream from server
        start_time = time.time()
        data_out = client.receive_data_float(address_data_out, size)
        print ('Getting output stream:\t(size = %d bit)\t%.5lfs' %
               ((size * 32), (time.time() - start_time)))

        # Free allocated memory for streams on server
        start_time = time.time()
        client.free(address_data_in)
        client.free(address_data_out)
        client.free(address_actions)
        print ('Freeing allocated memory for streams on server:\t%.5lfs' %
               (time.time() - start_time))

        # Free allocated maxfile data
        start_time = time.time()
        client.Simple_free()
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
    Calls SimpleDFE and SimpleCPU
    and checks if they return the same result.
    """
    # Input
    start_time = time.time()
    size = 384
    data_in = [i + 1 for i in range(size)]
    print ('Generating input data:\t\t\t\t%.5lfs' %
           (time.time() - start_time))

    # DFE Output
    start_time = time.time()
    data_out_dfe = simple_dfe(size, data_in)
    print ('DFE Simple total time:\t\t\t\t%.5lfs' %
           (time.time() - start_time))

    # CPU Output
    start_time = time.time()
    data_out_cpu = simple_cpu(size, data_in)
    print ('CPU Simple total time:\t\t\t\t%.5lfs' %
           (time.time() - start_time))

    # Checking results
    start_time = time.time()
    status = check(data_out_dfe, data_out_cpu, size)

    if status == 0:
        print "Test passed!"
    else:
        print "Test failed %.5d times!" % (status)
        sys.exit(-1)

if __name__ == '__main__':
    test()

