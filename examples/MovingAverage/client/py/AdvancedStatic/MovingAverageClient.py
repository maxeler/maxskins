#!/usr/bin/env python
"""MovingAverage example"""

import sys
import random
import time
import array
sys.path.append('../gen-py')

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from com.maxeler.MovingAverage import MovingAverageService
from com.maxeler.MovingAverage.ttypes import MovingAverage_actions_t_struct

def check(data_in, data_out, size):
    """Check if data_out is as expected."""
    data_in_float = array.array('f', data_in)
    for i in range(1, size - 1):
        result = array.array('f', [0.0])
        result[0] = (data_in_float[i - 1]  + data_in_float[i] +
                     data_in_float[i + 1]) / 3
        if data_out[i] != result[0]:
            print "Test failed!"
            sys.exit(-1)

    print "Test passed!"

def moving_average_dfe(size, data_in):
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
        client = MovingAverageService.Client(protocol)

        print ('Creating a client:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Connect!
        start_time = time.time()
        transport.open()
        print ('Opening connection:\t\t\t\t%.5lfs' %
               (time.time() - start_time))

        # Initialize maxfile
        start_time = time.time()
        max_file = client.MovingAverage_init()
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
        actions = MovingAverage_actions_t_struct(size, address_data_in,
                                                 address_data_out)
        address_actions = client.send_MovingAverage_actions_t(actions)
        client.MovingAverage_run(max_engine, address_actions)
        print ('Moving average time:\t\t\t\t%.5lfs' %
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
        client.MovingAverage_free()
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
    Calls moving_average_dfe and
    checks if it return the correct result.
    """
    # Scalar inputs
    size = 384

    # Generate random data
    data_in = array.array('f', [float(int(random.uniform(0.0, 1000.0)))
                                for _ in range(size)])

    # DFE Output
    start_dfe_time = time.time()
    data_out = moving_average_dfe(size, data_in)
    print ('DFE moving average total time:\t\t\t%.5lfs' %
           (time.time() - start_dfe_time))

    # Checking results
    check(data_in, data_out, size)

if __name__ == '__main__':
    test()

