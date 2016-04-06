#!/usr/bin/env python
"""Sign Extension Example"""

import sys
import time
sys.path.append('../gen-py')

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from com.maxeler.SignExt import SignExtService
from com.maxeler.SignExt.ttypes import max_config_key_bool_t_struct

def signext_dfe():
    """Sign Extension DFE implementation."""
    try:
        # Make socket
        socket = TSocket.TSocket('localhost', 9090)

        # Buffering is critical. Raw sockets are very slow
        transport = TTransport.TBufferedTransport(socket)

        # Wrap in a protocol
        protocol = TBinaryProtocol.TBinaryProtocol(transport)

        # Create a client to use the protocol encoder
        client = SignExtService.Client(protocol)

         # Connect!
        transport.open()

        if len(sys.argv) != 3:
            print 'Usage: signext.py dfe_ip remote_ip'
            sys.exit(1)

        dfe_ip_address = client.malloc_int64_t(5)
        client.inet_aton(sys.argv[1], dfe_ip_address)

        remote_ip_address = client.malloc_int64_t(5)
        client.inet_aton(sys.argv[2], remote_ip_address)

        netmask_address = client.malloc_int64_t(5)
        client.inet_aton('255.255.255.0', netmask_address)

        port = 2000

        maxfile = client.SignExt_init()
        engine = client.max_load(maxfile, '*')

        enum = max_config_key_bool_t_struct(SignExtService.
                                            max_config_key_bool_t_enum.
                                            MAX_CONFIG_PRINTF_TO_STDOUT)
        client.max_config_set_bool(enum, True)

        actions = client.max_actions_init(maxfile, 'default')

        client.max_run(engine, actions)
        client.max_actions_free(actions)


        my_buffer_address = client.malloc_int64_t(1)
        my_buffer_size = 4096 * 512
        client.posix_memalign(my_buffer_address, 4096, my_buffer_size)

        my_buffer = client.receive_data_int64_t(my_buffer_address, 1)[0]

        to_cpu = client.max_framed_stream_setup(engine, 'toCPU',
                                                my_buffer, my_buffer_size, -1)

        enum = max_config_key_bool_t_struct(
            SignExtService.max_net_connection_t_enum.
            MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1)
        client.max_ip_config(engine, enum, dfe_ip_address, netmask_address)
        dfe_socket = client.max_udp_create_socket(engine, 'udpTopPort1')
        client.max_udp_bind(dfe_socket, port)
        client.max_udp_connect(dfe_socket, remote_ip_address, 0)

        print 'Listening on %s port %d' % (sys.argv[1], port)

        print 'Waiting for kernel response...'
        sys.stdout.flush()

        frame_address = client.malloc_int64_t(1)
        fsz_address = client.malloc_int64_t(1)
        num_message_rx = 0
        cond = True
        while cond:
            if client.max_framed_stream_read(to_cpu, 1,
                                             frame_address, fsz_address) == 1:
                num_message_rx += 1

                fsz = client.receive_data_int64_t(fsz_address, 1)[0]
                print ('CPU: Got output frame %d - size %d bytes'
                       % (num_message_rx, fsz))

                frame = client.receive_data_int64_t(frame_address, 1)[0]

                word = client.receive_data_int64_t(frame, 3)
                for i in range(3):
                    print ('Frame [%d] Word[%d]: 0x%lx'
                           % (num_message_rx, i,
                              (word[i] + 2**64) if word[i] < 0 else word[i]))

                client.max_framed_stream_discard(to_cpu, 1)

                if word[0] == 0 & word[1] == 0 & word[2] == 0:
                    cond = False

            else:
                time.sleep(1/100000.0)

        client.max_udp_close(dfe_socket)
        client.max_framed_stream_release(to_cpu)
        client.max_unload(engine)
        client.max_file_free(maxfile)
        client.free(dfe_ip_address)
        client.free(remote_ip_address)
        client.free(netmask_address)
        client.free(my_buffer_address)
        client.free(frame_address)
        client.free(fsz_address)
        client.SignExt_free()

        print 'Done.'
        sys.stdout.flush()

        # Close!
        transport.close()

    except Thrift.TException, thrift_exception:
        print '%s' % (thrift_exception.message)

if __name__ == '__main__':
    signext_dfe()

