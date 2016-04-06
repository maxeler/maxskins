using System;
using System.Linq;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Threading;

using Thrift;
using Thrift.Protocol;
using Thrift.Transport;

using com.maxeler.SignExt;

internal class SignExt {
    public static void Main(string[] args) {
        try {
            var transport = new TSocket("localhost", 9090);
            var protocol = new TBinaryProtocol(transport);
            var client = new SignExtService.Client(protocol);
            
            // Connect!
            transport.Open();

            if(args.Length != 2) {
                Console.WriteLine("Usage: $0 dfe_ip remote_ip");
                Environment.Exit(-1);
            }

            var dfe_ip_address = client.malloc_int64_t(5);
            client.inet_aton(args[0], dfe_ip_address);
            
            var remote_ip_address = client.malloc_int64_t(5);
            client.inet_aton(args[1], remote_ip_address);

            var netmask_address = client.malloc_int64_t(5);
            client.inet_aton("255.255.255.0", netmask_address);
            
            short port = 2000;

            // Initialize maxfile
            var maxfile = client.SignExt_init();

            // Load DFE
            var engine = client.max_load(maxfile, "*");

            max_config_key_bool_t_struct enumkey = new max_config_key_bool_t_struct();
            enumkey.Type = max_config_key_bool_t_enum.MAX_CONFIG_PRINTF_TO_STDOUT;
            client.max_config_set_bool(enumkey, 1);

            var actions = client.max_actions_init(maxfile, "default");

            client.max_run(engine, actions);
            client.max_actions_free(actions);


            var buffer_address = client.malloc_int64_t(1);
            int bufferSize = 4096 * 512;
            client.posix_memalign(buffer_address, 4096, bufferSize);

            long buffer = client.receive_data_int64_t(buffer_address, 1)[0];

            var toCpu = client.max_framed_stream_setup(engine, "toCPU",
                                                        buffer, bufferSize, -1);

            max_net_connection_t_struct enumconn = new max_net_connection_t_struct();
            enumconn.Type = max_net_connection_t_enum.MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1;
            client.max_ip_config(engine, enumconn, dfe_ip_address, netmask_address);
            var dfe_socket = client.max_udp_create_socket(engine, "udpTopPort1");
            client.max_udp_bind(dfe_socket, port);
            client.max_udp_connect(dfe_socket, remote_ip_address, (short) 0);

            Console.WriteLine("Listening on {0} port {1}", args[0], port);
            
            Console.WriteLine("Waiting for kernel response...");

            var f_address = client.malloc_int64_t(1);
            var fsz_address = client.malloc_int64_t(1);
            int numMessageRx = 0;
            bool cond = true;
            
            while(cond) {
                if (client.max_framed_stream_read(toCpu, 1, f_address, fsz_address) == 1) {
                    numMessageRx += 1;

                    long fsz = client.receive_data_int64_t(fsz_address, 1)[0];
                    Console.WriteLine("CPU: Got output frame {0} - size {1} bytes", numMessageRx, fsz);

                    long f = client.receive_data_int64_t(f_address, 1)[0];

                    List<long> w = client.receive_data_int64_t(f, 3);
 
                    for (int i = 0; i < 3; i++) {
                        long wp;
                        if(w[i] < 0) {
                            wp = w[i] + (long)Math.Pow(2, 32) * (long)Math.Pow(2, 32);
                        } else {
                            wp = w[i];
                        }
                        Console.WriteLine("Frame [{0}] Word[{1}]: 0x{2:X}", numMessageRx, i, wp);
                    }
                    
                    client.max_framed_stream_discard(toCpu, 1);
                    
                    if (w[0] == 0 && w[1] == 0 && w[2] == 0) {
                        cond = false;
                    }

                } else {
                    Thread.Sleep(1/100000);
                }
            }
            
            client.max_udp_close(dfe_socket);
            client.max_framed_stream_release(toCpu);
            client.max_unload(engine);
            client.max_file_free(maxfile);
            client.free(dfe_ip_address);
            client.free(remote_ip_address);
            client.free(netmask_address);
            client.free(buffer_address);
            client.free(f_address);
            client.free(fsz_address);
            client.SignExt_free();

            Console.WriteLine("Done.");

            // Close!
            transport.Close();
            Environment.Exit(0);

        } catch (SocketException e) {
            Console.WriteLine("Could not connect to the server: {0}.", e.Message);
            Environment.Exit(-1);
        } catch (Exception e) {
            Console.WriteLine("An error occured: {0}", e.Message);
            Environment.Exit(-1);
        }
    }
}
