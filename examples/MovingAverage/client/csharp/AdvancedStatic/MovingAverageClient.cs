using System;
using System.Linq;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Diagnostics;

using Thrift;
using Thrift.Protocol;
using Thrift.Transport;

using com.maxeler.MovingAverage;

internal class MovingAverageClient {
        public static int check (List<double> dataIn, List<double> dataOut, int size) {
                int status = 0;
                
                for (var i = 1; i < dataOut.Count - 1; i++) {
                        float a = Convert.ToSingle(dataIn[i - 1]);
                        float b = Convert.ToSingle(dataIn[i]);
                        float c = Convert.ToSingle(dataIn[i + 1]);
                        float res = Convert.ToSingle(a+b+c)/3;
                        if (dataOut[i] != res) {
                                Console.WriteLine("Output data @ {0} = {1} (expected {2})", i, dataOut[i], res);
                                status++;
                        }
                }

                return status;
        }

        public static void Main(string[] args) {
                Stopwatch sw = new Stopwatch();
                Stopwatch swDFE = new Stopwatch();
                sw.Start();
                swDFE.Start();

                // Make socket
                var socket = new TSocket("localhost", 9090);

                // Buffering is critical. Raw sockets are very slow
                var transport = new TBufferedTransport(socket);

                // Wrap in a protocol
                var protocol = new TBinaryProtocol(transport);

                // Create a client to use the protocol encoder
                var client = new MovingAverageService.Client(protocol);

                sw.Stop();
                Console.WriteLine("Creating a client:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                try {
                        // Connect!
                        sw.Reset(); sw.Start();
                        transport.Open();
                        sw.Stop();
                        Console.WriteLine("Opening connection:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        sw.Reset(); sw.Start();
                        const int size = 384;
                        List<double> dataIn = new List<double>();
                        // Generate input data
                        Random random = new Random();
                        for(int i = 0; i<size; i++) {
                                dataIn.Add(random.Next(0, 100));
                        }
                        sw.Stop();
                        Console.WriteLine("Generating input data:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Initialize maxfile
                        sw.Reset(); sw.Start();
                        var maxfile = client.MovingAverage_init();
                        sw.Stop();
                        Console.WriteLine("Initializing maxfile:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Load DFE
                        sw.Reset(); sw.Start();
                        var engine = client.max_load(maxfile, "*");
                        sw.Stop();
                        Console.WriteLine("Loading DFE:\t\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Allocate and send input streams to server
                        sw.Reset(); sw.Start();
                        var address_dataIn = client.malloc_float(size);
                        client.send_data_float(address_dataIn, dataIn);
                        sw.Stop();
                        Console.WriteLine("Sending input data:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Allocate memory for output stream on server
                        sw.Reset(); sw.Start();
                        var address_dataOut = client.malloc_float(size);
                        sw.Stop();
                        Console.WriteLine("Allocating memory for output stream on server:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Action default
                        sw.Reset(); sw.Start();
                        var action = new MovingAverage_actions_t_struct();
                        action.Param_N = size;
                        action.Instream_x = address_dataIn;
                        action.Outstream_y = address_dataOut;
                        var address_action = client.send_MovingAverage_actions_t(action);
                        client.MovingAverage_run(engine, address_action);
                        sw.Stop();
                        Console.WriteLine("Moving average time:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Unload DFE
                        sw.Reset(); sw.Start();
                        client.max_unload (engine);
                        sw.Stop();
                        Console.WriteLine("Unloading DFE:\t\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Get output stream from server
                        sw.Reset(); sw.Start();
                        List<double> dataOut = client.receive_data_float(address_dataOut, size);
                        sw.Stop();
                        Console.WriteLine("Getting output stream:\t(size = {0} bit)\t{1}s", size * 32, sw.Elapsed.TotalMilliseconds / 1000);

                        // Free allocated memory for streams on server
                        sw.Reset(); sw.Start();
                        client.free(address_dataIn);
                        client.free(address_dataOut);
                        sw.Stop();
                        Console.WriteLine("Freeing allocated memory for streams on server:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Free allocated maxfile data
                        sw.Reset(); sw.Start();
                        client.MovingAverage_free();
                        sw.Stop();
                        Console.WriteLine("Freeing allocated maxfile data:\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Checking results
                        sw.Reset(); sw.Start();
                        int status = check(dataIn, dataOut, size);
                        sw.Stop();
                        Console.WriteLine("Checking results:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        sw.Reset(); sw.Start();
                        transport.Close();
                        sw.Stop();
                        Console.WriteLine("Closing connection:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);
                        
                        swDFE.Stop();
                        Console.WriteLine("DFE moving average total time:\t\t\t{0}s", swDFE.Elapsed.TotalMilliseconds / 1000);
                        
                        if (status==0) { 
                                Console.WriteLine("Test successful!");
                        } else {
                                Console.WriteLine("Test failed {0} times!", status);
                                Environment.Exit(-1);
                        }

                } catch (SocketException e) {
                        Console.WriteLine("Could not connect to the server: {0}.", e.Message);
                        Environment.Exit(-1);
                } catch (Exception e) {
                        Console.WriteLine("An error occured: {0}", e.Message);
                        Environment.Exit(-1);
                }
        }
}
