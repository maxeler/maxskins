// <copyright file="SimpleClient.cs" company="Maxeler">
// Copyright Maxeler. All rights reserved.
// </copyright>
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Sockets;

using com.maxeler.Simple;

using Thrift;
using Thrift.Protocol;
using Thrift.Transport;

/// <summary> 
/// Simple Dynamic example
/// </summary>
internal class SimpleClient 
{
    /// <summary> Checks if simpleDfe and simpleCpu return the same value 
    /// </summary>
    /// <param name = "dataOutDFE" > Data output from DFE </param>
    /// <param name = "dataOutCPU" > Data output from CPU </param>
    /// <param name = "size" > Size of array </param>
    /// <returns> Number of elements that doesn't match </returns>
    public static int Check(List<double> dataOutDFE, List<double> dataOutCPU, int size) 
    {
        int status = 0;

        for (int i = 0; i < size; i++) 
        {
            if (dataOutDFE[i] != dataOutCPU[i]) 
            {
                Console.WriteLine("Output data @ {0} = {1} (expected {2})", i, dataOutDFE[i], dataOutCPU[i]);
                status = 1;
            }
        }

        return status;  
    }

    /// <summary> Simple on CPU </summary>
    /// <param name = "size"> Size of array </param>
    /// <param name = "dataIn"> Data input </param>
    /// <returns> Data output </returns>
    public static List<double> SimpleCPU(int size, List<double> dataIn) 
    {
        List<double> dataOut = new List<double>();

        for (int i = 0; i < size; i++) 
        {
            dataOut.Add((dataIn[i] * dataIn[i]) + dataIn[i]);
        }

        return dataOut;
    }

    /// <summary> Simple on DFE </summary>
    /// <param name = "size"> Size of array </param>
    /// <param name = "dataIn"> Data input </param>
    /// <returns> Data output </returns>
    public static List<double> SimpleDFE(int size, List<double> dataIn)
    {
        Stopwatch sw = new Stopwatch();
        sw.Start();
        
        // Make socket
        var transport = new TSocket("localhost", 9090);

        // Wrap in a protocol
        var protocol = new TBinaryProtocol(transport);

        // Create a client to use the protocol encoder
        var client = new SimpleService.Client(protocol);

        sw.Stop();
        Console.WriteLine("Creating a client:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

        List<double> dataOut = new List<double>();
        int sizeBytes = size * 4;

        try
        {
            // Connect!
            sw.Reset();
            sw.Start();
            transport.Open();
            sw.Stop();
            Console.WriteLine("Opening connection:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Initialize maxfile
            sw.Reset();
            sw.Start();
            var maxfile = client.Simple_init();
            sw.Stop();
            Console.WriteLine("Initializing maxfile:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Load DFE
            sw.Reset();
            sw.Start();
            var engine = client.max_load(maxfile, "*");
            sw.Stop();
            Console.WriteLine("Loading DFE:\t\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Allocate and send input streams to server
            sw.Reset();
            sw.Start();
            var address_dataIn = client.malloc_float(size);
            client.send_data_float(address_dataIn, dataIn);
            sw.Stop();
            Console.WriteLine("Sending input data:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Allocate memory for output stream on server
            sw.Reset();
            sw.Start();
            var address_dataOut = client.malloc_float(size);
            sw.Stop();
            Console.WriteLine("Allocating memory for output stream on server:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Action default
            sw.Reset();
            sw.Start();
            var actions = client.max_actions_init(maxfile, "default");
            client.max_set_param_uint64t(actions, "N", size);
            client.max_queue_input(actions, "x", address_dataIn, sizeBytes);
            client.max_queue_output(actions, "y", address_dataOut, sizeBytes);

            client.max_run(engine, actions);
            sw.Stop();
            Console.WriteLine("Simple time:\t\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Unload DFE
            sw.Reset();
            sw.Start();
            client.max_unload(engine);
            sw.Stop();
            Console.WriteLine("Unloading DFE:\t\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Get output stream from server
            sw.Reset();
            sw.Start();
            dataOut = client.receive_data_float(address_dataOut, size);
            sw.Stop();
            Console.WriteLine("Getting output stream:\t(size = {0} bit)\t{1}s", size * 32, sw.Elapsed.TotalMilliseconds / 1000);

            // Free allocated memory for streams on server
            sw.Reset();
            sw.Start();
            client.free(address_dataIn);
            client.free(address_dataOut);
            sw.Stop();
            Console.WriteLine("Freeing allocated memory for streams on server:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Free allocated maxfile data
            sw.Reset();
            sw.Start();
            client.Simple_free();
            sw.Stop();
            Console.WriteLine("Freeing allocated maxfile data:\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            sw.Reset();
            sw.Start();
            transport.Close();
            sw.Stop();
            Console.WriteLine("Closing connection:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);
        }
        catch (SocketException e)
        {
            Console.WriteLine("Could not connect to the server: {0}.", e.Message);
            Environment.Exit(-1);
        }
        catch (Exception e)
        {
            Console.WriteLine("An error occured: {0}", e.Message);
            Environment.Exit(-1);
        }

        return dataOut;
    }

    /// <summary> Calculates simpleDfe and simpleCpu and
    /// checks if they return the same value. 
    /// </summary>
    /// <param name = "args"> Command line arguments </param>
    public static void Main(string[] args)
    {
        Stopwatch sw = new Stopwatch();
        int status;

        sw.Start();
        const int SIZE = 1024;
        List<double> dataIn = new List<double>();

        for (int i = 0; i < SIZE; i++)
        {
            dataIn.Add(i + 1);
        }

        sw.Stop();
        Console.WriteLine("Generating input data:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

        // DFE Output
        sw.Reset();
        sw.Start();
        List<double> dataOutDFE = SimpleDFE(SIZE, dataIn);
        sw.Stop();
        Console.WriteLine("DFE simple total time:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

        // CPU Output
        sw.Reset();
        sw.Start();
        List<double> dataOutCPU = SimpleCPU(SIZE, dataIn);
        sw.Stop();
        Console.WriteLine("CPU simple total time:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

        // Checking results
        sw.Reset();
        sw.Start();
        status = Check(dataOutDFE, dataOutCPU, SIZE);
        sw.Stop();
        Console.WriteLine("Checking results:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

        if (status > 0)
        {
            Console.WriteLine("Test failed {0} times! ", status);
            Environment.Exit(-1);
        }
        else
        {
            Console.WriteLine("Test passed!");
        }
    }
}
