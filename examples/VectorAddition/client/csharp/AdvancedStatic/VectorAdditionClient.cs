// <copyright file="VectorAdditionClient.cs" company="Maxeler Technologies">
// Copyright 2016 Maxeler Technologies. All rights reserved.
// </copyright>
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Sockets;

using com.maxeler.VectorAddition;

using Thrift;
using Thrift.Protocol;
using Thrift.Transport;

/// <summary> 
/// Vector addition AdvancedStatic example
/// </summary>
internal class VectorAdditionClient 
{
    /// <summary> Checks if vectorAdditionDfe and vectorAdditionCpu return the same value 
    /// </summary>
    /// <param name = "dataOutDFE" > Data output from DFE </param>
    /// <param name = "dataOutCPU" > Data output from CPU </param>
    /// <param name = "size" > Size of array </param>
    /// <returns> Number of elements that doesn't match </returns>
    public static int Check(List<int> dataOutDFE, List<int> dataOutCPU, int size)
    {
        int status = 0;

        for (int i = 0; i < size; i++) 
        {
            if (dataOutDFE[i] != dataOutCPU[i]) 
            {
                Console.WriteLine("Output data @ {0} = {1} (expected {2})", i, dataOutDFE[i], dataOutCPU[i]);
                status++;
            }
        }

        return status;  
    }

    /// <summary> Vector addition on CPU </summary>
    /// <param name = "size"> Size of arrays </param>
    /// <param name = "firstVector"> First vector </param>
    /// <param name = "secondVector"> Second vector </param>
    /// <param name = "scalar"> Scalar parameter </param>
    /// <returns> Data output </returns>
    public static List<int> VectorAdditionCpu(int size, List<int> firstVector, List<int> secondVector, int scalar)
    {
        List<int> dataOut = new List<int>();

        for (int i = 0; i < size; i++)
        {
            dataOut.Add((firstVector[i] + secondVector[i]) + scalar);
        }

        return dataOut;
    }

    /// <summary> Vector addition on DFE </summary>
    /// <param name = "size"> Size of arrays </param>
    /// <param name = "firstVector"> First vector </param>
    /// <param name = "secondVector"> Second vector </param>
    /// <param name = "scalar"> Scalar parameter </param>
    /// <returns> Data output </returns>
    public static List<int> VectorAdditionDfe(int size, List<int> firstVector, List<int> secondVector, int scalar)
    {
        Stopwatch sw = new Stopwatch();
        sw.Start();

        // Make socket
        var transport = new TSocket("localhost", 9090);

        // Wrap in a protocol
        var protocol = new TBinaryProtocol(transport);

        // Create a client to use the protocol encoder
        var client = new VectorAdditionService.Client(protocol);

        sw.Stop();
        Console.WriteLine("Creating a client:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

        List<int> dataOut = new List<int>();

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
            var maxfile = client.VectorAddition_init();
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
            var address_x = client.malloc_int32_t(size);
            client.send_data_int32_t(address_x, firstVector);

            var address_y = client.malloc_int32_t(size);
            client.send_data_int32_t(address_y, secondVector);
            sw.Stop();
            Console.WriteLine("Sending input data:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Allocate memory for output stream on server
            sw.Reset();
            sw.Start();
            var address_out = client.malloc_float(size);
            sw.Stop();
            Console.WriteLine("Allocating memory for output stream on server:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Action writeLMem
            sw.Reset(); 
            sw.Start();
            VectorAddition_writeLMem_actions_t_struct action_lmem = new VectorAddition_writeLMem_actions_t_struct();
            action_lmem.Param_address = 0;
            action_lmem.Param_nbytes = size * 4;
            action_lmem.Instream_cpu_to_lmem = address_x;
            var address_action_lmem = client.send_VectorAddition_writeLMem_actions_t(action_lmem);
            client.VectorAddition_writeLMem_run(engine, address_action_lmem);
            sw.Stop();
            Console.WriteLine("Writing to LMem:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Action default 
            sw.Reset(); 
            sw.Start();
            VectorAddition_actions_t_struct action = new VectorAddition_actions_t_struct();
            action.Param_A = scalar;
            action.Param_N = size;
            action.Instream_y = address_y;
            action.Outstream_s = address_out;
            var address_action = client.send_VectorAddition_actions_t(action);
            client.VectorAddition_run(engine, address_action);
            sw.Stop();
            Console.WriteLine("Vector addition time:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Unload DFE
            sw.Reset(); 
            sw.Start();
            client.max_unload(engine);
            sw.Stop();
            Console.WriteLine("Unloading DFE:\t\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Get output stream from server
            sw.Reset();
            sw.Start();
            dataOut = client.receive_data_int32_t(address_out, size);
            sw.Stop();
            Console.WriteLine("Getting output stream:\t(size = {0} bit)\t{1}s", size * 32, sw.Elapsed.TotalMilliseconds / 1000);

            // Free allocated memory for streams on server
            sw.Reset();
            sw.Start();
            client.free(address_x);
            client.free(address_y);
            client.free(address_out);
            sw.Stop();
            Console.WriteLine("Freeing allocated memory for streams on server:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Free allocated maxfile data
            sw.Reset(); 
            sw.Start();
            client.VectorAddition_free();
            sw.Stop();
            Console.WriteLine("Freeing allocated maxfile data:\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

            // Close
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

    public static void Main(string[] args) 
    {
        Stopwatch sw = new Stopwatch();
        int status;

        // Generate input data
        sw.Start();
        const int SIZE = 384;

        List<int> firstVector = new List<int>();
        List<int> secondVector = new List<int>();

        const int Scalar = 3;

        Random random = new Random();
        for (int i = 0; i < SIZE; i++) 
        {
                firstVector.Add(random.Next(0, 100));
                secondVector.Add(random.Next(0, 100));
        }

        sw.Stop();
        Console.WriteLine("Generating input data:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

        // DFE Output
        sw.Reset();
        sw.Start();
        List<int> dataOutDFE = VectorAdditionDfe(SIZE, firstVector, secondVector, Scalar);
        sw.Stop();
        Console.WriteLine("DFE vector addition total time:\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

        // CPU Output
        sw.Reset();
        sw.Start();
        List<int> dataOutCPU = VectorAdditionCpu(SIZE, firstVector, secondVector, Scalar);
        sw.Stop();
        Console.WriteLine("CPU vector addition time:\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

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
