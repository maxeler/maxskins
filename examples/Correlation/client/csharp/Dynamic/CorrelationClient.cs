using System;
using System.Linq;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Diagnostics;

using Thrift;
using Thrift.Protocol;
using Thrift.Transport;

using com.maxeler.correlation;

internal class CorrelationClient {
        private const int correlationMaxNumVariables = 6000;
        private const int correlationMaxNumTimeseries = 6000;
        private const int correlationNumTopScores = 10;
        private const int correlationNumPipes = 12;
        private const int correlationPCIE_ALIGNMENT = 16;
        private const int correlationNumVectorsPerBurst = 2;

        public static void randomData(double[,] data, int numTimeseries, int sizeTimeseries) {
                // Random generatordataOut
                Random rn = new Random();

                // Generate input data
                for(int i = 0; i < numTimeseries; i++) {
                        for(int j = 0; j < sizeTimeseries; j++) {
                                data[i, j] = rn.NextDouble();
                        }
                }
        }

        public static int calcNumBursts(int numTimeseries) {
                // Calculate number of bursts for initializing LMem.
                int numVectors = 0;
                for (int i = 1; i <= numTimeseries; ++i) {
                        numVectors += (i + (correlationNumPipes - 1)) / correlationNumPipes;
                }

                return (numVectors + (correlationNumVectorsPerBurst-1)) / correlationNumVectorsPerBurst;
        }

        public static void prepareDataForDFE(double[,] data, int sizeTimeseries, int numTimeseries,
                                             int numTimesteps, double windowSize, double[] precalculations,
                                             double[] dataPairs) {
                // Precalculates and reorders data for the DFE.
                if (numTimeseries > correlationMaxNumTimeseries) {
                        Console.WriteLine("Number of Time series should be less or equal to {0}. Terminating!",
                                          correlationMaxNumTimeseries);
                        Environment.Exit(-1);
                }

                if (windowSize <2) {
                        Console.WriteLine("Window size must be equal or greater than 2. Terminating!");
                        Environment.Exit(-1);
                }

                if (numTimesteps > sizeTimeseries) {
                        Console.WriteLine("Number of Time steps should be less or equal to size of Time series. Terminating!");
                        Environment.Exit(-1);
                }

                double oldVal, newVal;
                double[,] sums = new double[numTimesteps, numTimeseries];
                double[,] sumsSQ = new double[numTimesteps, numTimeseries];
                double[,] inv = new double[numTimesteps, numTimeseries];

                // 2 DFE input streams: precalculations and data pairs 
                for (int i = 0; i < numTimesteps; i++) {
                        for (int j = 0; j < numTimeseries; j++) {
                                oldVal = 0;
                                if(i > windowSize) {
                                        oldVal = data [j, i - (int)windowSize];
                                }
                                newVal = data [j, i];

                                if (i==0) {
                                        sums [i, j] = newVal;
                                        sumsSQ [i, j] = newVal * newVal;
                                }
                                else {
                                        sums[i, j] = sums[i-1, j] + newVal - oldVal;
                                        sumsSQ[i, j] = sumsSQ[i-1, j] + newVal * newVal - oldVal * oldVal;
                                }

                                inv[i, j] = 1/Math.Sqrt((int)windowSize * sumsSQ[i, j] - sums[i, j] * sums[i, j]);

                                //Precalculations REORDERED in DFE ORDER
                                precalculations[2*i*numTimeseries + 2*j] = sums[i, j];
                                precalculations[2*i*numTimeseries + 2*j + 1] = inv[i, j];

                                //Data pairs REORDERED in DFE ORDER
                                dataPairs[2*i*numTimeseries + 2*j] = newVal;
                                dataPairs[2*i*numTimeseries + 2*j + 1] = oldVal;
                        }
                }
        }

        public static int calcNumCorrelations(int numTimeseries) {
                // Calculates number  of correlations.
                return (numTimeseries * (numTimeseries - 1)) / 2;
        }

        public static int calcIndex (int i, int j) {
                // Calculates index of correlation between i and j
                if (i==j) {
                        Console.WriteLine("i and j must not be the same!");
                        return -1;
                }
                if (i<j) {
                        int tmp;
                        tmp = j;
                        j = i;
                        i = tmp;
                }

                return (i*(i-1))/2+j;
        }

        public static void CorrelateDFE(double[,] data, int sizeTimeseries,
                                        int numTimeseries, double[] correlations) {
                // Calculates correlations on DFE.
                Stopwatch sw = new Stopwatch();
                sw.Start();

                // Make socket
                var socket = new TSocket("localhost", 9090);

                // Buffering is critical. Raw sockets are very slow
                var transport = new TBufferedTransport(socket);

                // Wrap in a protocol
                var protocol = new TBinaryProtocol(transport);

                // Create a client to use the protocol encoder
                var client = new correlationService.Client(protocol);

                sw.Stop();
                Console.WriteLine("Creating a client:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                try {
                        // Connect!
                        sw.Reset(); sw.Start();
                        transport.Open();
                        sw.Stop();
                        Console.WriteLine("Opening connection:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Initialize maxfile
                        sw.Reset(); sw.Start();
                        var maxfile = client.correlation_init();
                        sw.Stop();
                        Console.WriteLine("Initializing maxfile:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Load DFE
                        sw.Reset(); sw.Start();
                        var engine = client.max_load(maxfile, "*");
                        sw.Stop();
                        Console.WriteLine("Loading DFE:\t\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        int numTimesteps = sizeTimeseries;  
                        double windowSize = (double)sizeTimeseries;
                        int numBursts = calcNumBursts(numTimeseries);

                        // Get loop length
                        sw.Reset(); sw.Start();
                        List<int> loopLength = new List<int>();
                        loopLength.Add(client.correlation_get_CorrelationKernel_loopLength());
                        sw.Stop();
                        Console.WriteLine("Getting Correlation Kernel loopLength:\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Prepare data for DFE
                        sw.Reset(); sw.Start();

                        double[] precalculations = new double [2 * numTimeseries * numTimesteps];
                        double[] dataPairs = new double [2 * numTimeseries * numTimesteps];

                        int burstSize = 384 / 2;  // for anything other than ISCA this should be 384
                        List<int> inMemLoad = new List<int>();
                        for(int i = 0; i < numBursts * burstSize; i++) {
                                inMemLoad.Add(0);
                        }

                        prepareDataForDFE (data, sizeTimeseries, numTimeseries, numTimesteps, windowSize,
                                           precalculations, dataPairs);

                        List<double> precalculationsVec = new List<double>();
                        List<double> dataPairsVec = new List<double>();
                        for(int i = 0; i < 2 * numTimeseries * numTimesteps; i++) {
                                precalculationsVec.Add(precalculations[i]);
                                dataPairsVec.Add(dataPairs[i]);
                        }

                        sw.Stop();
                        Console.WriteLine("Data reordering time:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Allocate and send input streams to server
                        sw.Reset(); sw.Start();
                        var loopLengthSize = 1;
                        var addressLoopLength = client.malloc_int32_t(loopLengthSize);
                        client.send_data_int32_t(addressLoopLength, loopLength);
                        sw.Stop();
                        var loopLengthTime = sw.Elapsed.TotalMilliseconds / 1000;
                        Console.WriteLine("\tSending LoopLength:\t\t(size = {0} bit)\t\t{1}s",
                                          loopLengthSize * 32, loopLengthTime);

                        sw.Reset(); sw.Start();
                        var inMemLoadSize = numBursts * burstSize;
                        var addressInMemLoad = client.malloc_int32_t(inMemLoadSize);
                        client.send_data_int32_t(addressInMemLoad, inMemLoad);
                        sw.Stop();
                        var inMemLoadTime = sw.Elapsed.TotalMilliseconds / 1000;
                        Console.WriteLine("\tSending InMemLoad:\t\t(size = {0} bit)\t{1}s",
                                          inMemLoadSize * 32, inMemLoadTime);

                        sw.Reset(); sw.Start();
                        var precalculationsSize = 2 * numTimeseries * numTimesteps;
                        var addressPrecalculations = client.malloc_double(precalculationsSize);
                        client.send_data_double(addressPrecalculations, precalculationsVec);
                        sw.Stop();
                        var precalculationsTime = sw.Elapsed.TotalMilliseconds / 1000;
                        Console.WriteLine("\tSending Precalculations:\t(size = {0} bit)\t{1}s",
                                          precalculationsSize * 32, precalculationsTime);

                        sw.Reset(); sw.Start();
                        var dataPairsSize = 2 * numTimeseries * numTimesteps;
                        var addressDataPairs = client.malloc_double(dataPairsSize);
                        client.send_data_double(addressDataPairs, dataPairsVec);
                        sw.Stop();
                        var dataPairsTime = sw.Elapsed.TotalMilliseconds / 1000;
                        Console.WriteLine("\tSending DataPairs:\t\t(size = {0} bit)\t{1}s",
                                          dataPairsSize * 64, dataPairsTime);

                        var time = loopLengthTime + inMemLoadTime + precalculationsTime + dataPairsTime;
                        var speed = (loopLengthSize * 32 + inMemLoadSize * 32 + precalculationsSize * 64 + dataPairsSize * 64) / time / 1000000;
                        Console.WriteLine("Sending input streams to server total time:\t{0}s\t(average speed = {1}Mb/s)", time, speed);

                        // Allocate memory for output stream on server
                        sw.Reset(); sw.Start();
                        var addressOutCorrelation = client.malloc_double(numTimesteps * loopLength[0] *
                                                                         correlationNumTopScores * correlationNumPipes +
                                                                         numBursts * 24);
                        var addressOutIndices = client.malloc_int32_t(2 * numTimesteps * loopLength[0] *
                                                                      correlationNumTopScores * correlationNumPipes);
                        sw.Stop();
                        Console.WriteLine("Allocating memory for output stream on server:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Initialize LMem
                        sw.Reset(); sw.Start();

                        var actions = client.max_actions_init(maxfile, "loadLMem");
                        client.max_set_param_uint64t(actions, "numBursts", numBursts);
                        client.max_get_offset_auto_loop_size(actions, "CorrelationKernel", "loopLength");
                        client.max_queue_input(actions, "in_memLoad", addressInMemLoad, numBursts * burstSize);
                        client.max_ignore_scalar(actions, "LMemCommandsKernel", "run_cycle_count" );
                        client.max_ignore_scalar(actions, "CorrelationKernel", "run_cycle_count" );
    
                        client.max_run(engine, actions);

                        sw.Stop();
                        Console.WriteLine("LMem initialization:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        //Executing correlation action
                        sw.Reset(); sw.Start();

                        actions = client.max_actions_init(maxfile, "default");
                        client.max_set_param_uint64t(actions, "numBursts", numBursts);
                        client.max_set_param_uint64t(actions, "numSteps", numTimesteps);
                        client.max_set_param_uint64t(actions, "numVariables", numTimeseries);
                        client.max_set_param_uint64t(actions, "outputLastStep", 1);
                        client.max_set_param_double(actions, "windowSize", windowSize);
                        client.max_queue_input(actions, "in_precalculations", addressPrecalculations,
                                               2 * numTimeseries * numTimesteps * 8);
                        client.max_queue_input(actions, "in_variable_pair", addressDataPairs,
                                               2 * numTimeseries * numTimesteps * 8);
                        client.max_queue_output(actions, "out_correlation", addressOutCorrelation,
                                                (numTimesteps * loopLength[0] * correlationNumTopScores * correlationNumPipes +
                                                 numBursts * 24) * 8);  // for anything other than ISCA 48 should be instead of 24
                        client.max_queue_output(actions, "out_indices", addressOutIndices,
                                                2 * numTimesteps * loopLength[0] *
                                                correlationNumTopScores * correlationNumPipes * 4);
    
                        client.max_run(engine, actions);

                        sw.Stop();
                        Console.WriteLine("Correlation time:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Unload DFE
                        sw.Reset(); sw.Start();
                        client.max_unload(engine);
                        sw.Stop();
                        Console.WriteLine("Unloading DFE:\t\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Get output stream from server
                        sw.Reset(); sw.Start();
                        var outCorrelationSize = numTimesteps * loopLength[0] * correlationNumTopScores * correlationNumPipes + numBursts * 24;
                        List<double> outCorrelation = new List<double>();
                        outCorrelation = client.receive_data_double(addressOutCorrelation, outCorrelationSize);
                        sw.Stop();
                        var outCorrelationTime = sw.Elapsed.TotalMilliseconds / 1000;
                        Console.WriteLine("\tGet output stream Correlation:\t(size = {0} bit)\t{1}s",
                                          outCorrelationSize * 64, outCorrelationTime);

                        sw.Reset(); sw.Start();
                        var outIndicesSzie = 2 * numTimesteps * loopLength[0] * correlationNumTopScores * correlationNumPipes;
                        List<int> outIndices = new List<int>();
                        outIndices = client.receive_data_int32_t(addressOutIndices, outIndicesSzie);
                        sw.Stop();
                        var outIndicesTime = sw.Elapsed.TotalMilliseconds / 1000;
                        Console.WriteLine("\tGet output stream outIndices:\t(size = {0} bit)\t{1}s",
                                          outIndicesSzie * 32, outIndicesTime);

                        sw.Reset(); sw.Start();
                        loopLengthSize = 1;
                        loopLength = client.receive_data_int32_t(addressLoopLength, loopLengthSize);
                        sw.Stop();
                        loopLengthTime = sw.Elapsed.TotalMilliseconds / 1000;
                        Console.WriteLine("\tGet output stream loopLength:\t(size = {0} bit)\t\t{1}s",
                                          loopLengthSize * 32, loopLengthTime);

                        time = loopLengthTime + outCorrelationTime + outIndicesTime;
                        speed = (loopLengthSize * 32 + outIndicesSzie * 32 + outCorrelationSize * 64) / time / 1000000;
                        Console.WriteLine("Getting output stream from server total time:\t{0}s\t(average speed = {1}Mb/s)", time, speed);

                        // Free allocated memory for streams on server
                        sw.Reset(); sw.Start();
                        client.free(addressLoopLength);
                        client.free(addressInMemLoad);
                        client.free(addressPrecalculations);
                        client.free(addressDataPairs);
                        client.free(addressOutCorrelation);
                        client.free(addressOutIndices);
                        client.free(actions);
                        sw.Stop();
                        Console.WriteLine("Freeing allocated memory for streams on server:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Free allocated maxfile data
                        sw.Reset(); sw.Start();
                        client.correlation_free();
                        sw.Stop();
                        Console.WriteLine("Freeing allocated maxfile data:\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Close!
                        sw.Reset(); sw.Start();
                        transport.Close();
                        sw.Stop();
                        Console.WriteLine("Closing connection:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                        // Store data
                        sw.Reset(); sw.Start();

                        int position = 0;
                        int index = 0;
                        int start = (numTimesteps-1) * loopLength[0] * correlationNumTopScores * correlationNumPipes;

                        for (int i=0; i < numTimeseries; i++) {
                                for(int j = 0; j < i; j++) {
                                        correlations[index + j] = outCorrelation[start + position + j];
                                }
                                index += i;
                                position += ((i/12)+1)*12;      
                        }

                        sw.Stop();
                        Console.WriteLine("LMem initialization:\t\t\t\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);
                } catch (SocketException e) {
                        Console.WriteLine("Could not connect to the server: {0}.", e.Message);
                        Environment.Exit(-1);
                } catch (Exception e) {
                        Console.WriteLine("An error occured: {0}", e.Message);
                        Environment.Exit(-1);
                }
        }

        public static void CorrelateCPU(double[,] data, int numTimeseries, double windowSize,
                                        int numTimesteps, double[] correlationsCPU, int[] indicesStep) {
                // Calculates correlations on CPU.
                double[] sums = new double [numTimeseries];
                double[] sumsSQ = new double [numTimeseries];
                double[] sumsXY = new double [calcNumCorrelations(numTimeseries)];
                for(int i = 0; i < numTimeseries; i++) {
                        sums[i] = 0;
                        sumsSQ[i] = 0;
                }
                for(int i = 0; i < calcNumCorrelations(numTimeseries); i++) {
                        sumsXY[i] = 0;
                }

                for(int k = 0; k < numTimesteps; k++) {
                        int indexCorrelation = 0;
                        for(int i = 0; i < numTimeseries; i++) {
                                double oldVal = 0;
                                if(k >= windowSize) {
                                        oldVal = data[i, k - (int)windowSize];
                                }
                                double newVal = data[i, k];
                                sums[i] += newVal - oldVal;
                                sumsSQ[i] += newVal * newVal - oldVal * oldVal;
                        }
                        for(int i = 0; i < numTimeseries; i++) {
                                double oldX = 0;
                                if(k >= windowSize) {
                                        oldX = data[i, k - (int)windowSize];
                                }
                                double newX = data[i, k];
                                for(int j = 0; j < i; j++) {
                                        double oldY = 0;
                                        if(k >= windowSize) {
                                                oldY = data[j, k - (int)windowSize];
                                        }
                                        double newY = data[j, k];
                                        sumsXY[indexCorrelation] += newX * newY - oldX * oldY;
                                        double numerator = (windowSize * sumsXY[indexCorrelation] - sums[i] * sums[j]);
                                        double denominator = ((1 / Math.Sqrt(windowSize * sumsSQ[i] - sums[i] * sums[i])) * 
                                                              (1 / Math.Sqrt(windowSize * sumsSQ[j] - sums[j] * sums[j])));
                                        correlationsCPU[indexCorrelation] = numerator * denominator;
                                        indicesStep[2 * indexCorrelation] = j;
                                        indicesStep[2 * indexCorrelation + 1] = i;
                                        indexCorrelation += 1;
                                }
                        }
                }
        }
    
        public static void check(double[] correlationsDFE, double[] correlationsCPU,
                                 int numTimeseries, int[] indicesStep) {
                // Checks if correlationsDFE and correlationsCPU are the same.
                int failed = 0;

                for(int i = 0; i < numTimeseries * (numTimeseries - 1) / 2; i++) {
                        int j = calcIndex(indicesStep[2 * i], indicesStep[2 * i + 1]);
                        if(correlationsDFE[j] != correlationsCPU[i]) {
                                failed++;
                                Console.WriteLine("correlationCPU[{0}]\t=  {1}", i,  correlationsCPU[i]);
                                Console.WriteLine("correlationDFE[{0}]\t= {1} ", j,  correlationsDFE[j]);
                        }
                }
                if(failed == 0){
                        Console.WriteLine("Test passed!");
                } else {
                        Console.WriteLine("Test failed {0} times.", failed);
                        Environment.Exit(-1);
                }
        } 

        public static void Main(string[] args) {
                if(args.Length != 2) {
                        Console.WriteLine("Usage: CorrelationClient.exe <stream size> <number of streams>");
                        Environment.Exit(-1);
                } 
                int sizeTimeseries = Int32.Parse(args[0]);
                int numTimeseries = Int32.Parse(args[1]);
                int numOfCorrelations = calcNumCorrelations(numTimeseries);
                double[,] data = new double[numTimeseries, sizeTimeseries];
                double[] correlationsDFE = new double [numOfCorrelations];
                double[] correlationsCPU = new double [numOfCorrelations];
                int[] indicesStep = new int [2 * numOfCorrelations];
                
                randomData(data, numTimeseries, sizeTimeseries);

                Stopwatch sw = new Stopwatch();
                sw.Start();
                CorrelateDFE(data, sizeTimeseries, numTimeseries, correlationsDFE);
                sw.Stop();
                Console.WriteLine("DFE correlation total time:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                sw.Reset();
                sw.Start();
                CorrelateCPU(data, numTimeseries, sizeTimeseries, sizeTimeseries, correlationsCPU, indicesStep);
                sw.Stop();
                Console.WriteLine("CPU correlation total time:\t{0}s", sw.Elapsed.TotalMilliseconds / 1000);

                check(correlationsDFE, correlationsCPU, numTimeseries, indicesStep);
        }
}

