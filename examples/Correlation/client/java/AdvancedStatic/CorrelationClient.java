import com.maxeler.correlation.*;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;

import java.util.Random;
import java.util.List;
import java.util.ArrayList;
import java.text.DecimalFormat;

public class CorrelationClient {
    private static final int correlationMaxNumVariables = 6000;
    private static final int correlationMaxNumTimeseries = 6000;
    private static final int correlationNumTopScores = 10;
    private static final int correlationNumPipes = 12;
    private static final int correlationPCIE_ALIGNMENT = 16;
    private static final int correlationNumVectorsPerBurst = 2;
    
    public static void randomData(double[][] data, int numTimeseries, int sizeTimeseries) {
        // Random generatordataOut
        Random rn = new Random();
        int minimum = 0;
        int maximum = 1000000000;    
        int range = maximum - minimum + 1;

        // Generate input data
        for(int i = 0; i < numTimeseries; i++) {
            for(int j = 0; j < sizeTimeseries; j++) {
                data[i][j] =  ((double)(rn.nextInt(range) + minimum)) / maximum;
            }
        }
    }

    public static int calcNumBursts(int numTimeseries) {
        // Calculate number of bursts for initializing LMem.
        int numVectors = 0;
        for (int i = 1; i <= numTimeseries; ++i)
            numVectors += (i + (correlationNumPipes - 1)) / correlationNumPipes;

        return (numVectors + (correlationNumVectorsPerBurst-1)) / correlationNumVectorsPerBurst;
    }

    public static void prepareDataForDFE(double[][] data, int sizeTimeseries, int numTimeseries,
                                         int numTimesteps, double windowSize, double[] precalculations,
                                         double[] dataPairs) {
        // Precalculates and reorders data for the DFE.
        if (numTimeseries > correlationMaxNumTimeseries) {
            System.out.println("Number of Time series should be less or equal to " +
                               correlationMaxNumTimeseries + ". Terminating!");
            System.exit(-1);
        }
        
        if (windowSize <2) {
            System.out.println("Window size must be equal or greater than 2. Terminating!");
            System.exit(-1);
        }

        if (numTimesteps > sizeTimeseries) {
            System.out.println("Number of Time steps should be less or equal to size of Time series. Terminating!");
            System.exit(-1);
        }

        double oldVal, newVal;
        double[][] sums = new double[numTimesteps][numTimeseries];
        double[][] sumsSQ = new double[numTimesteps][numTimeseries];
        double[][] inv = new double[numTimesteps][numTimeseries];

        // 2 DFE input streams: precalculations and data pairs 
        for (int i = 0; i < numTimesteps; i++) {
            for (int j = 0; j < numTimeseries; j++) {
                oldVal = 0;
                if(i > windowSize) {
                    oldVal = data [j][i - (int)windowSize];
                }
                newVal = data [j][i];

                if (i==0) {
                    sums [i][j] = newVal;
                    sumsSQ [i][j] = newVal * newVal;
                }
                else {
                    sums[i][j] = sums[i-1][j] + newVal - oldVal;
                    sumsSQ[i][j] = sumsSQ[i-1][j] + newVal * newVal - oldVal * oldVal;
                }
                        
                inv[i][j] = 1/Math.sqrt((int)windowSize * sumsSQ[i][j] - sums[i][j] * sums[i][j]);
                        
                //Precalculations REORDERED in DFE ORDER
                precalculations[2*i*numTimeseries + 2*j] = sums[i][j];
                precalculations[2*i*numTimeseries + 2*j + 1] = inv[i][j];

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
            System.out.println("i and j must not be the same!");
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

    public static void CorrelateDFE(double[][] data, int sizeTimeseries,
                                    int numTimeseries, double[] correlations) {
        // Calculates correlations on DFE.
        try {
            // Connect!
            TTransport transport;
            transport = new TSocket("localhost", 9090);
            transport.open();

            TProtocol protocol = new  TBinaryProtocol(transport);
            correlationService.Client client = new correlationService.Client(protocol);

            // Initialize maxfile
            long maxfile = client.correlation_init();

            // Load DFE
            long engine = client.max_load(maxfile, "*");
            
            DecimalFormat timeFormat = new DecimalFormat("#0.00000");
            double startTime = System.nanoTime();

            int numTimesteps = sizeTimeseries;  
            double windowSize = (double)sizeTimeseries;

            int numBursts = calcNumBursts(numTimeseries);
            List<Integer> loopLength = new ArrayList<Integer>();
            loopLength.add(client.correlation_get_CorrelationKernel_loopLength());

            double[] precalculations = new double [2 * numTimeseries * numTimesteps];
            double[] dataPairs = new double [2 * numTimeseries * numTimesteps];
        
            int burstSize = 384 / 2;  // for anything other than ISCA this should be 384
            List<Integer> inMemLoad = new ArrayList<Integer>();
            for(int i = 0; i < numBursts * burstSize; i++) {
                inMemLoad.add(0);
            }

            prepareDataForDFE (data, sizeTimeseries, numTimeseries, numTimesteps, windowSize,
                               precalculations, dataPairs);
            
            double estimatedTime = System.nanoTime() - startTime;
            System.out.println("Data reordering time:\t\t" +
                               timeFormat.format(estimatedTime / 1000000000) + "s");

            startTime = System.nanoTime();

            List<Double> precalculationsVec = new ArrayList<Double>();
            List<Double> dataPairsVec = new ArrayList<Double>();
            for(int i = 0; i < 2 * numTimeseries * numTimesteps; i++) {
                precalculationsVec.add(precalculations[i]);
                dataPairsVec.add(dataPairs[i]);
            }

            // Allocate and send input streams to server
            long addressLoopLength = client.malloc_int32_t(1);
            client.send_data_int32_t(addressLoopLength, loopLength);
    
            long addressInMemLoad = client.malloc_int32_t(numBursts * burstSize);
            client.send_data_int32_t(addressInMemLoad, inMemLoad);

            long addressPrecalculations = client.malloc_double(2 * numTimeseries * numTimesteps);
            client.send_data_double(addressPrecalculations, precalculationsVec);

            long addressDataPairs = client.malloc_double(2 * numTimeseries * numTimesteps);
            client.send_data_double(addressDataPairs, dataPairsVec);

            // Allocate memory for output stream on server
            long addressOutCorrelation = client.malloc_double(numTimesteps * loopLength.get(0) *
                                                              correlationNumTopScores * correlationNumPipes +
                                                              numBursts * 24);
            long addressOutIndices = client.malloc_int32_t(2 * numTimesteps * loopLength.get(0) *
                                                           correlationNumTopScores * correlationNumPipes);

            correlation_loadLMem_actions_t_struct actionsLoadLMem = new correlation_loadLMem_actions_t_struct();
            actionsLoadLMem.setParam_numBursts(numBursts);
            actionsLoadLMem.setParam_CorrelationKernel_loopLength(addressLoopLength);
            actionsLoadLMem.setInstream_in_memLoad(addressInMemLoad);

            long addressActionsLoadLMem = client.send_correlation_loadLMem_actions_t(actionsLoadLMem);
            client.correlation_loadLMem_run(engine, addressActionsLoadLMem);
            client.free(addressActionsLoadLMem);
            System.out.println("LMem initialized!");

            //Executing correlation action
            correlation_actions_t_struct actions = new correlation_actions_t_struct() ;
	    actions.setParam_numBursts(numBursts);                           // scalar input
	    actions.setParam_numSteps(numTimesteps);                         // scalar input
	    actions.setParam_numVariables(numTimeseries);                    // scalar input
	    actions.setParam_outputLastStep(1);                              // scalar input
	    actions.setParam_windowSize(windowSize);                         // scalar input
	    actions.setInstream_in_precalculations(addressPrecalculations);  // streaming reordered input     
	    actions.setInstream_in_variable_pair(addressDataPairs);          // streaming reordered input
	    actions.setOutstream_out_correlation(addressOutCorrelation);     // streaming reordered input
	    actions.setOutstream_out_indices(addressOutIndices);             // streaming reordered input
	    long addressActions = client.send_correlation_actions_t(actions);
	    client.correlation_run(engine, addressActions);
	    client.free(addressActions);

	    // Unload DFE
	    client.max_unload(engine);

            // Get output stream from server
            List<Double> outCorrelation = new ArrayList<Double>();
            outCorrelation = client.receive_data_double(addressOutCorrelation,
                                                        numTimesteps * loopLength.get(0) *
                                                        correlationNumTopScores * correlationNumPipes +
                                                        numBursts * 24);
            List<Integer> outIndices = new ArrayList<Integer>();
            outIndices = client.receive_data_int32_t(addressOutIndices,
                                                     2 * numTimesteps * loopLength.get(0) *
                                                     correlationNumTopScores * correlationNumPipes);
            loopLength = client.receive_data_int32_t(addressLoopLength, 1);

            // Free allocated memory for streams on server
            client.free(addressLoopLength);
            client.free(addressInMemLoad);
            client.free(addressPrecalculations);
            client.free(addressDataPairs);
            client.free(addressOutCorrelation);
            client.free(addressOutIndices);

	    // Free allocated maxfile data
	    client.correlation_free();
	    
            // Close!
            transport.close();

            estimatedTime = System.nanoTime() - startTime;
            System.out.println("DFE execution time:\t\t\t" +
                               timeFormat.format(estimatedTime / 1000000000) + "s");

            startTime = System.nanoTime();

            int position = 0;
            int index = 0;
            int start = (numTimesteps-1) * loopLength.get(0) * correlationNumTopScores * correlationNumPipes;

            for (int i=0; i < numTimeseries; i++) {
                for(int j = 0; j < i; j++) {
                    correlations[index + j] = outCorrelation.get(start + position + j);
                }
                index += i;
                position += ((i/12)+1)*12;      
            }

            estimatedTime = System.nanoTime() - startTime;
            System.out.println("Storing time:\t\t\t" +
                               timeFormat.format(estimatedTime / 1000000000) + "s");
   
        } catch (TException x) {
            x.printStackTrace();
            System.exit(-1);
        }
        
    }

    public static void CorrelateCPU(double[][] data, int numTimeseries, double windowSize,
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
                    oldVal = data[i][k - (int)windowSize];
                }
                double newVal = data[i][k];
                sums[i] += newVal - oldVal;
                sumsSQ[i] += newVal * newVal - oldVal * oldVal;
            }
            for(int i = 0; i < numTimeseries; i++) {
                double oldX = 0;
                if(k >= windowSize) {
                    oldX = data[i][k - (int)windowSize];
                }
                double newX = data[i][k];
                for(int j = 0; j < i; j++) {
                    double oldY = 0;
                    if(k >= windowSize) {
                        oldY = data[j][k - (int)windowSize];
                    }
                    double newY = data[j][k];
                    sumsXY[indexCorrelation] += newX * newY - oldX * oldY;
                    double numerator = (windowSize * sumsXY[indexCorrelation] - sums[i] * sums[j]);
                    double denominator = ((1 / Math.sqrt(windowSize * sumsSQ[i] - sums[i] * sums[i])) * 
                                          (1 / Math.sqrt(windowSize * sumsSQ[j] - sums[j] * sums[j])));
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
                System.out.println("correlationCPU[" + i + "]\t=  " + correlationsCPU[i]);
                System.out.println("correlationDFE[" + j + "]\t=  " + correlationsDFE[j]);
            }
        }
        if(failed == 0) {
            System.out.println("Test passed!");
        } else {
            System.out.println("Test failed " + failed + " times.");
            System.exit(-1);
        }
    }

    public static void main(String [] args) {
        final int sizeTimeseries = 100;
        final int numTimeseries = 250;
        int numOfCorrelations = calcNumCorrelations(numTimeseries);
        double[][] data = new double[numTimeseries][sizeTimeseries];
        double[] correlationsDFE = new double [numOfCorrelations];
        double[] correlationsCPU = new double [numOfCorrelations];
        int[] indicesStep = new int [2 * numOfCorrelations];

        randomData(data, numTimeseries, sizeTimeseries);
        
        DecimalFormat timeFormat = new DecimalFormat("#0.00000");
        double startTime = System.nanoTime();
        CorrelateDFE(data, sizeTimeseries, numTimeseries, correlationsDFE);
        double estimatedTime = System.nanoTime() - startTime;
        System.out.println("DFE correlation total time:\t\t" +
                           timeFormat.format(estimatedTime / 1000000000) + "s");

        startTime = System.nanoTime();
        CorrelateCPU(data, numTimeseries, sizeTimeseries, sizeTimeseries, correlationsCPU, indicesStep);
        estimatedTime = System.nanoTime() - startTime;
        System.out.println("CPU correlation total time:\t\t" +
                           timeFormat.format(estimatedTime / 1000000000) + "s");

        check(correlationsDFE, correlationsCPU, numTimeseries, indicesStep);    
    }    
}
