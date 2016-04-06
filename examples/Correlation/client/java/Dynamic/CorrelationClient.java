import com.maxeler.correlation.correlationService;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/** Correlation Dynamic example. */
public final class CorrelationClient {
  /** Maximal number of variables. */
  private static final int CORRELATION_MAX_NUM_VARIABLES = 6000;
  /** Maximal number of Time series. */
  private static final int CORRELATION_MAX_NUM_TIMESERIES = 6000;
  /** Maximal number of top scores. */
  private static final int CORRELATION_NUM_TOP_SCORES = 10;
  /** Number of pipes. */
  private static final int CORRELATION_NUM_PIPES = 12;
  /** PCIe alignment. */
  private static final int CORRELATION_PCIE_ALIGNMENT = 16;
  /** Number of vectors per burst. */
  private static final int CORRELATION_NUM_VECTORS_PER_BURST = 2;

  /** For anything other than ISCA this should be 384. */
  private static final int BURST_SIZE = 192;

  /** Number of nano seconds in one second. */
  private static final int NUM_OF_NANO_SECONDS = 1000000000;

  /** Size of double in bits. */
  private static final int  SIZE_OF_DOUBLE_IN_BITS = 64;
  /** Size of int in bits. */
  private static final int SIZE_OF_INT_IN_BITS = 32;
  /** Size of double in bytes. */
  private static final int  SIZE_OF_DOUBLE_IN_BYTES = 8;
  /** Size of int in bytes. */
  private static final int SIZE_OF_INT_IN_BYTES = 4;
  /** Size of megabyte in bytes. */
  private static final int SIZE_OF_MEGABYTE = 1000000;

  /** Server port. */
  private static final int PORT = 9090;

  /** Utility classes should not have a public or default constructor. */
  private CorrelationClient() { }

  /**
   * Generates random data.
   *
   * @param numberOfRows    Number of rows
   * @param numberOfColumns Number of columns
   *
   * @return                Random data
   */
  public static double[][] randomData(final int numberOfRows,
                                      final int numberOfColumns) {
    Random rn = new Random();
    final int minimum = 0;
    final int maximum = NUM_OF_NANO_SECONDS;
    final int range = maximum - minimum + 1;

    double[][] data = new double[numberOfRows][numberOfColumns];

    // Generate input data
    for (int i = 0; i < numberOfRows; i++) {
      for (int j = 0; j < numberOfColumns; j++) {
        data[i][j] =  ((double) (rn.nextInt(range) + minimum)) / maximum;
      }
    }

    return data;
  }

  /**
   * Calculate number of bursts for initializing LMem.
   *
   * @param numTimeseries Number of Time series
   *
   * @return              Number of bursts
   */
  public static int calcNumBursts(final int numTimeseries) {
    int numVectors = 0;
    for (int i = 1; i <= numTimeseries; ++i) {
      numVectors += (i + (CORRELATION_NUM_PIPES - 1)) / CORRELATION_NUM_PIPES;
    }

    return (numVectors + (CORRELATION_NUM_VECTORS_PER_BURST - 1))
           / CORRELATION_NUM_VECTORS_PER_BURST;
  }

  /**
   * Precalculates and reorders data for the DFE.
   *
   * @param data             Data for correlation
   * @param sizeTimeseries   Size   of Time series
   * @param numTimeseries    Number of Time series
   * @param precalculations  Precalculations
   * @param dataPairs        Data pairs
   */
  public static void prepareDataForDfe(
      final double[][] data, final int sizeTimeseries, final int numTimeseries,
      final double[] precalculations, final double[] dataPairs) {
    final int numTimesteps = sizeTimeseries;
    final double windowSize = (double) sizeTimeseries;

    if (numTimeseries > CORRELATION_MAX_NUM_TIMESERIES) {
      System.out.println("Number of Time series should be less or equal to "
                         + CORRELATION_MAX_NUM_TIMESERIES + ". Terminating!");
      System.exit(-1);
    }

    if (windowSize < 2) {
      System.out.println(
          "Window size must be equal or greater than 2. Terminating!");
      System.exit(-1);
    }

    if (numTimesteps > sizeTimeseries) {
      System.out.println(
          "Number of Time steps should be less or equal to"
          + " size of Time series. Terminating!");
      System.exit(-1);
    }

    double oldVal;
    double newVal;
    double[][] sums = new double[numTimesteps][numTimeseries];
    double[][] sumsSq = new double[numTimesteps][numTimeseries];
    double[][] inv = new double[numTimesteps][numTimeseries];

    // 2 DFE input streams: precalculations and data pairs
    for (int i = 0; i < numTimesteps; i++) {
      for (int j = 0; j < numTimeseries; j++) {
        oldVal = 0;
        if (i > windowSize) {
          oldVal = data[j][i - (int) windowSize];
        }
        newVal = data[j][i];

        if (i == 0) {
          sums[i][j] = newVal;
          sumsSq[i][j] = newVal * newVal;
        } else {
          sums[i][j] = sums[i - 1][j] + newVal - oldVal;
          sumsSq[i][j] = sumsSq[i - 1][j] + newVal * newVal - oldVal * oldVal;
        }

        inv[i][j] = 1 / Math.sqrt((int) windowSize * sumsSq[i][j]
                    - sums[i][j] * sums[i][j]);

        //Precalculations REORDERED in DFE ORDER
        precalculations[2 * i * numTimeseries + 2 * j] = sums[i][j];
        precalculations[2 * i * numTimeseries + 2 * j + 1] = inv[i][j];

        //Data pairs REORDERED in DFE ORDER
        dataPairs[2 * i * numTimeseries + 2 * j] = newVal;
        dataPairs[2 * i * numTimeseries + 2 * j + 1] = oldVal;
      }
    }
  }

  /**
   * Calculates number  of correlations.
   *
   * @param numTimeseries Number of Time series
   *
   * @return              Number of correlations
   */
  public static int calcNumCorrelations(final int numTimeseries) {
    return (numTimeseries * (numTimeseries - 1)) / 2;
  }

  /**
   * Calculates index of correlationDfe between i and j.
   *
   * @param row the first series
   * @param column the second series
   *
   * @return  Index
   */
  public static int calcIndex(final int row, final int column) {
    if (row == column) {
      System.out.println("row and j must not be the same!");
      return -1;
    }

    int smaller = row;
    int bigger = column;

    if (row < column) {
      smaller = column;
      bigger = row;
    }

    return (smaller * (smaller - 1)) / 2 + bigger;
  }

  /**
   * Calculates correlations on DFE.
   *
   * @param data           Data for correlation
   * @param numTimeseries  Number of Time series
   * @param sizeTimeseries Size   of Time series
   *
   * @return               Correlations
   */
  public static double[] correlateDfe(
      final double[][] data, final int numTimeseries,
      final int sizeTimeseries) {
    final int numOfCorrelations = calcNumCorrelations(numTimeseries);
    double[] correlations = new double[numOfCorrelations];

    DecimalFormat timeFormat = new DecimalFormat("#0.00000");
    double startTime = System.nanoTime();

    // Make socket
    TTransport transport = new TSocket("localhost", PORT);

    // Wrap in a protocol
    TProtocol protocol = new TBinaryProtocol(transport);

    // Create a client to use the protocol encoder
    correlationService.Client client = new correlationService.Client(protocol);

    double estimatedTime = (System.nanoTime() - startTime)
                           / NUM_OF_NANO_SECONDS;
    System.out.println("Createing a client:\t\t\t\t  "
                       + timeFormat.format(estimatedTime) + "s");

    try {
      // Connect!
      startTime = System.nanoTime();
      transport.open();
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Opening connection:\t\t\t\t  "
                         + timeFormat.format(estimatedTime) + "s");

      // Initialize maxfile
      startTime = System.nanoTime();
      final long maxfile = client.correlation_init();
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Initializing maxfile:\t\t\t  "
                         + timeFormat.format(estimatedTime) + "s");

      // Load DFE
      startTime = System.nanoTime();
      final long engine = client.max_load(maxfile, "*");
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Loading DFE:\t\t\t\t  "
                         + timeFormat.format(estimatedTime) + "s");

      final int numTimesteps = sizeTimeseries;
      final double windowSize = (double) sizeTimeseries;
      final int numBursts = calcNumBursts(numTimeseries);

      // Get loop length
      startTime = System.nanoTime();
      List<Integer> loopLength = new ArrayList<Integer>();
      loopLength.add(client.correlation_get_CorrelationKernel_loopLength());
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Geting Correlation Kernel loopLength:\t  "
                         + timeFormat.format(estimatedTime) + "s");

      // Prepare data for DFE
      startTime = System.nanoTime();

      int burstSize = BURST_SIZE;
      List<Integer> inMemLoad = new ArrayList<Integer>();
      for (int i = 0; i < numBursts * burstSize; i++) {
        inMemLoad.add(0);
      }

      double[] precalculations = new double[2 * numTimeseries * numTimesteps];
      double[] dataPairs = new double[2 * numTimeseries * numTimesteps];
      prepareDataForDfe(data, sizeTimeseries, numTimeseries,
                        precalculations, dataPairs);

      List<Double> precalculationsVec = new ArrayList<Double>();
      List<Double> dataPairsVec = new ArrayList<Double>();
      for (int i = 0; i < 2 * numTimeseries * numTimesteps; i++) {
        precalculationsVec.add(precalculations[i]);
        dataPairsVec.add(dataPairs[i]);
      }

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Data reordering time:\t\t\t  "
                         + timeFormat.format(estimatedTime) + "s");

      // Allocate and send input streams to server
      startTime = System.nanoTime();
      long loopLengthSize = 1;
      final long addressLoopLength = client.malloc_int32_t(loopLengthSize);
      client.send_data_int32_t(addressLoopLength, loopLength);
      double loopLengthTime = (System.nanoTime() - startTime)
                              / NUM_OF_NANO_SECONDS;
      System.out.println("\tSending LoopLength:\t\t(size = "
                         + loopLengthSize * SIZE_OF_INT_IN_BITS + " bit)\t\t"
                         + timeFormat.format(loopLengthTime) + "s");

      startTime = System.nanoTime();
      final long inMemLoadSize = numBursts * burstSize;
      final long addressInMemLoad = client.malloc_int32_t(inMemLoadSize);
      client.send_data_int32_t(addressInMemLoad, inMemLoad);
      final double inMemLoadTime = (System.nanoTime() - startTime)
                                   / NUM_OF_NANO_SECONDS;
      System.out.println("\tSending InMemLoad:\t\t(size = "
                         + inMemLoadSize * SIZE_OF_INT_IN_BITS + " bit)\t"
                         + timeFormat.format(inMemLoadTime) + "s");

      startTime = System.nanoTime();
      final long precalculationsVecSize = 2 * numTimeseries * numTimesteps;
      final long addressPrecalculations = client.malloc_double(
          precalculationsVecSize);
      client.send_data_double(addressPrecalculations, precalculationsVec);
      final double precalculationsVecTime = (System.nanoTime() - startTime)
                                            / NUM_OF_NANO_SECONDS;
      System.out.println("\tSending Precalculations:\t(size = "
                         + precalculationsVecSize
                         * SIZE_OF_DOUBLE_IN_BITS + " bit)\t"
                         + timeFormat.format(precalculationsVecTime) + "s");

      startTime = System.nanoTime();
      final long dataPairsVecSize = 2 * numTimeseries * numTimesteps;
      final long addressDataPairs = client.malloc_double(dataPairsVecSize);
      client.send_data_double(addressDataPairs, dataPairsVec);
      final double dataPairsVecTime = (System.nanoTime() - startTime)
                                      / NUM_OF_NANO_SECONDS;
      System.out.println("\tSending DataPairs:\t\t(size = "
                         + dataPairsVecSize * SIZE_OF_DOUBLE_IN_BITS + " bit)\t"
                         + timeFormat.format(dataPairsVecTime) + "s");

      estimatedTime = loopLengthTime + inMemLoadTime
                      + precalculationsVecTime + dataPairsVecTime;
      double speed = (loopLengthSize * SIZE_OF_INT_IN_BITS
                      + inMemLoadSize * SIZE_OF_INT_IN_BITS
                      + precalculationsVecSize * SIZE_OF_DOUBLE_IN_BITS
                      + dataPairsVecSize * SIZE_OF_DOUBLE_IN_BITS)
                     / estimatedTime / SIZE_OF_MEGABYTE;
      System.out.println("Sending input streams to server total time:\t  "
                         + timeFormat.format(estimatedTime)
                         + "s\t(average speed = " + timeFormat.format(speed)
                         + "Mb/s)");

      // Allocate memory for output stream on server
      startTime = System.nanoTime();
      // for anything other than ISCA 48 should be instead of 24
      final long outCorrelationSize =
          numTimesteps * loopLength.get(0) * CORRELATION_NUM_TOP_SCORES
          * CORRELATION_NUM_PIPES + numBursts * 24;
      final long addressOutCorrelation = client.malloc_double(
          outCorrelationSize);

      final long outIndicesSize =
          2 * numTimesteps * loopLength.get(0)
          * CORRELATION_NUM_TOP_SCORES * CORRELATION_NUM_PIPES;
      final long addressOutIndices = client.malloc_int32_t(
          outIndicesSize);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Allocating memory for output stream on server:  "
                         + timeFormat.format(estimatedTime) + "s");


      // Initialize LMem
      startTime = System.nanoTime();

      long actions = client.max_actions_init(maxfile, "loadLMem");
      client.max_set_param_uint64t(actions, "numBursts", numBursts);
      int test = client.max_get_offset_auto_loop_size(
          actions, "CorrelationKernel", "loopLength");
      client.max_queue_input(actions, "in_memLoad", addressInMemLoad,
                             numBursts * burstSize);
      client.max_ignore_scalar(
          actions, "LMemCommandsKernel", "run_cycle_count");
      client.max_ignore_scalar(
          actions, "CorrelationKernel", "run_cycle_count");

      client.max_run(engine, actions);

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("LMem initialization:\t\t\t    "
                         + timeFormat.format(estimatedTime) + "s");

      //Executing correlation action
      startTime = System.nanoTime();

      actions = client.max_actions_init(maxfile, "default");
      client.max_set_param_uint64t(actions, "numBursts", numBursts);
      client.max_set_param_uint64t(actions, "numSteps", numTimesteps);
      client.max_set_param_uint64t(actions, "numVariables", numTimeseries);
      client.max_set_param_uint64t(actions, "outputLastStep", 1);
      client.max_set_param_double(actions, "windowSize", windowSize);
      client.max_queue_input(
          actions, "in_precalculations", addressPrecalculations,
          precalculationsVecSize * SIZE_OF_DOUBLE_IN_BYTES);
      client.max_queue_input(
          actions, "in_variable_pair", addressDataPairs,
          dataPairsVecSize * SIZE_OF_DOUBLE_IN_BYTES);
      client.max_queue_output(
          actions, "out_correlation", addressOutCorrelation,
          outCorrelationSize * SIZE_OF_DOUBLE_IN_BYTES);
      client.max_queue_output(
          actions, "out_indices", addressOutIndices,
          outIndicesSize * SIZE_OF_INT_IN_BYTES);

      client.max_run(engine, actions);

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Correlation time:\t\t\t\t  "
                         + timeFormat.format(estimatedTime) + "s");

      // Unload DFE
      startTime = System.nanoTime();
      client.max_unload(engine);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Unloading DFE:\t\t\t\t  "
                         + timeFormat.format(estimatedTime) + "s");

      // Get output stream from server
      startTime = System.nanoTime();
      List<Double> outCorrelation = new ArrayList<Double>();
      outCorrelation = client.receive_data_double(addressOutCorrelation,
                                                  outCorrelationSize);
      final double outCorrelationTime = (System.nanoTime() - startTime)
                                        / NUM_OF_NANO_SECONDS;
      System.out.println("\tGet output stream Correlation:\t(size = "
                         + outCorrelationSize
                         * SIZE_OF_DOUBLE_IN_BITS + " bit)\t"
                         + timeFormat.format(outCorrelationTime) + "s");

      startTime = System.nanoTime();
      List<Integer> outIndices = new ArrayList<Integer>();
      outIndices = client.receive_data_int32_t(addressOutIndices,
                                               outIndicesSize);
      final double outIndicesTime = (System.nanoTime() - startTime)
                                    / NUM_OF_NANO_SECONDS;
      System.out.println("\tGet output stream outIndices:\t(size = "
                         + outIndicesSize * SIZE_OF_INT_IN_BITS + " bit)\t"
                         + timeFormat.format(outIndicesTime) + "s");

      startTime = System.nanoTime();
      loopLengthSize = 1;
      loopLength = client.receive_data_int32_t(addressLoopLength,
                                               loopLengthSize);
      loopLengthTime = (System.nanoTime() - startTime)
                       / NUM_OF_NANO_SECONDS;
      System.out.println("\tGet output stream loopLength:\t(size = "
                         + loopLengthSize * SIZE_OF_INT_IN_BITS + " bit)\t\t"
                         + timeFormat.format(loopLengthTime) + "s");

      estimatedTime = outCorrelationTime + outIndicesTime + loopLengthTime;
      speed = (outCorrelationSize * SIZE_OF_DOUBLE_IN_BITS
               + outIndicesSize * SIZE_OF_INT_IN_BITS
               + loopLengthSize * SIZE_OF_INT_IN_BITS)
              / estimatedTime / SIZE_OF_MEGABYTE;
      System.out.println("Sending input streams to server total time:\t  "
                         + timeFormat.format(estimatedTime)
                         + "s\t(average speed = " + timeFormat.format(speed)
                         + "Mb/s)");

      // Free allocated memory for streams on server
      startTime = System.nanoTime();
      client.free(addressLoopLength);
      client.free(addressInMemLoad);
      client.free(addressPrecalculations);
      client.free(addressDataPairs);
      client.free(addressOutCorrelation);
      client.free(addressOutIndices);
      client.free(actions);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Freeing allocated memory for streams on server: "
                         + timeFormat.format(estimatedTime) + "s");

      // Free allocated maxfile data
      startTime = System.nanoTime();
      client.correlation_free();
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Freeing allocated maxfile data:\t\t  "
                         + timeFormat.format(estimatedTime) + "s");

      // Close!
      startTime = System.nanoTime();
      transport.close();
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Closing connection:\t\t\t\t  "
                         + timeFormat.format(estimatedTime) + "s");

      // Store data
      startTime = System.nanoTime();

      final int start = (numTimesteps - 1) * loopLength.get(0)
                  * CORRELATION_NUM_TOP_SCORES * CORRELATION_NUM_PIPES;

      int position = 0;
      int index = 0;

      for (int i = 0; i < numTimeseries; i++) {
        for (int j = 0; j < i; j++) {
          correlations[index + j] = outCorrelation.get(start + position + j);
        }
        index += i;
        position += ((i / CORRELATION_NUM_PIPES) + 1) * CORRELATION_NUM_PIPES;
      }

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Storing time:\t\t\t\t  "
                         + timeFormat.format(estimatedTime) + "s");


    } catch (TException x) {
      x.printStackTrace();
      System.exit(-1);
    }

    return correlations;
  }

  /**
   * Calculates correlations on CPU.
   *
   * @param data           Data for correlation
   * @param numTimeseries  Number of Time series
   * @param sizeTimeseries Size   of Time series
   *
   * @return               Correlations
   */
  public static double[] correlateCpu(
      final double[][] data, final int numTimeseries,
      final double sizeTimeseries) {
    final double windowSize = sizeTimeseries;
    final double numTimesteps = sizeTimeseries;

    double[] sums = new double[numTimeseries];
    double[] sumsSq = new double[numTimeseries];
    for (int i = 0; i < numTimeseries; i++) {
      sums[i] = 0;
      sumsSq[i] = 0;
    }

    double[] sumsXtimesY = new double[calcNumCorrelations(numTimeseries)];
    for (int i = 0; i < calcNumCorrelations(numTimeseries); i++) {
      sumsXtimesY[i] = 0;
    }

    final int numOfCorrelations = calcNumCorrelations(numTimeseries);
    double[] correlations = new double[numOfCorrelations];
    for (int k = 0; k < numTimesteps; k++) {
      int indexCorrelation = 0;
      for (int i = 0; i < numTimeseries; i++) {
        double oldVal = 0;
        if (k >= windowSize) {
          oldVal = data[i][k - (int) windowSize];
        }
        double newVal = data[i][k];
        sums[i] += newVal - oldVal;
        sumsSq[i] += newVal * newVal - oldVal * oldVal;
      }
      for (int i = 0; i < numTimeseries; i++) {
        double oldX = 0;
        if (k >= windowSize) {
          oldX = data[i][k - (int) windowSize];
        }
        double newX = data[i][k];
        for (int j = 0; j < i; j++) {
          double oldY = 0;
          if (k >= windowSize) {
            oldY = data[j][k - (int) windowSize];
          }
          double newY = data[j][k];
          sumsXtimesY[indexCorrelation] += newX * newY - oldX * oldY;
          double numerator = (windowSize * sumsXtimesY[indexCorrelation]
                             - sums[i] * sums[j]);
          double denominator = ((1 / Math.sqrt(windowSize * sumsSq[i]
                                 - sums[i] * sums[i]))
                               * (1 / Math.sqrt(windowSize * sumsSq[j]
                                  - sums[j] * sums[j])));
          correlations[indexCorrelation] = numerator * denominator;
          indexCorrelation += 1;
        }
      }
    }

    return correlations;
  }

  /**
   * Calculates indices step.
   *
   * @param numTimeseries  Number of Time series
   * @param sizeTimeseries Size   of Time series
   *
   * @return               Indices step
   */
  public static int[] calculateIndicesStep(final int numTimeseries,
                                           final int sizeTimeseries) {
    final double numTimesteps = sizeTimeseries;
    final int numOfCorrelations = calcNumCorrelations(numTimeseries);
    int[] indicesStep = new int[2 * numOfCorrelations];
    for (int k = 0; k < numTimesteps; k++) {
      int indexCorrelation = 0;
      for (int i = 0; i < numTimeseries; i++) {
        for (int j = 0; j < i; j++) {
          indicesStep[2 * indexCorrelation] = j;
          indicesStep[2 * indexCorrelation + 1] = i;
          indexCorrelation += 1;
        }
      }
    }

    return indicesStep;
  }

  /**
   * Checks if correlationsDfe and correlationsCpu are the same.
   *
   * @param correlationsDfe Correaltion on DFE
   * @param correlationsCpu Correaltion on CPU
   * @param numTimeseries   Number of Time series
   * @param sizeTimeseries  Size   of Time series
   */
  public static void check(
      final double[] correlationsDfe, final double[] correlationsCpu,
      final int numTimeseries, final int sizeTimeseries) {
    int failed = 0;

    int[] indicesStep = calculateIndicesStep(numTimeseries, sizeTimeseries);

    for (int i = 0; i < numTimeseries * (numTimeseries - 1) / 2; i++) {
      int index = calcIndex(indicesStep[2 * i], indicesStep[2 * i + 1]);
      if (correlationsDfe[index] != correlationsCpu[i]) {
        failed++;
        System.out.println("correlationCpu[" + i + "]\t=  "
                           + correlationsCpu[i]);
        System.out.println("correlationDfe[" + index + "]\t=  "
                           + correlationsDfe[index]);
      }
    }
    if (failed == 0) {
      System.out.println("Test passed!");
    } else {
      System.out.println("Test failed " + failed + " times.");
      System.exit(-1);
    }
  }

  /**
   * Calculates correlationsDfe and correlationsCpu and
   * checks if they return the same value.
   *
   * @param args Command line arguments
   */
  public static void main(final String[] args) {
    if (args.length != 2) {
      System.out.println("Usage: ant -DstreamSize=<stream size> "
                         + "-DnumberOfStreams=<number of streams>");
      System.exit(-1);
    }
    final int sizeTimeseries = Integer.parseInt(args[0]);
    final int numTimeseries = Integer.parseInt(args[1]);
    double[][] data = randomData(numTimeseries, sizeTimeseries);

    DecimalFormat timeFormat = new DecimalFormat("#0.00000");
    double startTime = System.nanoTime();
    final double[] correlationsDfe = correlateDfe(data, numTimeseries,
                                                  sizeTimeseries);
    double estimatedTime = (System.nanoTime() - startTime)
                           / NUM_OF_NANO_SECONDS;
    System.out.println("DFE correlation total time:\t\t"
                       + timeFormat.format(estimatedTime) + "s");

    startTime = System.nanoTime();
    final double[] correlationsCpu = correlateCpu(data, numTimeseries,
                                                  sizeTimeseries);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("CPU correlation total time:\t\t"
                       + timeFormat.format(estimatedTime) + "s");

    check(correlationsDfe, correlationsCpu, numTimeseries, sizeTimeseries);
  }
}

