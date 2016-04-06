import com.maxeler.MovingAverage.MovingAverageService;
import com.maxeler.MovingAverage.MovingAverage_actions_t_struct;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/** MovingAverage AdvancedStatic example. */
public final class MovingAverageClient {

  /** Average range. */
  private static final int AVERAGE_RANGE = 3;

  /** Minimal random number. */
  private static final int MIN_RAND_NUM = 0;

  /** Maximal random number. */
  private static final int MAX_RAND_NUM = 1000;

  /** Size of int in bits. */
  private static final int SIZE_OF_INT = 32;

  /** Number of nano seconds in one second. */
  private static final int NUM_OF_NANO_SECONDS = 1000000000;

  /** Server port. */
  private static final int PORT = 9090;

  /** Utility classes should not have a public or default constructor. */
  private MovingAverageClient() { }

  /**
   * Generates random data.
   *
   * @param size    Size
   *
   * @return        Random data
   */
  public static List<Double> randomData(final int size) {
    List<Double> randomData = new ArrayList<Double>();

    Random rn = new Random();
    final int range = MAX_RAND_NUM - MIN_RAND_NUM + 1;

    for (int i = 0; i < size; i++) {
      randomData.add((double) rn.nextInt(range) + MIN_RAND_NUM);
    }

    return randomData;
  }

  /**
   * Checks if movingAverageDfe and movingAverageCpu return the same value.
   *
   * @param dataOutDfe  Data output from DFE
   * @param dataOutCpu  Data output from CPU
   * @param size        Size
   */
  public static void check(final List<Double> dataOutDfe,
                           final List<Double> dataOutCpu, final int size) {
    for (int i = 1; i < size - 1; i++) {
      if (!dataOutDfe.get(i).equals(dataOutCpu.get(i))) {
        System.out.println(
            "Output data @ " + i + " = " + dataOutDfe.get(i)
            + " (expected " + dataOutCpu.get(i) + ")");

        System.out.println("Test failed!");
        System.exit(-1);
      }
    }

    System.out.println("Test successful!");
  }

  /**
   * MovingAverage on CPU.
   *
   * @param size    Size
   * @param dataIn  Data input
   *
   * @return        Data output
   */
  public static List<Double> movingAverageCpu(
      final int size, final List<Double> dataIn) {
    List<Double> dataOut = new ArrayList<Double>();

    dataOut.add((dataIn.get(0) + dataIn.get(1)) / 2);

    for (int i = 1; i < size - 1; i++) {
      final float previous = (float) (double) dataIn.get(i - 1);
      final float current = (float) (double) dataIn.get(i);
      final float next = (float) (double) dataIn.get(i + 1);
      final float average = (previous + current + next) / AVERAGE_RANGE;
      dataOut.add((double) average);
    }

    dataOut.add((dataIn.get(size - 1) + dataIn.get(size - 2)) / 2);

    return dataOut;
  }

  /**
   * MovingAverage on DFE.
   *
   * @param size    Size
   * @param dataIn  Data input
   *
   * @return        Data output
   */
  public static List<Double> movingAverageDfe(
      final int size, final List<Double> dataIn) {
    List<Double> dataOut = new ArrayList<Double>();

    DecimalFormat timeFormat = new DecimalFormat("#0.00000");
    double startTime = System.nanoTime();

    // Make socket
    TTransport transport = new TSocket("localhost", PORT);

    // Wrap in a protocol
    TProtocol protocol = new TBinaryProtocol(transport);

    // Create a client to use the protocol encoder
    MovingAverageService.Client client =
        new MovingAverageService.Client(protocol);

    double estimatedTime = (System.nanoTime() - startTime)
                           / NUM_OF_NANO_SECONDS;
    System.out.println("Createing a client:\t\t\t\t\t"
                       + timeFormat.format(estimatedTime) + "s");

    try {
      // Connect!
      startTime = System.nanoTime();
      transport.open();
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Opening connection:\t\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Initialize maxfile
      startTime = System.nanoTime();
      final long maxfile = client.MovingAverage_init();
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Initializing maxfile:\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Load DFE
      startTime = System.nanoTime();
      final long engine = client.max_load(maxfile, "*");
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Loading DFE:\t\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Allocate and send input streams to server
      startTime = System.nanoTime();
      final long addressDataIn = client.malloc_float(size);
      client.send_data_float(addressDataIn, dataIn);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Sending input data:\t\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Allocate memory for output stream on server
      startTime = System.nanoTime();
      final long addressDataOut = client.malloc_float(size);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Allocating memory for output stream on server:\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Action default
      startTime = System.nanoTime();
      MovingAverage_actions_t_struct action =
          new MovingAverage_actions_t_struct(size, addressDataIn,
                                             addressDataOut);
      final long addressAction = client.send_MovingAverage_actions_t(action);
      client.MovingAverage_run(engine, addressAction);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Moving average time:\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Unload DFE
      startTime = System.nanoTime();
      client.max_unload(engine);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Unloading DFE:\t\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Get output stream from server
      startTime = System.nanoTime();
      dataOut = client.receive_data_float(addressDataOut, size);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Getting output stream:\t(size = "
                         + size * SIZE_OF_INT + " bit)\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Free allocated memory for streams on server
      startTime = System.nanoTime();
      client.free(addressDataIn);
      client.free(addressDataOut);
      client.free(addressAction);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Freeing allocated memory for streams on server:\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Free allocated maxfile data
      startTime = System.nanoTime();
      client.MovingAverage_free();
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Freeing allocated maxfile data:\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

      // Close!
      startTime = System.nanoTime();
      transport.close();
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Closing connection:\t\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

    } catch (TException x) {
      x.printStackTrace();
      System.exit(-1);
    }

    return dataOut;
  }

  /**
   * Calculates movingAverageDfe and movingAverageCpu and
   * checks if they return the same value.
   *
   * @param args Command line arguments
   */
  public static void main(final String[] args) {
    DecimalFormat timeFormat = new DecimalFormat("#0.00000");
    final int size = 384;

    // Generate data
    double startTime = System.nanoTime();
    final List<Double> dataIn = randomData(size);
    double estimatedTime = (System.nanoTime() - startTime)
                           / NUM_OF_NANO_SECONDS;
    System.out.println("Generating input data:\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

    // DFE Output
    startTime = System.nanoTime();
    final List<Double> dataOutDfe = movingAverageDfe(size, dataIn);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("DFE moving average total time:\t\t\t"
                       + timeFormat.format(estimatedTime) + "s");

    // CPU Output
    startTime = System.nanoTime();
    final List<Double> dataOutCpu = movingAverageCpu(size, dataIn);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("CPU moving average total time:\t\t\t"
                       + timeFormat.format(estimatedTime) + "s");

    // Checking results
    startTime = System.nanoTime();
    check(dataOutDfe, dataOutCpu, size);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("Checking results:\t\t\t\t\t"
                       + timeFormat.format(estimatedTime) + "s");
  }
}

