import com.maxeler.PassThrough.PassThroughService;
import com.maxeler.PassThrough.PassThrough_actions_t_struct;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

/** PassThrough AdvancedStatic example. */
public final class PassThroughClient {

  /** Number of nano seconds in one second. */
  private static final int NUM_OF_NANO_SECONDS = 1000000000;

  /** Size of int in bits. */
  private static final int SIZE_OF_INT = 32;

  /** Server port. */
  private static final int PORT = 9090;

  /** Utility classes should not have a public or default constructor. */
  private PassThroughClient() { }

  /**
   * Checks if passThroughDfe and passThroughCpu return the same value.
   *
   * @param dataOutDfe  Data output from DFE
   * @param dataOutCpu  Data output from CPU
   * @param size        Size
   */
  public static void check(final List<Double> dataOutDfe,
                           final List<Double> dataOutCpu, final int size) {
    for (int i = 0; i < size; i++) {
      if (!dataOutDfe.get(i).equals(dataOutCpu.get(i))) {
        System.out.println("Output data @ " + i + " = "
                           + dataOutDfe.get(i) + " (expected "
                           + dataOutCpu.get(i) + ")");

        System.out.println("Test failed.");
        System.exit(-1);
      }
    }

    System.out.println("Test passed!");
  }

  /**
   * PassThrough on CPU.
   *
   * @param size    Size
   * @param dataIn  Data input
   *
   * @return        Data output
   */
  public static List<Double> passThroughCpu(final int size,
                                            final List<Double> dataIn) {
    List<Double> dataOut = new ArrayList<Double>();

    for (int i = 0; i < size; i++) {
      dataOut.add(dataIn.get(i));
    }

    return dataOut;
  }

  /**
   * PassThrough on DFE.
   *
   * @param size    Size
   * @param dataIn  Data input
   *
   * @return        Data output
   */
  public static List<Double> passThroughDfe(final int size,
                                            final List<Double> dataIn) {
    List<Double> dataOut = new ArrayList<Double>();

    final DecimalFormat timeFormat = new DecimalFormat("#0.00000");

    double startTime = System.nanoTime();

    // Make socket
    TTransport transport = new TSocket("localhost", PORT);

    // Wrap in a protocol
    TProtocol protocol = new TBinaryProtocol(transport);

    // Create a client to use the protocol encoder
    PassThroughService.Client client =
        new PassThroughService.Client(protocol);

    double estimatedTime = (System.nanoTime() - startTime)
                           / NUM_OF_NANO_SECONDS;
    System.out.println("Creating a client:\t\t\t\t\t"
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
      final long maxfile = client.PassThrough_init();
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

      PassThrough_actions_t_struct action = new PassThrough_actions_t_struct(
          size, addressDataIn, addressDataOut);
      final long addressAction = client.send_PassThrough_actions_t(action);
      client.PassThrough_run(engine, addressAction);

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Pass through time:\t\t\t\t\t"
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
      client.PassThrough_free();
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
   * Calculates passThroughDfe and passThroughCpu and
   * checks if they return the same value.
   *
   * @param args Command line arguments
   */
  public static void main(final String[] args) {
    final int size = 1024;

    final DecimalFormat timeFormat = new DecimalFormat("#0.00000");

    // Generate data
    double startTime = System.nanoTime();

    List<Double> dataIn = new ArrayList<Double>();

    for (int i = 0; i < size; i++) {
      dataIn.add((double) (i + 1));
    }

    double estimatedTime = (System.nanoTime() - startTime)
                           / NUM_OF_NANO_SECONDS;
    System.out.println("Generating input data:\t\t\t\t"
                       + timeFormat.format(estimatedTime) + "s");

    // DFE Output
    startTime = System.nanoTime();
    final List<Double> dataOutDfe = passThroughDfe(size, dataIn);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("DFE pass through total time:\t\t\t"
                       + timeFormat.format(estimatedTime) + "s");

    // CPU Output
    startTime = System.nanoTime();
    final List<Double> dataOutCpu = passThroughCpu(size, dataIn);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("CPU pass through total time:\t\t\t"
                       + timeFormat.format(estimatedTime) + "s");

    // Checking results
    check(dataOutDfe, dataOutCpu, size);
  }
}

