import com.maxeler.VectorAddition.VectorAdditionService;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/** VectorAddition Dynamic example. */
public final class VectorAdditionClient {

  /** Minimal random number. */
  private static final int MIN_RAND_NUM = 0;

  /** Maximal random number. */
  private static final int MAX_RAND_NUM = 1000;

  /** Size of int in bytes. */
  private static final int SIZE_OF_INT_IN_BYTES = 4;

  /** Size of int in bits. */
  private static final int SIZE_OF_INT = 32;

  /** Number of nano seconds in one second. */
  private static final int NUM_OF_NANO_SECONDS = 1000000000;

  /** Server port. */
  private static final int PORT = 9090;

  /** Utility classes should not have a public or default constructor. */
  private VectorAdditionClient() { }

  /**
   * Generates random data.
   *
   * @param size    Size
   *
   * @return        Random data
   */
  public static List<Integer> randomData(final int size) {
    List<Integer> randomData = new ArrayList<Integer>();

    Random rn = new Random();
    final int range = MAX_RAND_NUM - MIN_RAND_NUM + 1;

    for (int i = 0; i < size; i++) {
      randomData.add(rn.nextInt(range) + MIN_RAND_NUM);
    }

    return randomData;
  }

  /**
   * Checks if vectorAdditionDfe and vectorAdditionCpu return the same value.
   *
   * @param dataOutDfe  Data output from DFE
   * @param dataOutCpu  Data output from CPU
   * @param size        Size
   *
   * @return status     Number of elements that doesn't match
   */
  public static int check(final List<Integer> dataOutDfe,
                           final List<Integer> dataOutCpu, final int size) {
    int status = 0;

    for (int i = 0; i < size; i++) {
      if (!dataOutDfe.get(i).equals(dataOutCpu.get(i))) {
        System.out.println(
            "Output data @ " + i + " = " + dataOutDfe.get(i)
            + " (expected " + dataOutCpu.get(i) + ")");

        status++;
      }
    }

    return status;
  }

  /**
   * VectorAddition on CPU.
   *
   * @param size          Size
   * @param firstVector   First vector
   * @param secondVector  Second vector
   * @param scalar        Scalar
   *
   * @return              Data output
   */
  public static List<Integer> vectorAdditionCpu(
      final int size, final List<Integer> firstVector,
      final List<Integer> secondVector, final int scalar) {
    List<Integer> dataOut = new ArrayList<Integer>();

    for (int i = 0; i < size; i++) {
      dataOut.add(firstVector.get(i) + secondVector.get(i) + scalar);
    }

    return dataOut;
  }

  /**
   * VectorAddition on DFE.
   *
   * @param size          Size
   * @param firstVector   First vector
   * @param secondVector  Second vector
   * @param scalar        Scalar
   *
   * @return              Data output
   */
  public static List<Integer> vectorAdditionDfe(
      final int size, final List<Integer> firstVector,
      final List<Integer> secondVector, final int scalar) {
    List<Integer> dataOut = new ArrayList<Integer>();
    double startTime = System.nanoTime();
    DecimalFormat timeFormat = new DecimalFormat("#0.00000");

    // Make socket
    TTransport transport = new TSocket("localhost", PORT);

    // Wrap in a protocol
    TProtocol protocol = new TBinaryProtocol(transport);

    // Create a client to use the protocol encoder
    VectorAdditionService.Client client =
        new VectorAdditionService.Client(protocol);

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
      final long maxfile = client.VectorAddition_init();
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
      final long addressFirstVector = client.malloc_int32_t(size);
      client.send_data_int32_t(addressFirstVector, firstVector);
      final long addressSecondVector = client.malloc_int32_t(size);
      client.send_data_int32_t(addressSecondVector, secondVector);

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Sending input data:\t\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Allocate memory for output stream on server
      startTime = System.nanoTime();
      final long addressDataOut = client.malloc_int32_t(size);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Allocating memory for output stream on server:\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Action writeLMem
      startTime = System.nanoTime();
      final int sizeBytes = size * SIZE_OF_INT_IN_BYTES;

      long actions = client.max_actions_init(maxfile, "writeLMem");
      client.max_set_param_uint64t(actions, "address", 0);
      client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
      client.max_queue_input(actions, "cpu_to_lmem",
                             addressFirstVector, sizeBytes);
      client.max_run(engine, actions);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Writing to LMem:\t\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Action default
      startTime = System.nanoTime();

      actions = client.max_actions_init(maxfile, "default");
      client.max_set_param_uint64t(actions, "N", size);
      client.max_set_param_uint64t(actions, "A", scalar);
      client.max_queue_input(actions, "y", addressSecondVector, sizeBytes);
      client.max_queue_output(actions, "s", addressDataOut, sizeBytes);

      client.max_run(engine, actions);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Vector addition time:\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Unload DFE
      startTime = System.nanoTime();
      client.max_unload(engine);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Unloading DFE:\t\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Get output stream from server
      startTime = System.nanoTime();
      dataOut = client.receive_data_int32_t(addressDataOut, size);
      System.out.println("Getting output stream:\t(size = "
                          + size * SIZE_OF_INT + " bit)\t"
                          + timeFormat.format(estimatedTime) + "s");

      // Free allocated memory for streams on server
      startTime = System.nanoTime();
      client.free(addressFirstVector);
      client.free(addressSecondVector);
      client.free(addressDataOut);
      client.free(actions);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Freeing allocated memory for streams on server:\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Free allocated maxfile data
      startTime = System.nanoTime();
      startTime = System.nanoTime();
      client.VectorAddition_free();
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
   * Calculates vectorAdditionDfe and vectorAdditionCpu and
   * checks if they return the same value.
   *
   * @param args Command line arguments
   */
  public static void main(final String[] args) {
    final int size = 384;
    final int scalar = 3;
    double startTime;
    double estimatedTime;
    DecimalFormat timeFormat = new DecimalFormat("#0.00000");

    // Generate input data
    startTime = System.nanoTime();
    final List<Integer> firstVector = randomData(size);
    final List<Integer> secondVector = randomData(size);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("Generating input data:\t\t\t\t"
                         + timeFormat.format(estimatedTime)
                         + "s");

    // DFE Output
    startTime = System.nanoTime();
    final List<Integer> dataOutDfe = vectorAdditionDfe(
        size, firstVector, secondVector, scalar);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("DFE vector addition total time:\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

    // CPU Output
    startTime = System.nanoTime();
    final List<Integer> dataOutCpu = vectorAdditionCpu(
        size, firstVector, secondVector, scalar);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("CPU vector addition total time:\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

    // Checking results
    startTime = System.nanoTime();
    int status = check(dataOutDfe, dataOutCpu, size);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("Checking results:\t\t\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");
    if (status == 0) {
      System.out.println("Test passed!");
    } else {
      System.out.println("Test failed " + status + " times!");
      System.exit(-1);
    }
  }
}

