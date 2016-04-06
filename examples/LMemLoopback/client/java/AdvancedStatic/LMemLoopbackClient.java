import com.maxeler.LMemLoopback.LMemLoopbackService;
import com.maxeler.LMemLoopback.LMemLoopback_actions_t_struct;
import com.maxeler.LMemLoopback.LMemLoopback_readLMem_actions_t_struct;
import com.maxeler.LMemLoopback.LMemLoopback_writeLMem_actions_t_struct;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

/** LMemLoopback AdvancedStatic example. */
public final class LMemLoopbackClient {

  /** Server port. */
  private static final int PORT = 9090;

  /** Size of int in bytes. */
  private static final int SIZE_OF_INT_IN_BYTES = 4;

  /** Size of int in bits. */
  private static final int SIZE_OF_INT = 32;

  /** Number of nano seconds in one second. */
  private static final int NUM_OF_NANO_SECONDS = 1000000000;

  /** Utility classes should not have a public or default constructor. */
  private LMemLoopbackClient() { }

  /**
   * Checks if lmemLoopbackDfe and lmemLoopbackCpu are the same.
   *
   * @param size       Size
   * @param dataOutDfe Out data from DFE
   * @param dataOutCpu Out data from CPU
   *
   * @return status Number of elements that doesn't match
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
   * LMemLoopback on CPU. outData = inA + inB
   *
   * @param size    Size
   * @param inA     In A
   * @param inB     In B
   *
   * @return        Out data
   */
  public static List<Integer> lmemLoopbackCpu(
      final int size, final List<Integer> inA, final List<Integer> inB) {
    List<Integer> outData = new ArrayList<Integer>();

    for (int i = 0; i < size; i++) {
      outData.add(inA.get(i) + inB.get(i));
    }

    return outData;
  }

  /**
   * LMemLoopback on DFE. outData = inA + inB
   *
   * @param size    Size
   * @param inA     In A
   * @param inB     In B
   *
   * @return        Out data
   */
  public static List<Integer> lmemLoopbackDfe(
      final int size, final List<Integer> inA, final List<Integer> inB) {
    List<Integer> outData = new ArrayList<Integer>();
    DecimalFormat timeFormat = new DecimalFormat("#0.00000");

    double startTime = System.nanoTime();
    double estimatedTime;
    int sizeBytes = size * SIZE_OF_INT_IN_BYTES;

    // Make socket
    TTransport transport;
    transport = new TSocket("localhost", PORT);

    // Wrap in a protocol
    TProtocol protocol = new  TBinaryProtocol(transport);

    // Create a client to use the protocol encoder
    LMemLoopbackService.Client client =
        new LMemLoopbackService.Client(protocol);

    estimatedTime = (System.nanoTime() - startTime)
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
      final long maxfile = client.LMemLoopback_init();
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
      final long addressInA = client.malloc_int32_t(size);
      client.send_data_int32_t(addressInA, inA);

      final long addressInB = client.malloc_int32_t(size);
      client.send_data_int32_t(addressInB, inB);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Sending input data:\t\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Allocate memory for output stream on server
      startTime = System.nanoTime();
      final long addressOutData = client.malloc_int32_t(size);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Allocating memory for output stream on server:\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Write to LMem
      startTime = System.nanoTime();
      LMemLoopback_writeLMem_actions_t_struct actionsLmemWrite =
          new LMemLoopback_writeLMem_actions_t_struct();

      actionsLmemWrite.setParam_address(0);
      actionsLmemWrite.setParam_nbytes(sizeBytes);
      actionsLmemWrite.setInstream_cpu_to_lmem(addressInA);

      long addressActionsLmemWrite =
          client.send_LMemLoopback_writeLMem_actions_t(actionsLmemWrite);
      client.LMemLoopback_writeLMem_run(engine, addressActionsLmemWrite);

      actionsLmemWrite.setParam_address(sizeBytes);
      actionsLmemWrite.setParam_nbytes(sizeBytes);
      actionsLmemWrite.setInstream_cpu_to_lmem(addressInB);

      addressActionsLmemWrite =
          client.send_LMemLoopback_writeLMem_actions_t(actionsLmemWrite);
      client.LMemLoopback_writeLMem_run(engine, addressActionsLmemWrite);

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Writing to LMem:\t\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Executing action
      startTime = System.nanoTime();
      LMemLoopback_actions_t_struct actions =
          new LMemLoopback_actions_t_struct();

      actions.setParam_N(size);

      final long addressActions = client.send_LMemLoopback_actions_t(actions);
      client.LMemLoopback_run(engine, addressActions);

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("LMemLoopback time:\t\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Read from LMem
      startTime = System.nanoTime();
      LMemLoopback_readLMem_actions_t_struct actionsLmemRead =
          new LMemLoopback_readLMem_actions_t_struct();

      actionsLmemRead.setParam_address(2 * sizeBytes);
      actionsLmemRead.setParam_nbytes(sizeBytes);
      actionsLmemRead.setOutstream_lmem_to_cpu(addressOutData);

      final long addressactionsLmemRead =
          client.send_LMemLoopback_readLMem_actions_t(actionsLmemRead);
      client.LMemLoopback_readLMem_run(engine, addressactionsLmemRead);

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Reading from LMem:\t\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Unload DFE
      startTime = System.nanoTime();
      client.max_unload(engine);

      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Unloading DFE:\t\t\t\t\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Get output stream from server
      startTime = System.nanoTime();
      outData = client.receive_data_int32_t(addressOutData, size);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Getting output stream:\t(size = "
                          + size * SIZE_OF_INT + " bit)\t"
                          + timeFormat.format(estimatedTime) + "s");

      // Free allocated memory for streams on server
      startTime = System.nanoTime();
      client.free(addressInA);
      client.free(addressInB);
      client.free(addressOutData);
      client.free(addressActionsLmemWrite);
      client.free(addressActions);
      client.free(addressactionsLmemRead);
      estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
      System.out.println("Freeing allocated memory for streams on server:\t"
                           + timeFormat.format(estimatedTime) + "s");

      // Free allocated maxfile data
      startTime = System.nanoTime();
      client.LMemLoopback_free();
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

    return outData;
  }

  /**
   * Calculates lmemLoopbackDfe and lmemLoopbackCpu and
   * checks if they return the same value.
   *
   * @param args Command line arguments
   */
  public static void main(final String[] args) {

    final int size = 384;
    double startTime = System.nanoTime();
    double estimatedTime;
    final DecimalFormat timeFormat = new DecimalFormat("#0.00000");

    // Generate data
    startTime = System.nanoTime();
    List<Integer> inA = new ArrayList<Integer>();
    List<Integer> inB = new ArrayList<Integer>();

    for (int i = 0; i < size; i++) {
      inA.add(i);
      inB.add(size - i);
    }
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("Generating input data:\t\t\t\t"
                         + timeFormat.format(estimatedTime)
                         + "s");
    // DFE Output
    startTime = System.nanoTime();
    final List<Integer> dataOutDfe = lmemLoopbackDfe(size, inA, inB);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("DFE LMemLoopback total time:\t\t\t"
                         + timeFormat.format(estimatedTime) + "s");

    // CPU Output
    startTime = System.nanoTime();
    final List<Integer> dataOutCpu = lmemLoopbackCpu(size, inA, inB);
    estimatedTime = (System.nanoTime() - startTime) / NUM_OF_NANO_SECONDS;
    System.out.println("CPU LMemLoopback total time:\t\t\t"
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

