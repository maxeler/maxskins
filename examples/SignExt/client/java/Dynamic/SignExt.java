import com.maxeler.SignExt.SignExtService;
import com.maxeler.SignExt.max_config_key_bool_t_enum;
import com.maxeler.SignExt.max_config_key_bool_t_struct;
import com.maxeler.SignExt.max_net_connection_t_enum;
import com.maxeler.SignExt.max_net_connection_t_struct;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

import java.util.List;

/** SignExt Dynamic example. */
public final class SignExt {

  /** Package size. */
  private static final int PACKAGE_SIZE = 3;

  /** Buffer size. */
  private static final int BUFFER_SIZE = 4096 * 512;

  /** Buffer aligment. */
  private static final int BUFFER_ALIGMENT = 4096;

  /** Thread sleep time. */
  private static final int THREAD_SLEEP_TIME = 1;

  /** Size of int in bits. */
  private static final int SIZE_OF_INT = 32;

  /** Server port. */
  private static final int PORT = 9090;

  /** Utility classes should not have a public or default constructor. */
  private SignExt() { }

  /**
   * An example project, which will receive packets containing
   * 3 signed integer values of variable lengths,
   * convert them to a fixed size and pass them to the CPU.
   * Since the values are signed, we must consider sign extension.
   * The structure of the incoming packet data field should be as follows:
   *     First byte:
   *         - defines the size of the following content (see table below)
   *     Rest of data:
   *         - contains the three values A, B and C (in that order)
   *         - where n = size of A + size of B + size of C
   *         - little endian
   * Usage: $0 dfe_ip remote_ip
   *
   * @param args Command line arguments
   */
  public static void main(final String[] args) {
    if (args.length != 2) {
      System.out.println("Usage: $0 dfe_ip remote_ip");
      System.exit(-1);
    }

    // Make socket
    TTransport transport = new TSocket("localhost", PORT);

    // Wrap in a protocol
    TProtocol protocol = new TBinaryProtocol(transport);

    // Create a client to use the protocol encoder
    SignExtService.Client client = new SignExtService.Client(protocol);

    try {
      // Connect!
      transport.open();

      final long dfeIpAddress = client.malloc_int64_t(5);
      client.inet_aton(args[0], dfeIpAddress);

      final long remoteIpAddress = client.malloc_int64_t(5);
      client.inet_aton(args[1], remoteIpAddress);

      final long netmaskAddress = client.malloc_int64_t(5);
      client.inet_aton("255.255.255.0", netmaskAddress);

      final short port = 2000;

      // Initialize maxfile
      final long maxfile = client.SignExt_init();

      // Load DFE
      final long engine = client.max_load(maxfile, "*");

      max_config_key_bool_t_struct enumKey =
          new max_config_key_bool_t_struct(max_config_key_bool_t_enum
                                           .MAX_CONFIG_PRINTF_TO_STDOUT);
      client.max_config_set_bool(enumKey, 1);

      final long actions = client.max_actions_init(maxfile, "default");

      client.max_run(engine, actions);
      client.max_actions_free(actions);

      long bufferAddress = client.malloc_int64_t(1);

      client.posix_memalign(bufferAddress, BUFFER_ALIGMENT, BUFFER_SIZE);

      long buffer = client.receive_data_int64_t(bufferAddress, 1).get(0);

      long toCpu = client.max_framed_stream_setup(engine, "toCPU",
                                                  buffer, BUFFER_SIZE, -1);

      max_net_connection_t_struct enumconn =
          new max_net_connection_t_struct(
              max_net_connection_t_enum.MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1);
      client.max_ip_config(engine, enumconn, dfeIpAddress, netmaskAddress);
      long dfeSocket = client.max_udp_create_socket(engine, "udpTopPort1");
      client.max_udp_bind(dfeSocket, port);
      client.max_udp_connect(dfeSocket, remoteIpAddress, (short) 0);

      System.out.println("Listening on " + args[0] + " port " + port);

      System.out.println("Waiting for kernel response...");

      long frameAddress = client.malloc_int64_t(1);
      long fszAddress = client.malloc_int64_t(1);
      int numMessageRx = 0;
      boolean cond = true;

      while (cond) {
        if (client.max_framed_stream_read(toCpu, 1,
                                          frameAddress, fszAddress) == 1) {
          numMessageRx += 1;

          long fsz = client.receive_data_int64_t(fszAddress, 1).get(0);
          System.out.println("CPU: Got output frame " + numMessageRx
                             + " - size " + fsz + " bytes");

          long frame = client.receive_data_int64_t(frameAddress, 1).get(0);

          List<Long> word = client.receive_data_int64_t(frame, PACKAGE_SIZE);

          for (int i = 0; i < PACKAGE_SIZE; i++) {
            String wp;
            if (word.get(i) < 0) {
              wp = Long.toHexString(
                  word.get(i) + (long) Math.pow(2, SIZE_OF_INT)
                  * (long) Math.pow(2, SIZE_OF_INT)).toUpperCase();
            } else {
              wp = Long.toHexString(word.get(i)).toUpperCase();
            }
            System.out.println("Frame [" + numMessageRx
                               + "] Word[" + i + "]: 0x" + wp);
          }

          client.max_framed_stream_discard(toCpu, 1);

          if (word.get(0) == 0 && word.get(1) == 0 && word.get(2) == 0) {
            cond = false;
          }

        } else {
          Thread.sleep(THREAD_SLEEP_TIME);
        }
      }

      // Free allocated memory for streams on server
      client.max_udp_close(dfeSocket);
      client.max_framed_stream_release(toCpu);
      client.max_unload(engine);
      client.max_file_free(maxfile);
      client.free(dfeIpAddress);
      client.free(remoteIpAddress);
      client.free(netmaskAddress);
      client.free(bufferAddress);
      client.free(frameAddress);
      client.free(fszAddress);

      // Free allocated maxfile data
      client.SignExt_free();

      System.out.println("Done.");

      // Close!
      transport.close();

    } catch (TException x) {
      x.printStackTrace();
      System.exit(-1);
    } catch (InterruptedException x) {
      x.printStackTrace();
      System.exit(-1);
    }
  }
}
