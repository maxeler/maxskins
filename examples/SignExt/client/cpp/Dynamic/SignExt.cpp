/* Copyright 2015 Maxeler Technologies */
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include <stdlib.h>
#include <math.h>
#include <unistd.h>

#include <vector>

#include "../gen-cpp/SignExtService.h"

using std::cout;
using std::endl;
using std::dec;
using std::hex;

using apache::thrift::TException;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TProtocol;
using apache::thrift::transport::TBufferedTransport;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;

using ::com::maxeler::SignExt::max_config_key_bool_t_enum;
using ::com::maxeler::SignExt::max_config_key_bool_t_struct;
using ::com::maxeler::SignExt::max_net_connection_t_enum;
using ::com::maxeler::SignExt::max_net_connection_t_struct;
using ::com::maxeler::SignExt::remote_ptr;
using ::com::maxeler::SignExt::SignExtServiceClient;

int main(int argc, char *argv[]) {
  // Make socket
  boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

  // Buffering is critical. Raw sockets are very slow
  boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

  // Wrap in a protocol
  boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

  // Create a client to use the protocol encoder
  SignExtServiceClient client(protocol);

  try {
    // Connect!
    transport->open();

    if (argc != 3) {
      cout << "Usage: $0 dfe_ip remote_ip" << endl;
      exit(-1);
    }

    remote_ptr dfe_ip_address = client.malloc_int64_t(5);
    client.inet_aton(argv[1], dfe_ip_address);

    remote_ptr remote_ip_address = client.malloc_int64_t(5);
    client.inet_aton(argv[2], remote_ip_address);

    remote_ptr netmask_address = client.malloc_int64_t(5);
    client.inet_aton("255.255.255.0", netmask_address);

    unsigned short port = 2000;

    // Initialize maxfile
    remote_ptr maxfile = client.SignExt_init();

    // Load DFE
    remote_ptr engine = client.max_load(maxfile, "*");

    max_config_key_bool_t_struct enumkey = max_config_key_bool_t_struct();
    enumkey.__set_type(
        max_config_key_bool_t_enum::MAX_CONFIG_PRINTF_TO_STDOUT);
    client.max_config_set_bool(enumkey, 1);

    remote_ptr actions = client.max_actions_init(maxfile, "default");

    client.max_run(engine, actions);
    client.max_actions_free(actions);

    remote_ptr buffer_address = client.malloc_int64_t(1);
    int bufferSize = 4096 * 512;
    client.posix_memalign(buffer_address, 4096, bufferSize);

    std::vector<int64_t> buffer_vec;

    client.receive_data_int64_t(buffer_vec, buffer_address, 1);
    int64_t buffer = buffer_vec[0];

    remote_ptr toCpu = client.max_framed_stream_setup(engine, "toCPU",
                                                      buffer, bufferSize, -1);

    max_net_connection_t_struct enumconn = max_net_connection_t_struct();
    enumconn.__set_type(
        max_net_connection_t_enum::MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1);
    client.max_ip_config(engine, enumconn, dfe_ip_address, netmask_address);
    remote_ptr dfe_socket =
        client.max_udp_create_socket(engine, "udpTopPort1");
    client.max_udp_bind(dfe_socket, port);
    client.max_udp_connect(dfe_socket, remote_ip_address, 0);

    cout <<  "Listening on " << dec << argv[1];
    cout << " port " << dec << port << endl;

    cout << "Waiting for kernel response..." << endl;

    remote_ptr f_address = client.malloc_int64_t(1);
    remote_ptr fsz_address = client.malloc_int64_t(1);
    int numMessageRx = 0;
    bool cond = true;

    while (cond) {
      if (client.max_framed_stream_read(toCpu, 1, f_address,
                                        fsz_address) == 1) {
        numMessageRx += 1;

        std::vector<int64_t> fsz_vec;
        client.receive_data_int64_t(fsz_vec, fsz_address, 1);
        int64_t fsz = fsz_vec[0];

        cout << "CPU: Got output frame " << dec << numMessageRx;
        cout << " - size " << dec << fsz << " bytes" << endl;

        std::vector<int64_t> f_vec;
        client.receive_data_int64_t(f_vec, f_address, 1);
        int64_t f = f_vec[0];

        std::vector<int64_t> w;
        client.receive_data_int64_t(w, f, 3);

        for (int i = 0; i < 3; i++) {
          int64_t wp;
          if (w[i] < 0) {
            wp = w[i] + (int64_t)pow(2, 32) * (int64_t)pow(2, 32);
          } else {
            wp = w[i];
          }
          cout << "Frame [" << dec << numMessageRx << "]";
          cout << " Word[" << dec << i << "]: 0x" << hex << wp << endl;
        }

        client.max_framed_stream_discard(toCpu, 1);

        if (w[0] == 0 && w[1] == 0 && w[2] == 0) {
          cond = false;
        }
      } else {
        usleep(10);
      }
    }

    client.max_udp_close(dfe_socket);
    client.max_framed_stream_release(toCpu);
    client.max_unload(engine);
    client.max_file_free(maxfile);
    client.free(dfe_ip_address);
    client.free(remote_ip_address);
    client.free(netmask_address);
    client.free(buffer_address);
    client.free(f_address);
    client.free(fsz_address);
    client.SignExt_free();

    cout << "Done." << endl;

    // Close!
    transport->close();
    exit(0);
  } catch (TException& thrift_exceptiion) {
    cout << "ERROR: " << thrift_exceptiion.what() << endl;
    exit(-1);
  }

  return 0;
}
