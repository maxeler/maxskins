/**
 * Copyright 2016 Maxeler Technologies
 *
 * Document: MaxCompiler Tutorial (maxcompiler-tutorial.pdf)
 * Chapter: 13      Example: 2      Name: LMem Loopback
 * MaxFile name: LMemLoopback
 * Summary:
 *        Adds two LMem input streams and writes the result to LMem.
 */
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include <stdlib.h>

#include <vector>

#include "../gen-cpp/LMemLoopbackService.h"

using std::cout;
using std::endl;

using apache::thrift::TException;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TProtocol;
using apache::thrift::transport::TBufferedTransport;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;

using ::com::maxeler::LMemLoopback::LMemLoopbackServiceClient;
using ::com::maxeler::LMemLoopback::LMemLoopback_actions_t_struct;
using ::com::maxeler::LMemLoopback::LMemLoopback_readLMem_actions_t_struct;
using ::com::maxeler::LMemLoopback::LMemLoopback_writeLMem_actions_t_struct;
using ::com::maxeler::LMemLoopback::remote_ptr;

/**
 * Calculates how much time pasted from strartTime.
 *
 * @param startTime  Starting time
 *
 * @return           Time difference in seconds
 */
double calcTime(struct timeval startTime) {
  struct timeval endTime;

  gettimeofday(&endTime, NULL);

  double seconds  = endTime.tv_sec  - startTime.tv_sec;
  double useconds = endTime.tv_usec - startTime.tv_usec;

  return (((seconds) * 1000 + useconds / 1000.0) + 0.5) / 1000;
}

/**
 * Checks if LMemLoopbackDfe and LMemLoopbackCpu are the same.
 *
 * @param size       Size
 * @param outDataDfe Out data from DFE
 * @param outDataCpu Out data from CPU
 */
void check(std::vector<int32_t> outDataDFE,
           std::vector<int32_t> outDataCPU, int size) {
  for (int i = 0; i < size; i++) {
    if (outDataDFE[i] != outDataCPU[i]) {
      fprintf(stderr, "Output data @ %d = %d (expected %d)\n",
              i, outDataDFE[i], outDataCPU[i]);

      cout << "Test failed." << endl;
      exit(-1);
    }
  }

  cout << "Test passed!" << endl;
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
std::vector<int32_t> LMemLoopbackCPU(int size, std::vector<int32_t> inA,
                                     std::vector<int32_t> inB) {
  std::vector<int32_t> outDataCPU(size);

  for (int i = 0; i < size; i++) {
    outDataCPU[i] = inA[i] + inB[i];
  }

  return outDataCPU;
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
std::vector<int32_t> LMemLoopbackDFE(int size, std::vector<int32_t> inA,
                                     std::vector<int32_t> inB) {
  int sizeBytes = size * sizeof(int32_t);
  std::vector<int32_t> outData(size);

  struct timeval startTime;

  gettimeofday(&startTime, NULL);

  // Make socket
  boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

  // Buffering is critical. Raw sockets are very slow
  boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

  // Wrap in a protocol
  boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

  // Create a client to use the protocol encoder
  LMemLoopbackServiceClient client(protocol);

  cout << "Creating a client:\t\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  try {
    // Connect!
    gettimeofday(&startTime, NULL);
    transport->open();
    cout << "Opening connection:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Initialize maxfile
    gettimeofday(&startTime, NULL);
    remote_ptr maxfile = client.LMemLoopback_init();
    cout << "Initializing maxfile:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Load DFE
    gettimeofday(&startTime, NULL);
    remote_ptr engine = client.max_load(maxfile, "*");
    cout << "Loading DFE:\t\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Allocate and send input streams to server
    gettimeofday(&startTime, NULL);

    remote_ptr address_inA = client.malloc_int32_t(size);
    client.send_data_int32_t(address_inA, inA);

    remote_ptr address_inB = client.malloc_int32_t(size);
    client.send_data_int32_t(address_inB, inB);

    cout << "Sending input data:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Allocate memory for output stream on server
    gettimeofday(&startTime, NULL);
    remote_ptr address_outData = client.malloc_int32_t(size);
    cout << "Allocating memory for output stream on server:\t";
    cout << calcTime(startTime) << "s" << endl;

    // Write to LMem
    gettimeofday(&startTime, NULL);

    remote_ptr actions = client.max_actions_init(maxfile, "writeLMem");
    client.max_set_param_uint64t(actions, "address", 0);
    client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
    client.max_queue_input(actions, "cpu_to_lmem", address_inA, sizeBytes);

    client.max_run(engine, actions);

    actions = client.max_actions_init(maxfile, "writeLMem");
    client.max_set_param_uint64t(actions, "address", sizeBytes);
    client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
    client.max_queue_input(actions, "cpu_to_lmem", address_inB, sizeBytes);

    client.max_run(engine, actions);

    cout << "Writing to LMem:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Action default
    gettimeofday(&startTime, NULL);

    actions = client.max_actions_init(maxfile, "default");
    client.max_set_param_uint64t(actions, "N", size);

    client.max_run(engine, actions);

    cout << "LMemLoopback time:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Reading from LMem
    gettimeofday(&startTime, NULL);

    actions = client.max_actions_init(maxfile, "readLMem");
    client.max_set_param_uint64t(actions, "address", 2 * sizeBytes);
    client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
    client.max_queue_output(actions, "lmem_to_cpu", address_outData,
                            sizeBytes);

    client.max_run(engine, actions);

    cout << "Reading from LMem:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Unload DFE
    gettimeofday(&startTime, NULL);
    client.max_unload(engine);
    cout << "Unloading DFE:\t\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Get output stream from server
    gettimeofday(&startTime, NULL);
    client.receive_data_int32_t(outData, address_outData, size);
    cout << "Getting output stream:\t(size = " << size * 32 << " bit)\t";
    cout << calcTime(startTime) << "s" << endl;

    // Free allocated memory for streams on server
    gettimeofday(&startTime, NULL);
    client.free(address_inA);
    client.free(address_inB);
    client.free(address_outData);
    client.free(actions);
    cout << "Freeing allocated memory for streams on server:\t";
    cout << calcTime(startTime) << "s" << endl;

    // Free allocated maxfile data
    gettimeofday(&startTime, NULL);
    client.LMemLoopback_free();
    cout << "Freeing allocated maxfile data:\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Close!
    gettimeofday(&startTime, NULL);
    transport->close();
    cout << "Closing connection:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;
  } catch (TException& tx) {
    cout << "ERROR: " << tx.what() << endl;
    exit(-1);
  }

  return outData;
}

/**
 * Calculates LMemLoopbackDFE and LMemLoopbackCPU and
 * checks if they return the same value.
 */
int main() {
  struct timeval startTime;

  // Input
  gettimeofday(&startTime, NULL);

  const int size = 384;
  std::vector<int32_t> inA(size);
  std::vector<int32_t> inB(size);

  for (int i = 0; i < size; i++) {
    inA[i] = i;
    inB[i] = size - i;
  }

  cout << "Generating input data:\t\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // DFE Output
  gettimeofday(&startTime, NULL);
  std::vector<int32_t> outDataDFE = LMemLoopbackDFE(size, inA, inB);
  cout << "DFE LMemLoopback total time:\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // CPU Output
  gettimeofday(&startTime, NULL);
  std::vector<int32_t> outDataCPU = LMemLoopbackCPU(size, inA, inB);
  cout << "CPU LMemLoopback total time:\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // Checking results
  check(outDataCPU, outDataDFE, size);

  return 0;
}
