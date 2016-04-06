/* Copyright 2016 Maxeler Technologies */
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include <stdlib.h>

#include <vector>

#include "../gen-cpp/VectorAdditionService.h"

using std::cout;
using std::endl;

using apache::thrift::TException;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TProtocol;
using apache::thrift::transport::TBufferedTransport;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;

using ::com::maxeler::VectorAddition::VectorAdditionServiceClient;
using ::com::maxeler::VectorAddition::remote_ptr;

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
 * Generates random data.
 *
 * @param size    Size
 *
 * @return        Random data
 */
std::vector<int32_t> randomData(int size) {
  std::vector<int32_t> dataIn;

  for (int i = 0; i < size; i++) {
    dataIn.push_back(random() % 100);
  }

  return dataIn;
}

/**
 * Checks if VectorAdditionDFE and VectorAdditionCPU return the same value.
 *
 * @param dataOutDfe  Data output from DFE
 * @param dataOutCpu  Data output from CPU
 * @param size        Size
 */
void check(std::vector<int32_t> dataOutDFE,
           std::vector<int32_t> dataOutCPU, int size) {
  for (int i = 0; i < size; i++) {
    if (dataOutDFE[i] != dataOutCPU[i]) {
      fprintf(stderr, "Output data @ %d = %d (expected %d)\n",
              i, dataOutDFE[i], dataOutCPU[i]);

      cout << "Test failed." << endl;
      exit(-1);
    }
  }

  cout << "Test passed!" << endl;
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
std::vector<int32_t> VectorAdditionCPU(int size, std::vector<int32_t> x,
                                       std::vector<int32_t> y, int scalar) {
  std::vector<int32_t> dataOut(size);

  for (int i = 0 ; i < size ; i++) {
    dataOut[i] = x[i] + y[i] + scalar;
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
std::vector<int32_t> VectorAdditionDFE(int size, std::vector<int32_t> x,
                                       std::vector<int32_t> y, int scalar) {
  int sizeBytes = size * sizeof(int32_t);
  std::vector<int32_t> dataOut(size);

  struct timeval startTime;

  gettimeofday(&startTime, NULL);

  // Make socket
  boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

  // Buffering is critical. Raw sockets are very slow
  boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

  // Wrap in a protocol
  boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

  // Create a client to use the protocol encoder
  VectorAdditionServiceClient client(protocol);

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
    remote_ptr maxfile = client.VectorAddition_init();
    cout << "Initializing maxfile:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Load DFE
    gettimeofday(&startTime, NULL);
    remote_ptr engine = client.max_load(maxfile, "*");
    cout << "Loading DFE:\t\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Allocate and send input streams to server
    gettimeofday(&startTime, NULL);

    remote_ptr address_x = client.malloc_int32_t(size);
    client.send_data_int32_t(address_x, x);

    remote_ptr address_y = client.malloc_int32_t(size);
    client.send_data_int32_t(address_y, y);

    cout << "Sending input data:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Allocate memory for output stream on server
    gettimeofday(&startTime, NULL);
    remote_ptr address_dataOut = client.malloc_int32_t(size);
    cout << "Allocating memory for output stream on server:\t";
    cout << calcTime(startTime) << "s" << endl;

    // Write to LMem
    gettimeofday(&startTime, NULL);

    remote_ptr act = client.max_actions_init(maxfile, "writeLMem");
    client.max_set_param_uint64t(act, "address", 0);
    client.max_set_param_uint64t(act, "nbytes", sizeBytes);
    client.max_queue_input(act, "cpu_to_lmem", address_x, sizeBytes);
    client.max_run(engine, act);

    cout << "Writing to LMem:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Action default
    gettimeofday(&startTime, NULL);

    act = client.max_actions_init(maxfile, "default");
    client.max_set_param_uint64t(act, "N", size);
    client.max_set_param_uint64t(act, "A", scalar);
    client.max_queue_input(act, "y", address_y, sizeBytes);
    client.max_queue_output(act, "s", address_dataOut, sizeBytes);
    client.max_run(engine, act);

    cout << "Vector addition time:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Unload DFE
    gettimeofday(&startTime, NULL);
    client.max_unload(engine);
    cout << "Unloading DFE:\t\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Get output stream from server
    gettimeofday(&startTime, NULL);
    client.receive_data_int32_t(dataOut, address_dataOut, size);
    cout << "Getting output stream:\t(size = " << size * 32 << " bit)\t";
    cout << calcTime(startTime) << "s" << endl;

    // Free allocated memory for streams on server
    gettimeofday(&startTime, NULL);
    client.free(address_x);
    client.free(address_y);
    client.free(address_dataOut);
    client.free(act);
    cout << "Freeing allocated memory for streams on server:\t";
    cout << calcTime(startTime) << "s" << endl;

    // Free allocated maxfile data
    gettimeofday(&startTime, NULL);
    client.VectorAddition_free();
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

  return dataOut;
}

/**
 * Calculates VectorAdditionDFE and VectorAdditionCPU and
 * checks if they return the same value.
 */
int main() {
  struct timeval startTime;

  // Input
  gettimeofday(&startTime, NULL);

  const int size = 384;

  std::vector<int32_t> x = randomData(size);
  std::vector<int32_t> y = randomData(size);
  int scalar = 3;

  cout << "Generating input data:\t\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // DFE Output
  gettimeofday(&startTime, NULL);
  std::vector<int32_t> dataOutDFE = VectorAdditionDFE(size, x, y, scalar);
  cout << "DFE vector addition total time:\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // CPU Output
  gettimeofday(&startTime, NULL);
  std::vector<int32_t> dataOutCPU = VectorAdditionCPU(size, x, y, scalar);
  cout << "CPU vector addition time:\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // Checking results
  check(dataOutDFE, dataOutCPU, size);

  return 0;
}

