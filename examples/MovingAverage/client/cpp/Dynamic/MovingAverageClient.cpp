/* Copyright 2015 Maxeler Technologies */
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include <stdlib.h>
#include <sys/time.h>

#include <vector>

#include "../gen-cpp/MovingAverageService.h"

using std::cout;
using std::endl;

using apache::thrift::TException;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TProtocol;
using apache::thrift::transport::TBufferedTransport;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;

using ::com::maxeler::MovingAverage::MovingAverageServiceClient;
using ::com::maxeler::MovingAverage::remote_ptr;

double calcTime(struct timeval startTime) {
  struct timeval endTime;

  gettimeofday(&endTime, NULL);

  double seconds  = endTime.tv_sec  - startTime.tv_sec;
  double useconds = endTime.tv_usec - startTime.tv_usec;

  return (((seconds) * 1000 + useconds/1000.0) + 0.5) / 1000;
}

/**
 * Generates random data.
 *
 * @param size    Size
 *
 * @return        Random data
 */
std::vector<double> randomData(int size) {
  std::vector<double> dataIn;

  for (int i = 0; i < size; i++) {
    dataIn.push_back(random() % 100);
  }

  return dataIn;
}

/**
 * Checks if MovingAverageDFE and MovingAverageCPU return the same value.
 *
 * @param dataOutDfe  Data output from DFE
 * @param dataOutCpu  Data output from CPU
 * @param size        Size
 */
void check(std::vector<double> dataOutCPU,
           std::vector<double> dataOutDFE, int size) {
  for (int i = 1; i < size - 1; i++) {
    if (dataOutDFE[i] != dataOutCPU[i]) {
      cout << "Output data @ " << i << " = " << dataOutDFE[i];
      cout << " (expected " << dataOutCPU[i] << ")" << endl;

      cout << "Test failed!" << endl;
      exit(-1);
    }
  }

  cout << "Test successful!" << endl;
}

/**
 * MovingAverage on CPU.
 *
 * @param size    Size
 * @param dataIn  Data input
 *
 * @return        Data output
 */
std::vector<double> MovingAverageCPU(std::vector<double> dataIn, int size) {
  std::vector<double> dataOut(size);

  for (int i = 1; i < size - 1; i++) {
    float x = dataIn[i - 1];
    float y = dataIn[i];
    float z = dataIn[i + 1];
    float tmp = (x + y + z) / 3;
    dataOut[i] = tmp;
  }

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
std::vector<double> MovingAverageDFE(std::vector<double> dataIn, int size) {
  struct timeval startTime;
  gettimeofday(&startTime, NULL);

  // Make socket
  boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

  // Buffering is critical. Raw sockets are very slow
  boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

  // Wrap in a protocol
  boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

  // Create a client to use the protocol encoder
  MovingAverageServiceClient client(protocol);

  double currentTime = calcTime(startTime);
  cout << "Creating a client:\t\t\t\t" << currentTime << "s" << endl;

  std::vector<double> dataOut(size);

  try {
    // Connect!
    gettimeofday(&startTime, NULL);
    transport->open();
    currentTime = calcTime(startTime);
    cout << "Opening connection:\t\t\t\t" << currentTime << "s" << endl;

    // Initialize maxfile
    gettimeofday(&startTime, NULL);
    remote_ptr maxfile = client.MovingAverage_init();
    currentTime = calcTime(startTime);
    cout << "Initializing maxfile:\t\t\t\t" << currentTime << "s" << endl;

    // Load DFE
    gettimeofday(&startTime, NULL);
    remote_ptr engine = client.max_load(maxfile, "*");
    currentTime = calcTime(startTime);
    cout << "Loading DFE:\t\t\t\t\t" << currentTime << "s" << endl;

    // Allocate and send input streams to server
    gettimeofday(&startTime, NULL);
    remote_ptr address_dataIn = client.malloc_float(size);
    client.send_data_float(address_dataIn, dataIn);
    currentTime = calcTime(startTime);
    cout << "Sending input data:\t\t\t\t" << currentTime << "s" << endl;

    // Allocate memory for output stream on server
    gettimeofday(&startTime, NULL);
    remote_ptr address_dataOut = client.malloc_float(size);
    currentTime = calcTime(startTime);
    cout << "Allocating memory for output stream on server:\t";
    cout << currentTime << "s" << endl;

    // Action default
    gettimeofday(&startTime, NULL);
    int sizeBytes = size * sizeof(float);
    remote_ptr actions = client.max_actions_init(maxfile, "default");
    client.max_set_param_uint64t(actions, "N", size);
    client.max_queue_input(actions, "x", address_dataIn, sizeBytes);
    client.max_queue_output(actions, "y", address_dataOut, sizeBytes);
    client.max_run(engine, actions);
    currentTime = calcTime(startTime);
    cout << "Moving average time:\t\t\t\t" << currentTime << "s" << endl;

    // Unload DFE
    gettimeofday(&startTime, NULL);
    client.max_unload(engine);
    currentTime = calcTime(startTime);
    cout << "Unloading DFE:\t\t\t\t\t" << currentTime << "s" << endl;

    // Get output stream from server
    gettimeofday(&startTime, NULL);
    client.receive_data_float(dataOut, address_dataOut, size);
    currentTime = calcTime(startTime);
    cout << "Getting output stream:\t(size = " << size * 32 << " bit)\t";
    cout << currentTime << "s" << endl;

    // Free allocated memory for streams on server
    gettimeofday(&startTime, NULL);
    client.free(address_dataIn);
    client.free(address_dataOut);
    client.free(actions);
    currentTime = calcTime(startTime);
    cout << "Freeing allocated memory for streams on server:\t";
    cout << currentTime << "s" << endl;

    // Free allocated maxfile data
    gettimeofday(&startTime, NULL);
    client.MovingAverage_free();
    currentTime = calcTime(startTime);
    cout << "Freeing allocated maxfile data:\t\t\t";
    cout << currentTime << "s" << endl;

    // Close!
    gettimeofday(&startTime, NULL);
    transport->close();
    currentTime = calcTime(startTime);
    cout << "Closing connection:\t\t\t\t" << currentTime << "s" << endl;
  } catch (TException& tx) {
    cout << "ERROR: " << tx.what() << endl;
    exit(-1);
  }

  return dataOut;
}

/**
 * Calculates MovingAverageDFE and MovingAverageCPU and
 * checks if they return the same value.
 */
int main() {
  struct timeval startTime;

  // Generate input data
  gettimeofday(&startTime, NULL);
  const int size = 384;
  std::vector<double> dataIn = randomData(size);
  double currentTime = calcTime(startTime);
  cout << "Generating input data:\t\t\t\t" << currentTime << "s" << endl;

  // DFE Output
  gettimeofday(&startTime, NULL);
  std::vector<double> dataOutDFE = MovingAverageDFE(dataIn, size);
  currentTime = calcTime(startTime);
  cout << "DFE moving average total time:\t\t\t" << currentTime << "s" << endl;

  // CPU Output
  gettimeofday(&startTime, NULL);
  std::vector<double> dataOutCPU = MovingAverageCPU(dataIn, size);
  currentTime = calcTime(startTime);
  cout << "CPU moving average total time:\t\t\t" << currentTime << "s" << endl;

  // Checking results
  check(dataOutCPU, dataOutDFE, size);

  return 0;
}

