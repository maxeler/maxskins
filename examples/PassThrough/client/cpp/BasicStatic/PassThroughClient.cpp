/* Copyright 2016 Maxeler Technologies */
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include <stdlib.h>

#include <vector>

#include "../gen-cpp/PassThroughService.h"

using std::cout;
using std::endl;

using apache::thrift::TException;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TProtocol;
using apache::thrift::transport::TBufferedTransport;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;

using ::com::maxeler::PassThrough::PassThroughServiceClient;
using ::com::maxeler::PassThrough::remote_ptr;



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
 * Checks if PassThroughDFE and PassThroughCPU return the same value.
 *
 * @param dataOutDfe  Data output from DFE
 * @param dataOutCpu  Data output from CPU
 * @param size        Size
 */
void check(std::vector<int32_t> dataOutDFE,
           std::vector<int32_t> dataOutCPU, int size) {
  for (int i = 0; i < size; i++) {
    if (dataOutDFE[i] != dataOutCPU[i]) {
      fprintf(stderr, "Output data @ %d = %1.8g (expected %1.8g)\n",
              i, dataOutDFE[i], dataOutCPU[i]);

      cout << "Test failed." << endl;
      exit(-1);
    }
  }

  cout << "Test passed!" << endl;
}

/**
 * PassThrough on CPU.
 *
 * @param size    Size
 * @param dataIn  Data input
 *
 * @return        Data output
 */
std::vector<int32_t> PassThroughCPU(int size, std::vector<int32_t> dataIn) {
  std::vector<int32_t> dataOut(size);

  for (int i = 0 ; i < size ; i++) {
    dataOut[i] = dataIn[i];
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
std::vector<int32_t> PassThroughDFE(int size, std::vector<int32_t> dataIn) {
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
  PassThroughServiceClient client(protocol);

  cout << "Creating a client:\t\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  try {
    // Connect!
    gettimeofday(&startTime, NULL);
    transport->open();
    cout << "Opening connection:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Allocate and send input streams to server
    gettimeofday(&startTime, NULL);
    remote_ptr address_dataIn = client.malloc_int32_t(size);
    client.send_data_int32_t(address_dataIn, dataIn);
    cout << "Sending input data:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Allocate memory for output stream on server
    gettimeofday(&startTime, NULL);
    remote_ptr address_dataOut = client.malloc_int32_t(size);
    cout << "Allocating memory for output stream on server:\t";
    cout << calcTime(startTime) << "s" << endl;

    // Action default
    gettimeofday(&startTime, NULL);

    client.PassThrough(size, address_dataIn, address_dataOut);

    cout << "Pass through time:\t\t\t\t";
    cout << calcTime(startTime) << "s" << endl;

    // Get output stream from server
    gettimeofday(&startTime, NULL);
    client.receive_data_int32_t(dataOut, address_dataOut, size);
    cout << "Getting output stream:\t(size = " << size * 32 << " bit)\t";
    cout << calcTime(startTime) << "s" << endl;

    // Free allocated memory for streams on server
    gettimeofday(&startTime, NULL);
    client.free(address_dataIn);
    client.free(address_dataOut);
    cout << "Freeing allocated memory for streams on server:\t";
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
 * Calculates PassThroughDFE and PassThroughCPU and
 * checks if they return the same value.
 */
int main() {
  struct timeval startTime;

  // Input
  gettimeofday(&startTime, NULL);

  const int size = 1024;
  std::vector<int32_t> dataIn;

  for (int i = 0; i < size; i++) {
    dataIn.push_back(i + 1);
  }

  cout << "Generating input data:\t\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // DFE Output
  gettimeofday(&startTime, NULL);
  std::vector<int32_t> dataOutDFE = PassThroughDFE(size, dataIn);
  cout << "DFE pass through total time:\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // CPU Output
  gettimeofday(&startTime, NULL);
  std::vector<int32_t> dataOutCPU = PassThroughCPU(size, dataIn);
  cout << "CPU pass through total time:\t\t\t";
  cout << calcTime(startTime) << "s" << endl;

  // Checking results
  check(dataOutDFE, dataOutCPU, size);

  return 0;
}

