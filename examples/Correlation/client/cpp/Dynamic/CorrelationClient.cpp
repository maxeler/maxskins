/* Copyright 2016 Maxeler Technologies */
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include <math.h>
#include <sys/time.h>

#include <iomanip>
#include <vector>

#include "../gen-cpp/correlationService.h"

using std::cout;
using std::endl;

using apache::thrift::TException;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TProtocol;
using apache::thrift::transport::TBufferedTransport;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;

using ::com::maxeler::correlation::correlationServiceClient;
using ::com::maxeler::correlation::remote_ptr;

#define correlationMaxNumVariables (6000)
#define correlationMaxNumTimeseries (6000)
#define correlationNumTopScores (10)
#define correlationNumPipes (12)
#define correlationPCIE_ALIGNMENT (16)
#define correlationNumVectorsPerBurst (2)

/**
 * Generates random data.
 *
 * @param numberOfRows    Number of rows
 * @param numberOfColumns Number of columns
 *
 * @return                Random data
 */
void randomData(double** data, uint64_t numTimeseries,
                uint64_t sizeTimeseries) {
  unsigned int seed = time(NULL);

  for (uint64_t i = 0; i < numTimeseries; i++) {
    for (uint64_t j = 0; j < sizeTimeseries; j++) {
      data[i][j] = (static_cast<double>(rand_r(&seed)) /
                    static_cast<double>(RAND_MAX));
    }
  }
}

/**
 * Calculate number of bursts for initializing LMem.
 *
 * @param numTimeseries Number of Time series
 *
 * @return              Number of bursts
 */
static size_t calcNumBursts(size_t numTimeseries) {
  size_t numVectors = 0;
  for (size_t i = 1; i <= numTimeseries; ++i)
    numVectors += (i + (correlationNumPipes - 1)) / correlationNumPipes;

  return (numVectors + (correlationNumVectorsPerBurst-1)) /
         correlationNumVectorsPerBurst;
}


/**
 * Precalculates and reorders data for the DFE.
 *
 * @param data             Data for correlation
 * @param sizeTimeseries   Size   of Time series
 * @param numTimeseries    Number of Time series
 * @param numTimesteps     Number of Time steps
 * @param windowSize       Window size
 * @param precalculations  Precalculations
 * @param dataPairs        Data pairs
 */
static void prepareDataForDFE(double** data, uint64_t sizeTimeseries,
                              uint64_t numTimeseries, uint64_t numTimesteps,
                              double windowSize, double* precalculations,
                              double* dataPairs) {
  if (numTimeseries > correlationMaxNumTimeseries) {
    fprintf(stderr, "Number of Time series should be less or equal to %d.",
            correlationMaxNumTimeseries);
    fprintf(stderr, " Terminating!\n");
    fflush(stderr);
    exit(-1);
  }

  if (windowSize <2) {
    fprintf(stderr, "Window size must be equal or greater than 2.");
    fprintf(stderr, " Terminating!\n");
    fflush(stderr);
    exit(-1);
  }

  if (numTimesteps > sizeTimeseries) {
    fprintf(stderr, "Number of Time steps should be less or equal to");
    fprintf(stderr, " size of Time series. Terminating!\n");
    fflush(stderr);
    exit(-1);
  }

  double oldVal, newVal;
  double** sums = reinterpret_cast<double**>(malloc(numTimesteps *
                                                    sizeof(double*)));
  double** sumsSQ = reinterpret_cast<double**>(malloc(numTimesteps *
                                                      sizeof(double*)));
  double** inv = reinterpret_cast<double**>(malloc(numTimesteps *
                                                   sizeof(double*)));
  for (uint64_t i=0; i < numTimesteps; i++) {
    sums[i] = reinterpret_cast<double*>(malloc(numTimeseries *
                                               sizeof(double)));
    sumsSQ[i] = reinterpret_cast<double*>(malloc(numTimeseries *
                                                 sizeof(double)));
    inv[i] = reinterpret_cast<double*>(malloc(numTimeseries * sizeof(double)));
  }

  // 2 DFE input streams: precalculations and data pairs
  for (uint64_t i = 0; i < numTimesteps; i++) {
    for (uint64_t j = 0; j < numTimeseries; j++) {
      oldVal = 0;
      if (i > windowSize) {
        oldVal = data[j][i - (uint64_t)windowSize];
      }
      newVal = data[j][i];

      if (i == 0) {
        sums[i][j] = newVal;
        sumsSQ[i][j] = newVal * newVal;
      } else {
        sums[i][j] = sums[i-1][j] + newVal - oldVal;
        sumsSQ[i][j] = sumsSQ[i-1][j] + newVal * newVal - oldVal * oldVal;
      }

      inv[i][j] = 1/sqrt((uint64_t)windowSize * sumsSQ[i][j] -
                          sums[i][j] * sums[i][j]);

      // Precalculations REORDERED in DFE ORDER
      precalculations[2*i*numTimeseries + 2*j] = sums[i][j];
      precalculations[2*i*numTimeseries + 2*j + 1] = inv[i][j];

      // Data pairs REORDERED in DFE ORDER
      dataPairs[2*i*numTimeseries + 2*j] = newVal;
      dataPairs[2*i*numTimeseries + 2*j + 1] = oldVal;
    }
  }

  for (uint64_t i = 0; i < numTimesteps; i++) {
    free(sums[i]);
    free(sumsSQ[i]);
    free(inv[i]);
  }

  free(sums);
  free(sumsSQ);
  free(inv);
}

/**
 * Calculates number  of correlations.
 *
 * @param numTimeseries Number of Time series
 *
 * @return              Number of correlations
 */
uint64_t calcNumCorrelations(uint64_t numTimeseries) {
  return (numTimeseries * (numTimeseries - 1)) / 2;
}

/**
 * Calculates index of correlationDfe between i and j.
 *
 * @param i the first series
 * @param j the second series
 *
 * @return  Index
 */
uint64_t calcIndex(uint64_t i, uint64_t j) {
  if (i == j) {
    cout << "i and j must not be the same!" << endl;
    return -1;
  }
  if (i < j) {
    uint64_t tmp;
    tmp = j;
    j = i;
    i = tmp;
  }

  return (i * (i - 1)) / 2 + j;
}

double calcTime(struct timeval startTime) {
    struct timeval endTime;

    gettimeofday(&endTime, NULL);

    double seconds  = endTime.tv_sec  - startTime.tv_sec;
    double useconds = endTime.tv_usec - startTime.tv_usec;

    return (((seconds) * 1000 + useconds/1000.0) + 0.5) / 1000;
}

/**
 * Calculates correlations on DFE.
 *
 * @param data             Data for correlation
 * @param numTimeseries    Number of Time series
 * @param sizeTimeseries   Size   of Time series
 * @param correlationsDFE  Correlation on DFE
 */
void CorrelateDFE(double** data, uint64_t sizeTimeseries,
                  uint64_t numTimeseries, double* correlations) {
  struct timeval startTime;
  double currentTime;

  gettimeofday(&startTime, NULL);

  // Make socket
  boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

  // Buffering is critical. Raw sockets are very slow
  boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

  // Wrap in a protocol
  boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

  // Create a client to use the protocol encoder
  correlationServiceClient client(protocol);

  currentTime = calcTime(startTime);
  cout << "Creating a client:\t\t\t\t" << currentTime << "s" << endl;

  try {
    // Connect!
    gettimeofday(&startTime, NULL);
    transport->open();
    currentTime = calcTime(startTime);
    cout << "Opening connection:\t\t\t\t" << currentTime << "s" << endl;

    // Initialize maxfile
    gettimeofday(&startTime, NULL);
    remote_ptr maxfile = client.correlation_init();
    currentTime = calcTime(startTime);
    cout << "Initializing maxfile:\t\t\t\t" << currentTime << "s" << endl;

    // Load DFE
    gettimeofday(&startTime, NULL);
    remote_ptr engine = client.max_load(maxfile, "*");
    currentTime = calcTime(startTime);
    cout << "Loading DFE:\t\t\t\t\t" << currentTime << "s" << endl;

    uint64_t numTimesteps = sizeTimeseries;
    double windowSize = static_cast<double>(sizeTimeseries);
    uint64_t numBursts = calcNumBursts(numTimeseries);

    // Get loop length
    gettimeofday(&startTime, NULL);
    std::vector<int32_t> loopLength;
    loopLength.push_back(
        client.correlation_get_CorrelationKernel_loopLength());
    currentTime = calcTime(startTime);
    cout << "Getting Correlation Kernel loopLength:\t\t";
    cout << currentTime << "s" << endl;

    // Prepare data for DFE
    gettimeofday(&startTime, NULL);

    double* precalculations =
        reinterpret_cast<double*>(malloc(2 * numTimeseries *
                                         numTimesteps * sizeof(double)));
    double* dataPairs =
        reinterpret_cast<double*>(malloc(2 * numTimeseries *
                                         numTimesteps * sizeof(double)));

    // for anything other than ISCA this should be 384
    int burstSize = 384 / 2;
    std::vector<int32_t> inMemLoad;
    for (int i = 0; i < numBursts*burstSize; i++) {
      inMemLoad.push_back(0);
    }

    prepareDataForDFE(data, sizeTimeseries, numTimeseries, numTimesteps,
                      windowSize, precalculations, dataPairs);

    std::vector<double> precalculationsVec;
    std::vector<double> dataPairsVec;
    for (int i = 0; i < 2 * numTimeseries * numTimesteps; i++) {
      precalculationsVec.push_back(precalculations[i]);
      dataPairsVec.push_back(dataPairs[i]);
    }

    currentTime = calcTime(startTime);
    cout << "Data reordering time:\t\t\t\t" << currentTime << "s" << endl;

    // Allocate and send input streams to server
    gettimeofday(&startTime, NULL);
    size_t LoopLengthSize = 32;
    remote_ptr addressLoopLength = client.malloc(LoopLengthSize);
    client.send_data_int32_t(addressLoopLength, loopLength);
    double LoopLengthTime = calcTime(startTime);
    cout << "\tSending LoopLength:\t\t(size = ";
    cout << LoopLengthSize << " bit)\t\t";
    cout << LoopLengthTime << "s" << endl;

    gettimeofday(&startTime, NULL);
    size_t InMemLoadSize = numBursts * burstSize * 32;
    remote_ptr addressInMemLoad = client.malloc(InMemLoadSize);
    client.send_data_int32_t(addressInMemLoad, inMemLoad);
    double InMemLoadTime = calcTime(startTime);
    cout << "\tSending InMemLoad:\t\t(size = " << InMemLoadSize << " bit)\t";
    cout << InMemLoadTime << "s" << endl;

    gettimeofday(&startTime, NULL);
    size_t PrecalculationsSize = 2 * numTimeseries *
                                 numTimesteps * sizeof(double);
    remote_ptr addressPrecalculations = client.malloc(PrecalculationsSize);
    client.send_data_double(addressPrecalculations, precalculationsVec);
    double PrecalculationsTime = calcTime(startTime);
    cout << "\tSending Precalculations:\t(size = ";
    cout << PrecalculationsSize << " bit)\t";
    cout << PrecalculationsTime << "s" << endl;

    gettimeofday(&startTime, NULL);
    size_t DataPairsSize = 2 * numTimeseries * numTimesteps * sizeof(double);
    remote_ptr addressDataPairs = client.malloc(DataPairsSize);
    client.send_data_double(addressDataPairs, dataPairsVec);
    double DataPairsTime = calcTime(startTime);
    cout << "\tSending DataPairs:\t\t(size = " << DataPairsSize << " bit)\t";
    cout << DataPairsTime << "s" << endl;

    currentTime = LoopLengthTime + InMemLoadTime +
                  PrecalculationsTime + DataPairsTime;
    double speed = (LoopLengthSize + InMemLoadSize +
                    PrecalculationsSize + DataPairsSize) /
                    currentTime / 1000000;
    cout << "Sending input streams to server total time:\t";
    cout << currentTime << "s\t";
    cout << "(average speed = " << speed << "Mb/s)" << endl;

    // Allocate memory for output stream on server
    gettimeofday(&startTime, NULL);
    // for anything other than ISCA 48 should be instead of 24
    size_t outCorrelationSize = numTimesteps * loopLength.at(0) *
                                correlationNumTopScores * correlationNumPipes +
                                numBursts * 24;
    size_t outIndicesSize = 2 * numTimesteps * loopLength.at(0) *
                            correlationNumTopScores * correlationNumPipes;
    remote_ptr addressOutCorrelation =
        client.malloc_double(outCorrelationSize);
    remote_ptr addressOutIndices = client.malloc_int32_t(outIndicesSize);
    currentTime = calcTime(startTime);
    cout << "Allocating memory for output stream on server:\t";
    cout << currentTime << "s" << endl;

    // Initialize LMem
    gettimeofday(&startTime, NULL);
    remote_ptr actions = client.max_actions_init(maxfile, "loadLMem");
    client.max_set_param_uint64t(actions, "numBursts", numBursts);
    client.max_queue_input(actions, "in_memLoad", addressInMemLoad,
                           numBursts * burstSize);
    client.max_ignore_scalar(actions, "LMemCommandsKernel",
                             "run_cycle_count");
    client.max_ignore_scalar(actions, "CorrelationKernel",
                             "run_cycle_count");
    client.max_run(engine, actions);
    currentTime = calcTime(startTime);
    cout << "LMem initialization:\t\t\t\t" << currentTime << "s" << endl;

    // Executing correlation action
    gettimeofday(&startTime, NULL);
    actions = client.max_actions_init(maxfile, "default");
    client.max_set_param_uint64t(actions, "numBursts", numBursts);
    client.max_set_param_uint64t(actions, "numSteps", numTimesteps);
    client.max_set_param_uint64t(actions, "numVariables", numTimeseries);
    client.max_set_param_uint64t(actions, "outputLastStep", 1);
    client.max_set_param_double(actions, "windowSize", windowSize);
    client.max_queue_input(actions, "in_precalculations",
                           addressPrecalculations, PrecalculationsSize);
    client.max_queue_input(actions, "in_variable_pair",
                           addressDataPairs, DataPairsSize);
    client.max_queue_output(actions, "out_correlation", addressOutCorrelation,
                            outCorrelationSize * sizeof(double));
    client.max_queue_output(actions, "out_indices", addressOutIndices,
                            outIndicesSize * sizeof(int32_t));
    client.max_run(engine, actions);
    currentTime = calcTime(startTime);
    cout << "Correlation time:\t\t\t\t" << currentTime << "s" << endl;

    // Unload DFE
    gettimeofday(&startTime, NULL);
    client.max_unload(engine);
    currentTime = calcTime(startTime);
    cout << "Unloading DFE:\t\t\t\t\t" << currentTime << "s" << endl;

    // Get output stream from server
    gettimeofday(&startTime, NULL);
    size_t loopLengthSize = 1;
    client.receive_data_int32_t(loopLength, addressLoopLength, 1);
    double loopLengthTime = calcTime(startTime);
    cout << "\tGet output stream loopLength:\t(size = ";
    cout << loopLengthSize * 32 << " bit)\t\t";
    cout << loopLengthTime << "s" << endl;

    gettimeofday(&startTime, NULL);
    std::vector<double> outCorrelation(outCorrelationSize);
    client.receive_data_double(outCorrelation, addressOutCorrelation,
                               outCorrelationSize);
    double outCorrelationTime = calcTime(startTime);
    cout << "\tGet output stream Correlation:\t(size = ";
    cout << outCorrelationSize * 64 << " bit)\t";
    cout << outCorrelationTime << "s" << endl;

    gettimeofday(&startTime, NULL);
    std::vector<int32_t> outIndices(outIndicesSize);
    client.receive_data_int32_t(outIndices, addressOutIndices, outIndicesSize);
    double outIndicesTime = calcTime(startTime);
    cout << "\tGet output stream outIndices:\t(size = ";
    cout << outIndicesSize * 32 << " bit)\t";
    cout << outIndicesTime << "s" << endl;

    currentTime = loopLengthTime + outCorrelationTime + outIndicesTime;
    speed = (loopLengthSize * 32 + outCorrelationSize * 64 +
             outIndicesSize * 32) / currentTime / 1000000;
    cout << "Getting output stream from server total time:\t";
    cout << currentTime << "s\t";
    cout << "(average speed = " << speed << "Mb/s)" << endl;

    // Free allocated memory for streams on server
    gettimeofday(&startTime, NULL);
    client.free(addressLoopLength);
    client.free(addressInMemLoad);
    client.free(addressPrecalculations);
    client.free(addressDataPairs);
    client.free(addressOutCorrelation);
    client.free(addressOutIndices);
    client.free(actions);
    currentTime = calcTime(startTime);
    cout << "Freeing allocated memory for streams on server:\t";
    cout << currentTime << "s" << endl;

    // Free allocated maxfile data
    gettimeofday(&startTime, NULL);
    client.correlation_free();
    currentTime = calcTime(startTime);
    cout << "Freeing allocated maxfile data:\t\t\t";
    cout << currentTime << "s" << endl;

    // Close!
    gettimeofday(&startTime, NULL);
    transport->close();
    currentTime = calcTime(startTime);
    cout << "Closing connection:\t\t\t\t" << currentTime << "s" << endl;

    // Store data
    gettimeofday(&startTime, NULL);

    uint64_t position = 0;
    uint64_t index = 0;
    uint64_t start = (numTimesteps - 1) * loopLength.at(0) *
                     correlationNumTopScores * correlationNumPipes;

    for (uint64_t i=0; i < numTimeseries; i++) {
      for (uint64_t j = 0; j < i; j++) {
        correlations[index + j] = outCorrelation.at(start + position + j);
      }
      index += i;
      position += ((i / 12) + 1) * 12;
    }

    currentTime = calcTime(startTime);
    cout << "Storing time:\t\t\t\t\t" << currentTime << "s" << endl;

    free(precalculations);
    free(dataPairs);
  } catch (TException& tx) {
    cout << "ERROR: " << tx.what() << endl;
    exit(-1);
  }
}

/**
 * Calculates correlations on CPU.
 *
 * @param data             Data for correlation
 * @param numTimeseries    Number of Time series
 * @param sizeTimeseries   Size   of Time series
 * @param numTimesteps     Number of Time steps
 * @param correlationsCPU  Correlation on CPU
 * @param indicesStep      Index of steps
 */
void CorrelateCPU(double** data, uint64_t numTimeseries, double windowSize,
                  uint64_t numTimesteps, double* correlationsCPU,
                  int32_t* indicesStep ) {
  double* sums = reinterpret_cast<double*>(malloc(numTimeseries *
                                                  sizeof(double)));
  double* sumsSQ = reinterpret_cast<double*>(malloc(numTimeseries *
                                                    sizeof(double)));
  double* sumsXY =
      reinterpret_cast<double*>(malloc(calcNumCorrelations(numTimeseries) *
                                       sizeof(double)));
  for (uint64_t i = 0; i < numTimeseries; i++) {
    sums[i] = 0;
    sumsSQ[i] = 0;
  }
  for (uint64_t i = 0; i < calcNumCorrelations(numTimeseries); i++) {
    sumsXY[i] = 0;
  }

  for (uint64_t k = 0; k < numTimesteps; k++) {
    uint64_t indexCorrelation = 0;
    for (uint64_t i = 0; i < numTimeseries; i++) {
      double oldVal = 0;
      if (k >= windowSize) {
        oldVal = data[i][k - (uint64_t)windowSize];
      }
      double newVal = data[i][k];
      sums[i] += newVal - oldVal;
      sumsSQ[i] += newVal * newVal - oldVal * oldVal;
    }
    for (uint64_t i = 0; i < numTimeseries; i++) {
      double oldX = 0;
      if (k >= windowSize) {
        oldX = data[i][k - (uint64_t)windowSize];
      }
      double newX = data[i][k];
      for (uint64_t j = 0; j < i; j++) {
        double oldY = 0;
        if (k >= windowSize) {
          oldY = data[j][k - (uint64_t)windowSize];
        }
        double newY = data[j][k];
        sumsXY[indexCorrelation] += newX * newY - oldX * oldY;
        double numerator = (windowSize * sumsXY[indexCorrelation] -
                            sums[i] * sums[j]);
        double denominator = ((1 / sqrt(windowSize * sumsSQ[i] -
                                        sums[i] * sums[i])) *
                              (1 / sqrt(windowSize * sumsSQ[j] -
                                        sums[j] * sums[j])));
        correlationsCPU[indexCorrelation] = numerator * denominator;
        indicesStep[2 * indexCorrelation] = j;
        indicesStep[2 * indexCorrelation + 1] = i;
        indexCorrelation += 1;
      }
    }
  }
  free(sums);
  free(sumsSQ);
  free(sumsXY);
}

/**
 * Checks if correlationsDFE and correlationsCPU are the same.
 *
 * @param correlationsDfe Correaltion on DFE
 * @param correlationsCpu Correaltion on CPU
 * @param numTimeseries   Number of Time series
 * @param indicesStep     Index of steps
 */
void check(double* correlationsDFE, double* correlationsCPU,
           int32_t numTimeseries, int32_t* indicesStep) {
  uint64_t failed = 0;

  for (uint64_t i = 0; i < numTimeseries * (numTimeseries - 1) / 2; i++) {
    uint64_t j = calcIndex(indicesStep[2 * i], indicesStep[2 * i + 1]);
    if (correlationsDFE[j] != correlationsCPU[i]) {
      failed++;
      fprintf(stderr, "correlationCPU[%d]\t=  %.20lf\n",
              i, correlationsCPU[i]);
      fprintf(stderr, "correlationDFE[%d]\t=  %.20lf\n",
              j, correlationsDFE[j]);
    }
  }
  if (!failed) {
    cout << "Test passed!" << endl;
  } else {
    cout << "Test failed " << failed << " times." << endl;
    exit(-1);
  }
}

/**
 * Calculates correlationsDFE and correlationsCPU and
 * checks if they return the same value.
 */
int main(int argc, char* argv[]) {
  if (argc != 3) {
    cout << "Usage: correlation_client <stream size> <number of streams>";
    exit(-1);
  }
  const int sizeTimeseries = atoi(argv[1]);
  const int numTimeseries = atoi(argv[2]);
  uint64_t numOfCorrelations = calcNumCorrelations(numTimeseries);
  double** data = reinterpret_cast<double**>(malloc(numTimeseries *
                                                    sizeof(double*)));
  double* correlationsDFE =
      reinterpret_cast<double*>(malloc(numOfCorrelations * sizeof(double)));
  double* correlationsCPU =
      reinterpret_cast<double*>(malloc(numOfCorrelations * sizeof(double)));
  int32_t* indicesStep =
      reinterpret_cast<int32_t*>(malloc(2 * numOfCorrelations *
                                        sizeof(int32_t)));

  for (uint64_t i=0; i < numTimeseries; i++)
    data[i] = reinterpret_cast<double*>(malloc(sizeTimeseries *
                                               sizeof(double)));
  randomData(data, numTimeseries, sizeTimeseries);

  struct timeval startTime;
  double currentTime;

  gettimeofday(&startTime, NULL);
  CorrelateDFE(data, sizeTimeseries, numTimeseries, correlationsDFE);
  currentTime = calcTime(startTime);
  cout << "DFE correlation total time:\t" << currentTime << "s" << endl;

  gettimeofday(&startTime, NULL);
  CorrelateCPU(data, numTimeseries, sizeTimeseries, sizeTimeseries,
               correlationsCPU, indicesStep);
  currentTime = calcTime(startTime);
  cout << "CPU correlation total time:\t" << currentTime << "s" << endl;

  check(correlationsDFE, correlationsCPU, numTimeseries, indicesStep);

  free(correlationsDFE);
  free(correlationsCPU);
  free(indicesStep);
  for (uint64_t i = 0; i < numTimeseries; i++) {
    free(data[i]);
  }
  free(data);

  return 0;
}

