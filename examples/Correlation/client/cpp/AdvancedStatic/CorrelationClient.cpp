#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>
#include <math.h>
#include <sys/time.h>
#include <iomanip>

#include "../gen-cpp/correlationService.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace ::com::maxeler::correlation;

#define correlationMaxNumVariables (6000)
#define correlationMaxNumTimeseries (6000)
#define correlationNumTopScores (10)
#define correlationNumPipes (12)
#define correlationPCIE_ALIGNMENT (16)
#define correlationNumVectorsPerBurst (2)

void randomData (double** data, uint64_t numTimeseries, uint64_t sizeTimeseries) {
  // Generates random data.
  srand(time(NULL));
    
  for (uint64_t i=0; i<numTimeseries; i++) {
    for (uint64_t j=0; j<sizeTimeseries; j++) {
      data[i][j] = ((double)rand()/(double)RAND_MAX);
    }
  }
}

static size_t calcNumBursts (size_t numTimeseries) {
  // Calculate number of bursts for initializing LMem.
  size_t numVectors = 0;
  for (size_t i = 1; i <= numTimeseries; ++i)
    numVectors += (i + (correlationNumPipes - 1)) / correlationNumPipes;

  return (numVectors + (correlationNumVectorsPerBurst-1)) / correlationNumVectorsPerBurst;
}


static void prepareDataForDFE (double** data, uint64_t sizeTimeseries, uint64_t numTimeseries,
			       uint64_t numTimesteps, double windowSize, double* precalculations,
			       double* dataPairs) {
  // Precalculates and reorders data for the DFE.
  if (numTimeseries > correlationMaxNumTimeseries) {
    fprintf(stderr, "Number of Time series should be less or equal to %d. Terminating!\n",
	    correlationMaxNumTimeseries);
    fflush(stderr);
    exit(-1);
  }
	
  if (windowSize <2) {
    fprintf(stderr, "Window size must be equal or greater than 2. Terminating!\n");
    fflush(stderr);
    exit(-1);
  }

  if (numTimesteps > sizeTimeseries) {
    fprintf(stderr, "Number of Time steps should be less or equal to size of Time series. Terminating!\n");
    fflush(stderr);
    exit(-1);
  }

  double oldVal, newVal;
  double** sums = (double**) malloc(numTimesteps * sizeof(double*));
  double** sumsSQ = (double**) malloc(numTimesteps * sizeof(double*));
  double** inv = (double**) malloc(numTimesteps * sizeof(double*));
  for (uint64_t i=0; i < numTimesteps; i++) {
    sums[i] = (double*) malloc(numTimeseries * sizeof(double));
    sumsSQ[i] = (double*) malloc(numTimeseries * sizeof(double));
    inv[i] = (double*) malloc(numTimeseries * sizeof(double));
  }

  // 2 DFE input streams: precalculations and data pairs 
  for (uint64_t i=0; i<numTimesteps; i++) {
    for (uint64_t j=0; j<numTimeseries; j++) {
      oldVal = 0;
      if(i > windowSize) {
	oldVal = data [j][i - (uint64_t)windowSize];
      }
      newVal = data [j][i];

      if (i==0) {
	sums [i][j] = newVal;
	sumsSQ [i][j] = newVal * newVal;
      }
      else {
	sums[i][j] = sums[i-1][j] + newVal - oldVal;
	sumsSQ[i][j] = sumsSQ[i-1][j] + newVal * newVal - oldVal * oldVal;
      }
			
      inv[i][j] = 1/sqrt((uint64_t)windowSize * sumsSQ[i][j] - sums[i][j] * sums[i][j]);
			
      //Precalculations REORDERED in DFE ORDER
      precalculations[2*i*numTimeseries + 2*j] = sums[i][j];
      precalculations[2*i*numTimeseries + 2*j + 1] = inv[i][j];

      //Data pairs REORDERED in DFE ORDER
      dataPairs[2*i*numTimeseries + 2*j] = newVal;
      dataPairs[2*i*numTimeseries + 2*j + 1] = oldVal;
    }
  }

  for (uint64_t i=0; i<numTimesteps; i++) {
    free (sums[i]);
    free (sumsSQ[i]);
    free (inv[i]);
  }
	
  free (sums);
  free (sumsSQ);
  free (inv);			
}

uint64_t calcNumCorrelations(uint64_t numTimeseries) {
  // Calculates number  of correlations.
  return (numTimeseries * (numTimeseries - 1)) / 2;
}

uint64_t calcIndex (uint64_t i, uint64_t j) {
  // Calculates index of correlation between i and j
  if (i==j) {
    cout << "i and j must not be the same!" << endl;
    return -1;
  }
  if (i<j) {
    uint64_t tmp;
    tmp = j;
    j = i;
    i = tmp;
  }

  return (i*(i-1))/2+j;
}

void CorrelateDFE (double** data, uint64_t sizeTimeseries, uint64_t numTimeseries, double* correlations) {
  // Calculates correlations on DFE.
  
  // Make socket
  boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

  // Buffering is critical. Raw sockets are very slow
  boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

  // Wrap in a protocol
  boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

  // Create a client to use the protocol encoder
  correlationServiceClient client(protocol);

  try {
    // Connect!
    transport->open();

    // Initialize maxfile
    remote_ptr maxfile = client.correlation_init();

    // Load DFE
    remote_ptr engine = client.max_load(maxfile, "*");

    struct timeval startTime, endTime;
    double mtime, seconds, useconds;
    gettimeofday(&startTime, NULL);
    
    uint64_t numTimesteps = sizeTimeseries;	
    double windowSize = (double)sizeTimeseries;

    uint64_t numBursts = calcNumBursts (numTimeseries);
    std::vector<int32_t> loopLength;
    loopLength.push_back(client.correlation_get_CorrelationKernel_loopLength());

    double* precalculations = (double*) malloc(2 * numTimeseries * numTimesteps * sizeof(double));
    double* dataPairs = (double*) malloc(2 * numTimeseries * numTimesteps * sizeof(double));
	
    int burstSize = 384 / 2;  // for anything other than ISCA this should be 384
    std::vector<int32_t> inMemLoad;
    for(int i = 0; i < numBursts*burstSize; i++) {
      inMemLoad.push_back(0);
    }

    prepareDataForDFE (data, sizeTimeseries, numTimeseries, numTimesteps, windowSize,
		       precalculations, dataPairs);

    gettimeofday(&endTime, NULL);
    seconds  = endTime.tv_sec  - startTime.tv_sec;
    useconds = endTime.tv_usec - startTime.tv_usec;
    mtime = ((seconds) * 1000 + useconds/1000.0) + 0.5;
    cout << "Data reordering time:\t\t" <<  mtime / 1000 << "s" << endl;

    gettimeofday(&startTime, NULL);

    std::vector<double> precalculationsVec;
    std::vector<double> dataPairsVec;
    for(int i = 0; i < 2 * numTimeseries * numTimesteps; i++) {
      precalculationsVec.push_back(precalculations[i]);
      dataPairsVec.push_back(dataPairs[i]);
    }

    // Allocate and send input streams to server
    remote_ptr addressLoopLength = client.malloc_int32_t(1);
    client.send_data_int32_t(addressLoopLength, loopLength);
    
    remote_ptr addressInMemLoad = client.malloc_int32_t(numBursts * burstSize);
    client.send_data_int32_t(addressInMemLoad, inMemLoad);

    remote_ptr addressPrecalculations = client.malloc_double(2 * numTimeseries * numTimesteps);
    client.send_data_double(addressPrecalculations, precalculationsVec);

    remote_ptr addressDataPairs = client.malloc_double(2 * numTimeseries * numTimesteps);
    client.send_data_double(addressDataPairs, dataPairsVec);

    // Allocate memory for output stream on server
    remote_ptr addressOutCorrelation = client.malloc_double(numTimesteps * loopLength.at(0) *
							      correlationNumTopScores * correlationNumPipes +
							      numBursts * 24);  // for anything other than ISCA 48 should be instead of 24
    remote_ptr addressOutIndices = client.malloc_int32_t(2 * numTimesteps * loopLength.at(0) *
							   correlationNumTopScores * correlationNumPipes);

    correlation_loadLMem_actions_t_struct actionsLoadLMem;
    actionsLoadLMem.__set_param_numBursts(numBursts);
    actionsLoadLMem.__set_param_CorrelationKernel_loopLength(addressLoopLength);
    actionsLoadLMem.__set_instream_in_memLoad(addressInMemLoad);
    
    remote_ptr addressActionsLoadLMem = client.send_correlation_loadLMem_actions_t(actionsLoadLMem);
    client.correlation_loadLMem_run(engine, addressActionsLoadLMem);
    client.free(addressActionsLoadLMem);
    cout << "LMem initialized!" << endl;

    correlation_actions_t_struct actions;
    actions.__set_param_numBursts(numBursts);                           // scalar input
    actions.__set_param_numSteps(numTimesteps);                         // scalar input
    actions.__set_param_numVariables(numTimeseries);                    // scalar input
    actions.__set_param_outputLastStep(1);                              // scalar input
    actions.__set_param_windowSize(windowSize);                         // scalar input
    actions.__set_instream_in_precalculations(addressPrecalculations);  // streaming reordered input     
    actions.__set_instream_in_variable_pair(addressDataPairs);          // streaming reordered input
    actions.__set_outstream_out_correlation(addressOutCorrelation);     // streaming reordered input
    actions.__set_outstream_out_indices(addressOutIndices);             // streaming reordered input
    remote_ptr addressActions = client.send_correlation_actions_t(actions);
    client.correlation_run(engine, addressActions);
    client.free(addressActions);

    // Unload DFE
    client.max_unload(engine);
    
    // Get output stream from server
    std::vector<double> outCorrelation;
    client.receive_data_double(outCorrelation,
			       addressOutCorrelation, numTimesteps * loopLength.at(0) *
			       correlationNumTopScores * correlationNumPipes + numBursts * 24);;  // for anything other than ISCA 48 should be instead of 24
    std::vector<int32_t> outIndices;
    client.receive_data_int32_t(outIndices,
				addressOutIndices, 2 * numTimesteps * loopLength.at(0) *
				correlationNumTopScores * correlationNumPipes);
    client.receive_data_int32_t(loopLength, addressLoopLength, 1);

    // Free allocated memory for streams on server
    client.free(addressLoopLength);
    client.free(addressInMemLoad);
    client.free(addressPrecalculations);
    client.free(addressDataPairs);
    client.free(addressOutCorrelation);
    client.free(addressOutIndices);

    // Free allocated maxfile data
    client.correlation_free();

    // Close!
    transport->close();

    gettimeofday(&endTime, NULL);
    seconds  = endTime.tv_sec  - startTime.tv_sec;
    useconds = endTime.tv_usec - startTime.tv_usec;
    mtime = ((seconds) * 1000 + useconds/1000.0) + 0.5;
    cout << "DFE execution time:\t\t" <<  mtime / 1000 << "s" << endl;

    gettimeofday(&startTime, NULL);

    uint64_t position = 0;
    uint64_t index = 0;
    uint64_t start = (numTimesteps-1) * loopLength.at(0) * correlationNumTopScores * correlationNumPipes;

    for (uint64_t i=0; i < numTimeseries; i++) {
      for(uint64_t j = 0; j < i; j++) {
	correlations[index + j] = outCorrelation.at(start + position + j);
      }
      index += i;
      position += ((i/12)+1)*12;	
    }

    gettimeofday(&endTime, NULL);
    seconds  = endTime.tv_sec  - startTime.tv_sec;
    useconds = endTime.tv_usec - startTime.tv_usec;
    mtime = ((seconds) * 1000 + useconds/1000.0) + 0.5;
    cout << "Storing time:\t\t\t" <<  mtime / 1000 << "s" << endl;
						
    free (precalculations);
    free (dataPairs);
  } catch (TException& tx) {
    cout << "ERROR: " << tx.what() << endl;
    exit(-1);
  }	
}

void CorrelateCPU (double** data, uint64_t numTimeseries, double windowSize, uint64_t numTimesteps,
		   double* correlationsCPU, int32_t* indicesStep ) {
  // Calculates correlations on CPU.
  double* sums = (double*) malloc(numTimeseries * sizeof(double));
  double* sumsSQ = (double*) malloc(numTimeseries * sizeof(double));
  double* sumsXY = (double*) malloc(calcNumCorrelations(numTimeseries) * sizeof(double));
  for(uint64_t i = 0; i < numTimeseries; i++) {
    sums[i] = 0;
    sumsSQ[i] = 0;
  }
  for(uint64_t i = 0; i < calcNumCorrelations(numTimeseries); i++) {
    sumsXY[i] = 0;
  }

  for(uint64_t k = 0; k < numTimesteps; k++) {
    uint64_t indexCorrelation = 0;
    for(uint64_t i = 0; i < numTimeseries; i++) {
      double oldVal = 0;
      if(k >= windowSize) {
	oldVal = data[i][k - (uint64_t)windowSize];
      }
      double newVal = data[i][k];
      sums[i] += newVal - oldVal;
      sumsSQ[i] += newVal * newVal - oldVal * oldVal;
    }
    for(uint64_t i = 0; i < numTimeseries; i++) {
      double oldX = 0;
      if(k >= windowSize) {
	oldX = data[i][k - (uint64_t)windowSize];
      }
      double newX = data[i][k];
      for(uint64_t j = 0; j < i; j++) {
	double oldY = 0;
	if(k >= windowSize) {
	  oldY = data[j][k - (uint64_t)windowSize];
	}
	double newY = data[j][k];
	sumsXY[indexCorrelation] += newX * newY - oldX * oldY;
	double numerator = (windowSize * sumsXY[indexCorrelation] - sums[i] * sums[j]);
	double denominator = ((1 / sqrt(windowSize * sumsSQ[i] - sums[i] * sums[i])) * 
			      (1 / sqrt(windowSize * sumsSQ[j] - sums[j] * sums[j])));
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

void check(double* correlationsDFE, double* correlationsCPU, int32_t numTimeseries, int32_t* indicesStep) {
  // Checks if correlationsDFE and correlationsCPU are the same.
  uint64_t failed = 0;

  for(uint64_t i = 0; i < numTimeseries * (numTimeseries - 1) / 2; i++) {
    uint64_t j = calcIndex(indicesStep[2 * i], indicesStep[2 * i + 1]);
    if(correlationsDFE[j] != correlationsCPU[i]) {
      failed++;
      fprintf(stderr, "correlationCPU[%d]\t=  %.20lf\n", i, correlationsCPU[i]);
      fprintf(stderr, "correlationDFE[%d]\t=  %.20lf\n", j, correlationsDFE[j]);
    }
  }
  if(!failed) {
    cout << "Test passed!" << endl;
  } else {
    cout << "Test failed " << failed << " times." << endl;
    exit(-1);
  }
}

int main() {
  // Calls correlateDFE and correlateCPU
  // and checks if they return the same result.
  const int sizeTimeseries = 100;
  const int numTimeseries = 250;
  uint64_t numOfCorrelations = calcNumCorrelations(numTimeseries);
  double** data = (double**) malloc(numTimeseries * sizeof(double*));
  double* correlationsDFE = (double*) malloc(numOfCorrelations * sizeof(double));
  double* correlationsCPU = (double*) malloc(numOfCorrelations * sizeof(double));
  int32_t* indicesStep = (int32_t*) malloc(2 * numOfCorrelations * sizeof(int32_t));
  
  for (uint64_t i=0; i < numTimeseries; i++) 
    data[i] = (double*) malloc(sizeTimeseries * sizeof(double));
  randomData(data, numTimeseries, sizeTimeseries);

  struct timeval startTime, endTime;
  double mtime, seconds, useconds;
  gettimeofday(&startTime, NULL);
  CorrelateDFE(data, sizeTimeseries, numTimeseries, correlationsDFE);
  gettimeofday(&endTime, NULL);
  seconds  = endTime.tv_sec  - startTime.tv_sec;
  useconds = endTime.tv_usec - startTime.tv_usec;
  mtime = ((seconds) * 1000 + useconds/1000.0) + 0.5;
  cout << "DFE correlation total time:\t" <<  mtime / 1000 << "s" << endl;

  gettimeofday(&startTime, NULL);
  CorrelateCPU(data, numTimeseries, sizeTimeseries, sizeTimeseries, correlationsCPU, indicesStep);
  gettimeofday(&endTime, NULL);
  seconds  = endTime.tv_sec  - startTime.tv_sec;
  useconds = endTime.tv_usec - startTime.tv_usec;
  mtime = ((seconds) * 1000 + useconds/1000.0) + 0.5;
  cout << "CPU correlation total time:\t" <<  mtime / 1000 << "s" << endl;
  
  check(correlationsDFE, correlationsCPU, numTimeseries, indicesStep);

  free(correlationsDFE);
  free(correlationsCPU);
  free(indicesStep);
  for(uint64_t i=0; i < numTimeseries; i++) {
    free(data[i]);
  }
  free(data);
  
  return 0;
}
