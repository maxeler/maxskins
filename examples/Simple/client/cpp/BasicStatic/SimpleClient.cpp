#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>
#include <stdlib.h>

#include "../gen-cpp/SimpleService.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace ::com::maxeler::Simple;

int check(std::vector<double> dataOutDFE, std::vector<double> dataOutCPU, int size) {
	int status = 0;

	for(int i=0; i < size; i++) {
		if(dataOutDFE[i] != dataOutCPU[i]) {
			fprintf(stderr, "Output data @ %d = %1.8g (expected %1.8g)\n",
				i, dataOutDFE[i], dataOutCPU[i]);
			status = 1;
		}
	}

	return status;
}

std::vector<double> SimpleCPU(int size, std::vector<double> dataIn) {
	std::vector<double> dataOut;
	dataOut.resize(size);

	for (int i=0 ; i<size ; i++) {
		dataOut[i] = dataIn[i]*dataIn[i] + dataIn[i];
	}

	return dataOut;
}

std::vector<double> SimpleDFE(int size, std::vector<double> dataIn) {
	// Make socket
	boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

	// Buffering is critical. Raw sockets are very slow
	boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

	// Wrap in a protocol
	boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

	// Create a client to use the protocol encoder
	SimpleServiceClient client(protocol);

	std::vector<double> dataOut;
	dataOut.resize(size);

	try {
		// Connect!
		transport->open();

		// Allocate and send input streams to server
		remote_ptr address_dataIn = client.malloc_float(size);
		client.send_data_float(address_dataIn, dataIn);

		// Allocate memory for output stream on server
		remote_ptr address_dataOut = client.malloc_float(size);

		cout << "Running DFE.\n";

		client.Simple(size, address_dataIn, address_dataOut);

		// Get output stream from server
	        client.receive_data_float(dataOut, address_dataOut, size);

		// Free allocated memory for streams on server
		client.free(address_dataIn);
        	client.free(address_dataOut);

		// Free allocated maxfile data
		client.Simple_free();
	
		// Close!
		transport->close();
    
	} catch (TException& thrift_exceptiion) {
		cout << "ERROR: " << thrift_exceptiion.what() << endl;
		exit(-1);
	}

	return dataOut;
}

int main() {
	// Input
	const int size = 1024;
	std::vector<double> dataIn;

	for(int i = 0; i < size; i++) {
		dataIn.push_back(i + 1);
	}

	// CPU Output
	std::vector<double> dataOutCPU = SimpleCPU(size, dataIn);

	// DFE Output
        std::vector<double> dataOutDFE = SimpleDFE(size, dataIn);

	// Checking results
	if (check(dataOutDFE, dataOutCPU, size)) {
		cout << "Test failed." << endl;
		exit(-1);
	} else {
		cout << "Test passed!" << endl;
	}

	return 0;
}

