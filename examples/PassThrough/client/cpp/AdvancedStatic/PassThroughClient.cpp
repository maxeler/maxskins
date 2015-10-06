#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>
#include <stdlib.h>

#include "../gen-cpp/PassThroughService.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace ::com::maxeler::PassThrough;

int check(std::vector<int32_t> dataOutDFE, std::vector<int32_t> dataOutCPU, int size) {
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

std::vector<int32_t> PassThroughCPU(int size, std::vector<int32_t> dataIn) {
	std::vector<int32_t> dataOut;
	dataOut.resize(size);

	for (int i=0 ; i<size ; i++) {
		dataOut[i] = dataIn[i];
	}

	return dataOut;
}

std::vector<int32_t> PassThroughDFE(int size, std::vector<int32_t> dataIn) { 
	// Make socket
	boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

	// Buffering is critical. Raw sockets are very slow
	boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

	// Wrap in a protocol
	boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

	// Create a client to use the protocol encoder
	PassThroughServiceClient client(protocol);

	std::vector<int32_t> dataOut;
	dataOut.resize(size);

	try {
		// Connect!
		transport->open();

		// Initialize maxfile
		remote_ptr maxfile = client.PassThrough_init();

		// Load DFE
		remote_ptr engine = client.max_load(maxfile, "*");

		// Allocate and send input streams to server
		remote_ptr address_dataIn = client.malloc_int32_t(size);
		client.send_data_int32_t(address_dataIn, dataIn);

		// Allocate memory for output stream on server
		remote_ptr address_dataOut = client.malloc_int32_t(size);

		cout << "Running DFE.\n";

		PassThrough_actions_t_struct actions;

		actions.__set_param_N(size);
		actions.__set_instream_x(address_dataIn);
		actions.__set_outstream_y(address_dataOut);

		remote_ptr address_actions = client.send_PassThrough_actions_t(actions);
		client.PassThrough_run(engine, address_actions);

		// Unload DFE
		client.max_unload (engine);

		// Get output stream from server
	        client.receive_data_int32_t(dataOut, address_dataOut, size);

		// Free allocated memory for streams on server
		client.free(address_dataIn);
        	client.free(address_dataOut);

		// Free allocated maxfile data
		client.PassThrough_free();

		// Close!
		transport->close();

	} catch (TException& tx) {
		cout << "ERROR: " << tx.what() << endl;
		exit(-1);
	}

	return dataOut;
}

int main() {
	// Input
	const int size = 1024;
	std::vector<int32_t> dataIn;

	for(int i = 0; i < size; i++) {
		dataIn.push_back(i + 1);
	}

	// CPU Output
	std::vector<int32_t> dataOutCPU = PassThroughCPU(size, dataIn);

	// DFE Output
        std::vector<int32_t> dataOutDFE = PassThroughDFE(size, dataIn);

	// Checking results
	if (check(dataOutDFE, dataOutCPU, size)) {
		cout << "Test failed." << endl;
		exit(-1);
	} else {
		cout << "Test passed!" << endl;
	}

	return 0;
}

