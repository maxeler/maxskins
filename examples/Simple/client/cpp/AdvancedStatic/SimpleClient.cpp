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


int check(double *dataOut, double *expected, int size)
{
	int status = 0;
	for(int i=0; i < size; i++)
	{
		if(dataOut[i] != expected[i]) {
			fprintf(stderr, "Output data @ %d = %1.8g (expected %1.8g)\n",
				i, dataOut[i], expected[i]);
			status = 1;
		}
	}
	return status;
}

void SimpleCPU(int size, double *dataIn, double *dataOut)
{
	for (int i=0 ; i<size ; i++) {
		dataOut[i] = dataIn[i]*dataIn[i] + dataIn[i];
	}
}

int main()
{
	boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));
	boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));
	boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
	SimpleServiceClient client(protocol);

	try {
      
	        transport->open();

		std::vector<double> dataIn;
		const int size = 1024;

		// Generate input data
		for(int i = 0; i < size; i++) {
			dataIn.push_back(i + 1);
		}

		// Initialize maxfile
		remote_ptr maxfile = client.Simple_init();

		// Load DFE
		remote_ptr engine = client.max_load(maxfile, "*");

		// Allocate and send input streams to server
		remote_ptr address_dataIn = client.malloc_float(size);
		client.send_data_float(address_dataIn, dataIn);

		// Allocate memory for output stream on server
		remote_ptr address_dataOut = client.malloc_float(size);

		cout << "Running DFE.\n";

		Simple_actions_t_struct actions;

		actions.__set_param_N(size);
		actions.__set_instream_x(address_dataIn);
		actions.__set_outstream_y(address_dataOut);

		remote_ptr address_actions = client.send_Simple_actions_t(actions);
		client.Simple_run(engine, address_actions);

		// Unload DFE
		client.max_unload(engine);

		// Get output stream from server
		std::vector<double> dataOut;
		dataOut.resize(size);
	        client.receive_data_float(dataOut, address_dataOut, size);

		// Free allocated memory for streams on server
		client.free(address_dataIn);
        	client.free(address_dataOut);
	
		// Free allocated maxfile data
		client.Simple_free();

		// Checking results
		std::vector<double> expected;
		expected.resize(size);	
		SimpleCPU(size, dataIn.data(), expected.data());

		int status = check(dataOut.data(), expected.data(), size);

		if (status)
			cout << "Test failed." << endl;
		else
			cout << "Test passed!" << endl;

		transport->close();
    
	} catch (TException& tx) {
		cout << "ERROR: " << tx.what() << endl;
	}

	return 0;
}

