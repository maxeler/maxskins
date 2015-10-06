#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>
#include <stdlib.h>

#include "../gen-cpp/MovingAverageService.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace ::com::maxeler::MovingAverage;

void check(std::vector<double> dataIn, std::vector<double> dataOut, int size)
{
	int status = 0;

	for (int i = 1; i < size - 1; i++) {
		float x = dataIn[i - 1];
		float y = dataIn[i];
		float z = dataIn[i + 1];
		float tmp = (x + y + z) / 3;
		if(dataOut[i] != tmp) {
			status = 1;
			cout << "Test failed!" << endl;
			exit(-1);
			break;
		}
	}

	if (!status) 
		cout << "Test successful!" << endl;
}

int main()
{
	boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));
	boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));
	boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
	MovingAverageServiceClient client(protocol);

	try {
      
	        transport->open();

		const int size = 384;
		std::vector<double> dataIn;
		int sizeBytes = size * sizeof(float);

		// Generate input data
		for(int i = 0; i<size; i++) {
			dataIn.push_back(random() % 100);
		}


		// Initialize maxfile
		remote_ptr maxfile = client.MovingAverage_init();

		// Load DFE
		remote_ptr engine = client.max_load(maxfile, "*");
	
		// Allocate and send input streams to server
		remote_ptr address_dataIn = client.malloc_float(size);
		client.send_data_float(address_dataIn, dataIn);

		// Allocate memory for output stream on server
		remote_ptr address_dataOut = client.malloc_float(size);

		cout << "Running DFE\n";

		remote_ptr actions = client.max_actions_init(maxfile, "default");
		client.max_set_param_uint64t(actions, "N", size);
		client.max_queue_input(actions, "x", address_dataIn, sizeBytes);
		client.max_queue_output(actions, "y", address_dataOut, sizeBytes);

		client.max_run(engine, actions);

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
		client.MovingAverage_free();
	
		// Checking results
		check(dataIn, dataOut, size);		

		transport->close();
    
	} catch (TException& tx) {
		cout << "ERROR: " << tx.what() << endl;
		exit(-1);
	}

	return 0;
}


