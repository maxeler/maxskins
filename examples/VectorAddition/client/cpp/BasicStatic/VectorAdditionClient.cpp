#include <iostream>

#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>
#include <stdlib.h>

#include "../gen-cpp/VectorAdditionService.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace ::com::maxeler::VectorAddition;

void check(std::vector<int32_t> x, std::vector<int32_t> y, std::vector<int32_t> s, int scalar, int size)
{
	int status = 0;

	for(int i = 0; i < size; i++)
		if (s[i] != x[i] + y[i] + scalar) {
			cout << "Test failed!" << endl;
			status = 1;
			break;
		}

	if (!status) 
		cout << "Test successful!" << endl;
}

int main(int argc, char** argv) {

	boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));
	boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));
	boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
	VectorAdditionServiceClient client(protocol);
	
	try {
	  
		transport->open();
  
		const int size = 384;
		int sizeBytes = size * sizeof(int32_t);
		std::vector<int32_t> x;
		std::vector<int32_t> y; 
		int scalar = 3;

		// Generate input data
		for(int i = 0; i<size; ++i) {
			x.push_back(random() % 100);
			y.push_back(random() % 100);
		}

		// Allocate and send input streams to server
		remote_ptr address_x = client.malloc_int32_t(size);
		client.send_data_int32_t(address_x, x);

		remote_ptr address_y = client.malloc_int32_t(size);
		client.send_data_int32_t(address_y, y);

		// Allocate memory for output stream on server
		remote_ptr address_s = client.malloc_int32_t(size);

		cout << "Writing to LMem." << endl;
	   
		client.VectorAddition_writeLMem(0, sizeBytes, address_x); 

		cout << "Running on DFE." << endl;
		
		client.VectorAddition(scalar, size, address_y, address_s);

		// Get output stream from server
		std::vector<int32_t> s;
		s.resize(size);
		client.receive_data_int32_t(s, address_s, size);

		// Free allocated memory for streams on server
		client.free(address_x);
		client.free(address_y);
		client.free(address_s);

		// Checking results
		check(x, y, s, scalar, size);
	
		transport->close();
	
	} catch (TException& tx) {
		cout << "ERROR: " << tx.what() << endl;
	}

	return 0;
}
