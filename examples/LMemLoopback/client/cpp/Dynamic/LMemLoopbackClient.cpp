/**
 * Document: MaxCompiler Tutorial (maxcompiler-tutorial.pdf)
 * Chapter: 13      Example: 2      Name: LMem Loopback
 * MaxFile name: LMemLoopback
 * Summary:
 *        Adds two LMem input streams and writes the result to LMem.
 */
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>
#include <stdlib.h>

#include "../gen-cpp/LMemLoopbackService.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace ::com::maxeler::LMemLoopback;

int check(int size, std::vector<int32_t> outData, std::vector<int32_t> inA, std::vector<int32_t> inB) {
	int status = 0;

	for (int i = 0; i < size; i++) {
		if (outData[i] != inA[i] + inB[i]) {
			fprintf(stderr, "[%d] Verification error, out: %u != expected: %u\n",
				i, outData[i], inA[i] + inB[i]);
			status = 1;
		}
	}

	return status;
}

std::vector<int32_t> LMemLoopbackDFE(int size, std::vector<int32_t> inA, std::vector<int32_t> inB) { 
	// Make socket
	boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));

	// Buffering is critical. Raw sockets are very slow
	boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));

	// Wrap in a protocol
	boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

	// Create a client to use the protocol encoder
	LMemLoopbackServiceClient client(protocol);

	std::vector<int32_t> outData;
	outData.resize(size);

	try {
		// Connect!
		transport->open();

		int sizeBytes = size * sizeof(int32_t);

		// Initialize maxfile
		remote_ptr maxfile = client.LMemLoopback_init();

		// Load DFE
		remote_ptr engine = client.max_load(maxfile, "*");

		// Allocate and send input streams to server
		remote_ptr address_inA = client.malloc_int32_t(size);
		client.send_data_int32_t(address_inA, inA);

		remote_ptr address_inB = client.malloc_int32_t(size);
		client.send_data_int32_t(address_inB, inB);

		// Allocate memory for output stream on server
		remote_ptr address_outData = client.malloc_int32_t(size);

		cout << "Loading DFE memory.\n";

		remote_ptr actions = client.max_actions_init(maxfile, "writeLMem");
		client.max_set_param_uint64t(actions, "address", 0);
		client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
		client.max_queue_input(actions, "cpu_to_lmem", address_inA, sizeBytes);

		client.max_run(engine, actions);

		actions = client.max_actions_init(maxfile, "writeLMem");
		client.max_set_param_uint64t(actions, "address", sizeBytes);
		client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
		client.max_queue_input(actions, "cpu_to_lmem", address_inB, sizeBytes);

		client.max_run(engine, actions);

		cout << "Running DFE.\n";

		actions = client.max_actions_init(maxfile, "default");
		client.max_set_param_uint64t(actions, "N", size);

		client.max_run(engine, actions);

		cout << "Reading DFE memory.\n";

		actions = client.max_actions_init(maxfile, "readLMem");
		client.max_set_param_uint64t(actions, "address", 2 * sizeBytes);
		client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
		client.max_queue_output(actions, "lmem_to_cpu", address_outData, sizeBytes);	

		client.max_run(engine, actions);
		client.free(actions);

		// Unload DFE
		client.max_unload (engine);

		// Get output stream from server
	        client.receive_data_int32_t(outData, address_outData, size);

		// Free allocated memory for streams on server
		client.free(address_inA);
		client.free(address_inB);
        	client.free(address_outData);

		// Free allocated maxfile data
		client.LMemLoopback_free();

		// Close!
		transport->close();

	} catch (TException& tx) {
		cout << "ERROR: " << tx.what() << endl;
		exit(-1);
	}

	return outData;
}

int main() {
	// Input
	const int size = 384;
	std::vector<int32_t> inA;
	std::vector<int32_t> inB;

	inA.resize(size);
	inB.resize(size);
	for (int i = 0; i < size; i++) {
		inA[i] = i;
		inB[i] = size - i;
	}

	// DFE Output
        std::vector<int32_t> outData = LMemLoopbackDFE(size, inA, inB);

	// Checking results
	if (check(size, outData, inA, inB)) {
		cout << "Test failed." << endl;
		exit(-1);
	} else {
		cout << "Test passed!" << endl;
	}

	return 0;
}

