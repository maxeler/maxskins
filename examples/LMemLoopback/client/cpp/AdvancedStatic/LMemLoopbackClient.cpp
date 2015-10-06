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

		LMemLoopback_writeLMem_actions_t_struct actions_lmem_write;

		actions_lmem_write.__set_param_address(0);
		actions_lmem_write.__set_param_nbytes(sizeBytes);
		actions_lmem_write.__set_instream_cpu_to_lmem(address_inA);

		remote_ptr address_actions_lmem_write = client.send_LMemLoopback_writeLMem_actions_t(actions_lmem_write);
		client.LMemLoopback_writeLMem_run(engine, address_actions_lmem_write);

		actions_lmem_write.__set_param_address(sizeBytes);
		actions_lmem_write.__set_param_nbytes(sizeBytes);
		actions_lmem_write.__set_instream_cpu_to_lmem(address_inB);	

		address_actions_lmem_write = client.send_LMemLoopback_writeLMem_actions_t(actions_lmem_write);
		client.LMemLoopback_writeLMem_run(engine, address_actions_lmem_write);
		client.free(address_actions_lmem_write);

		cout << "Running DFE.\n";

		LMemLoopback_actions_t_struct actions;

		actions.__set_param_N(size);

		remote_ptr address_actions = client.send_LMemLoopback_actions_t(actions);
		client.LMemLoopback_run(engine, address_actions);
		client.free(address_actions);

		cout << "Reading DFE memory.\n";

		LMemLoopback_readLMem_actions_t_struct actions_lmem_read;

		actions_lmem_read.__set_param_address(2 * sizeBytes);
		actions_lmem_read.__set_param_nbytes(sizeBytes);
		actions_lmem_read. __set_outstream_lmem_to_cpu(address_outData);	

		remote_ptr address_actions_lmem_read = client.send_LMemLoopback_readLMem_actions_t(actions_lmem_read);
		client.LMemLoopback_readLMem_run(engine, address_actions_lmem_read);
		client.free(address_actions_lmem_read);

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

