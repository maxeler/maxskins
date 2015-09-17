#!/usr/bin/env python

import sys, glob
import random
sys.path.append('../gen-py')

from com.maxeler.Simple import SimpleService
from com.maxeler.Simple.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

def check(dataOut, expected, size):
	status = 0;
	for i in range(0, size):
		if dataOut[i] != expected[i]:
			print "dataOut ="
			print dataOut[i]
			print "epected ="
			print epected[i]
			status = 1
	return status

def SimpleCPU(size, dataIn, dataOut):
	for i in range(0, size):
		dataOut.append(dataIn[i] * dataIn[i] + dataIn[i])

try:

	# Make socket
	transport = TSocket.TSocket('localhost', 9090)

	# Buffering is critical. Raw sockets are very slow
	transport = TTransport.TBufferedTransport(transport)

	# Wrap in a protocol
	protocol = TBinaryProtocol.TBinaryProtocol(transport)

	# Create a client to use the protocol encoder
	client = SimpleService.Client(protocol)

	# Connect!
	transport.open()

	# Scalar inputs 
	size = 384

	# Generate two random vectors
	dataIn = []
	for i in range(0, size):
		dataIn.append(i + 1);

	# Initialize maxfile
	max_file = client.Simple_init()

	# Load DFE
	max_engine = client.max_load (max_file, '*')	

	# Allocate and send input streams to server
	address_dataIn = client.malloc_float(size)
	client.send_data_float(address_dataIn, dataIn)

	# Allocate memory for output stream on server
	address_dataOut = client.malloc_float(size)

	print "Running DFE.";
	actions = Simple_actions_t_struct(size, address_dataIn, address_dataOut)
        address_actions = client.send_Simple_actions_t(actions);
	client.Simple_run(max_engine, address_actions)

	# Unload DFE
	client.max_unload(max_engine)

	# Get output stream from server
	dataOut = client.receive_data_float(address_dataOut, size)

	# Free allocated memory for streams on server
	client.free(address_dataIn)
	client.free(address_dataOut)

	# Free allocated maxfile data
	client.Simple_free()

	# Checking results
	expected = []
	SimpleCPU(size, dataIn, expected);

	status = check(dataOut, expected, size);

	if status:
		print "Test failed."
	else:
		print "Test passed!"

	# Close!
	transport.close()

except Thrift.TException, tx:
	print '%s' % (tx.message)
