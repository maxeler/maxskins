#!/usr/bin/env ruby 

$:.push('../gen-rb')

require 'thrift'
require 'simple_service'

def check(dataOut, expected, size)
	status = 0;
	for i in 0..(size-1)
		if (dataOut[i] != expected[i])
			puts dataOut[i]
			puts expected[i];
			status = 1
		end
	end
	return status
end

def SimpleCPU(size, dataIn, dataOut)
	for i in 0..(size-1)
		dataOut[i] = (dataIn[i] * dataIn[i] + dataIn[i])
	end
end

begin

	include Com::Maxeler::Simple

	port = 9090

	# Make socket
	transport = Thrift::BufferedTransport.new(Thrift::Socket.new('localhost', port))

	# Wrap in a protocol
	protocol = Thrift::BinaryProtocol.new(transport)

	# Create a client to use the protocol encoder
	client = SimpleService::Client.new(protocol)

	# Connect!
	transport.open()
 
	size = 384        

	# Generate input
	dataIn = Array.new(size)

	for i in 0..(size-1)
		dataIn[i] = i + 1
	end

	# Allocate and send input streams to server
	address_dataIn = client.malloc_float(size)
	client.send_data_float(address_dataIn, dataIn)

	# Allocate memory for output stream on server
	address_dataOut = client.malloc_float(size)

	puts "Running DFE."
	client.Simple(size, address_dataIn, address_dataOut)

	# Get output stream from server
	dataOut = client.receive_data_float(address_dataOut, size)

	# Free allocated memory for streams on server
	client.free(address_dataIn)
	client.free(address_dataOut)

	# Close!
	transport.close()

	# Checking results
	expected = Array.new(size)
	SimpleCPU(size, dataIn, expected);

	status = check(dataOut, expected, size);

	if (status == 1)
		puts "Test failed."
	else
		puts "Test passed!"
	end


rescue Thrift::Exception => tx
	puts 'Thrift::Exception: ', tx.message, "\n"
end
