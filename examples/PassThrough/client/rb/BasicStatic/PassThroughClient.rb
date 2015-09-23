#!/usr/bin/env ruby 

$:.push('../gen-rb')

require 'thrift'
require 'pass_through_service'

def check(data_out, expected, size)
    status = 0
    for i in 0..(size-1)
        if (data_out[i] != expected[i])
            puts "#{data_out[i]} != #{expected[i]}"
            status = 1
        end
    end
    return status
end

def PassThroughCPU(size, data_in, data_out)
    for i in 0..(size-1)
        data_out[i] = data_in[i]
    end
end

begin
    include Com::Maxeler::PassThrough

    port = 9090

    # Make socket
    transport = Thrift::BufferedTransport.new(Thrift::Socket.new('localhost', port))

    # Wrap in a protocol
    protocol = Thrift::BinaryProtocol.new(transport)

    # Create a client to use the protocol encoder
    client = PassThroughService::Client.new(protocol)

    # Connect!
    transport.open()
 
    size = 384        

    # Generate input
    data_in = Array.new(size)

    for i in 0..(size-1)
        data_in[i] = i + 1
    end

    # Allocate and send input streams to server
    address_data_in = client.malloc_int32_t(size)
    client.send_data_int32_t(address_data_in, data_in)

    # Allocate memory for output stream on server
    address_data_out = client.malloc_int32_t(size)

    puts "Running DFE."
    client.PassThrough(size, address_data_in, address_data_out)

    # Get output stream from server
    data_out = client.receive_data_int32_t(address_data_out, size)

    # Free allocated memory for streams on server
    client.free(address_data_in)
    client.free(address_data_out)

    # Close!
    transport.close()

    # Checking results
    expected = Array.new(size)
    PassThroughCPU(size, data_in, expected)

    status = check(data_out, expected, size)

    if (status == 1)
        puts "Test failed."
    else
        puts "Test passed!"
    end


rescue Thrift::Exception => thrift_exception
    puts 'Thrift::Exception: ', thrift_exception.message, "\n"
end
