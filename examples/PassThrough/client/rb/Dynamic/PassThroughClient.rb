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
    size_bytes = 4 * size

    # Generate input
    data_in = Array.new(size)

    for i in 0..(size-1)
        data_in[i] = i + 1
    end

    # Initialize maxfile
    max_file = client.PassThrough_init()

    # Load DFE
    max_engine = client.max_load(max_file, '*')

    # Allocate and send input streams to server
    address_data_in = client.malloc_float(size)
    client.send_data_float(address_data_in, data_in)

    # Allocate memory for output stream on server
    address_data_out = client.malloc_float(size)

    # Allocate memory for output stream on server
    address_dataOut = client.malloc_i32(size)

    puts "Running DFE."
    actions = client.max_actions_init(max_file, "default");

    client.max_set_param_uint64t(actions, "N", size);
    client.max_queue_input(actions, "x", address_data_in, size_bytes);
    client.max_queue_output(actions, "y", address_data_out, size_bytes);

    client.max_run(max_engine, actions);

    # Unload DFE
    client.max_unload(max_engine)

    # Get output stream from server
    data_out = client.receive_data_float(address_data_out, size)

    # Free allocated memory for streams on server
    client.free(address_data_in)
    client.free(address_data_out)

    # Free allocated maxfile data
    client.PassThrough_free()

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