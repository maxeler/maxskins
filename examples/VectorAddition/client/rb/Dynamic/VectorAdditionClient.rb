#!/usr/bin/env ruby

$:.push('../gen-rb')

require 'thrift'
require 'vector_addition_service'
require 'vector_addition_types'

def check(a, b, c, scalar, size)
    for i in 0..(size-1)
        if (c[i] != a[i] + b[i] + scalar)
            puts 'Test failed!'
            Kernel.exit(-1)
        end
    end

    puts 'Test passed!'

    return nil
end

begin

    include Com::Maxeler::VectorAddition

    port = 9090

    # Make socket
    transport = Thrift::BufferedTransport.new(Thrift::Socket.new('localhost', port))
   
    # Wrap in a protocol
    protocol = Thrift::BinaryProtocol.new(transport)

    # Create a client to use the protocol encoder
    client = VectorAdditionService::Client.new(protocol)

    # Connect!
    transport.open()

    # Scalar inputs 
    size = 384        
    scalar = 3

    # Generate two random vectors
    a = Array.new(size)
    b = Array.new(size)

    for i in 0..(size-1)
        a[i] = rand (0..99)
        b[i] = rand (0..99)
    end
 
    # Allocate and send input streams to server
    address_a = client.malloc_int32_t(size)
    client.send_data_int32_t(address_a, a)

    address_b = client.malloc_int32_t(size)
    client.send_data_int32_t(address_b, b)

    # Allocate memory for output stream on server
    address_c = client.malloc_int32_t(size)

    # Initialize maxfile
    max_file = client.VectorAddition_init()

    # Load DFE
    max_engine = client.max_load(max_file, '*')

    # Write vector a to LMem
    puts 'Writing to LMem.'
    act = client.max_actions_init(max_file, "writeLMem")
    client.max_set_param_uint64t(act, "address", 0)
    client.max_set_param_uint64t(act, "nbytes", size*4)
    client.max_queue_input(act, "cpu_to_lmem", address_a, size * 4)
    client.max_run(max_engine, act)

    # Add two vectors and a scalar
    puts 'Running on DFE.'
    act = client.max_actions_init(max_file, "default")
    client.max_set_param_uint64t(act, "N", size)
    client.max_set_param_uint64t(act, "A", scalar)
    client.max_queue_input(act, "y", address_b, size * 4)
    client.max_queue_output(act, "s", address_c, size * 4)
    client.max_run(max_engine, act)

    # Unload DFE
    client.max_unload(max_engine)

    # Get output stream from server
    c = client.receive_data_int32_t(address_c, size)

    # Free allocated memory for streams on server
    client.free(address_a)
    client.free(address_b)
    client.free(address_c)

    # Free allocated maxfile data
    client.VectorAddition_free()

    # Close!
    transport.close()

    # Checking results
    check(a, b, c, scalar, size)

rescue Thrift::Exception => tx
    print 'Thrift::Exception: ', tx.message, "\n"
    Kernel.exit(-1)
end
