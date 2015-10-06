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
    actions_lmem = VectorAddition_writeLMem_actions_t_struct.new()
    actions_lmem.param_address = 0
    actions_lmem.param_nbytes = size*4
    actions_lmem.instream_cpu_to_lmem = address_a
    address_actions_lmem = client.send_VectorAddition_writeLMem_actions_t(actions_lmem)
 
    client.VectorAddition_writeLMem_run(max_engine, address_actions_lmem)

    # Add two vectors and a scalar
    puts 'Running on DFE.'
    actions = VectorAddition_actions_t_struct.new()
    actions.param_A = scalar
    actions.param_N = size
    actions.instream_y = address_b
    actions.outstream_s = address_c
    address_actions = client.send_VectorAddition_actions_t(actions)
 
    client.VectorAddition_run(max_engine, address_actions)

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
