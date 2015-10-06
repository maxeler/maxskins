#!/usr/bin/env ruby 

$:.push('../gen-rb')

require 'thrift'
require 'vector_addition_service'

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

    # Write vector a to LMem
    puts 'Writing to LMem.'
    client.VectorAddition_writeLMem(0,size*4, address_a)

    # Add two vectors and a scalar
    puts 'Running on DFE.'
    client.VectorAddition(scalar, size, address_b, address_c)

    # Get output stream from server
    c = client.receive_data_int32_t(address_c, size)

    # Free allocated memory for streams on server
    client.free(address_a)
    client.free(address_b)
    client.free(address_c)

    # Close!
    transport.close()

    # Checking results
    check(a, b, c, scalar, size)

rescue Thrift::Exception => tx
    print 'Thrift::Exception: ', tx.message, "\n"
    Kernel.exit(-1)
end
