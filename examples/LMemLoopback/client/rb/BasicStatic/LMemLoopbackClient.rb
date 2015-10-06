#!/usr/bin/env ruby 

$:.push('../gen-rb')

require 'thrift'
require 'l_mem_loopback_service'

def check(size, outData, inA, inB)
    status = 0;
    for i in 0..(size-1)
        if (outData[i] != inA[i] + inB[i])
            puts outData[i]
            puts inA[i] + inB[i];
            status = 1
        end
    end
    return status
end

begin

    include Com::Maxeler::LMemLoopback

    port = 9090

    # Make socket
    transport = Thrift::BufferedTransport.new(Thrift::Socket.new('localhost', port))

    # Wrap in a protocol
    protocol = Thrift::BinaryProtocol.new(transport)

    # Create a client to use the protocol encoder
    client = LMemLoopbackService::Client.new(protocol)

    # Connect!
    transport.open()
 
    size = 384
    sizeBytes = size * 4     

    # Generate input
    inA = Array.new(size)
    inB = Array.new(size)

    for i in 0..(size-1)
        inA[i] = i
        inB[i] = size - i
    end

    # Allocate and send input streams to server
    address_inA = client.malloc_int32_t(size)
    client.send_data_int32_t(address_inA, inA)

    address_inB = client.malloc_int32_t(size)
    client.send_data_int32_t(address_inB, inB)

    # Allocate memory for output stream on server
    address_outData = client.malloc_int32_t(size)

    puts "Loading DFE memory."
    client.LMemLoopback_writeLMem(0, sizeBytes, address_inA);
    client.LMemLoopback_writeLMem(sizeBytes, sizeBytes, address_inB);

    puts "Running DFE."
    client.LMemLoopback(size);

    puts "Reading DFE memory."
    client.LMemLoopback_readLMem(2 * sizeBytes, sizeBytes, address_outData);

    # Get output stream from server
    outData = client.receive_data_int32_t(address_outData, size)

    # Free allocated memory for streams on server
    client.free(address_inA)
    client.free(address_inB)
    client.free(address_outData)

    # Close!
    transport.close()

    # Checking results
    status = check(size, outData, inA, inB);

    if (status == 1)
        puts "Test failed."
        Kernel.exit(-1)
    else
        puts "Test passed!"
    end


rescue Thrift::Exception => tx
    puts 'Thrift::Exception: ', tx.message, "\n"
    Kernel.exit(-1)
end
