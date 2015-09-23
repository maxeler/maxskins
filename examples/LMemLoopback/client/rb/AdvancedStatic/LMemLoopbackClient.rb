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

    # Initialize maxfile
    max_file = client.LMemLoopback_init()

    # Load DFE
    max_engine = client.max_load(max_file, '*')

    # Allocate and send input streams to server
    address_inA = client.malloc_int32_t(size)
    client.send_data_int32_t(address_inA, inA)

    address_inB = client.malloc_int32_t(size)
    client.send_data_int32_t(address_inB, inB)

    # Allocate memory for output stream on server
    address_outData = client.malloc_int32_t(size)

    puts "Loading DFE memory."
    actions_writeLMem = LMemLoopback_writeLMem_actions_t_struct.new()
    actions_writeLMem.param_address = 0
    actions_writeLMem.param_nbytes = sizeBytes
    actions_writeLMem.instream_cpu_to_lmem = address_inA
    address_actions_writeLMem = client.send_LMemLoopback_writeLMem_actions_t(actions_writeLMem)
    client.LMemLoopback_writeLMem_run(max_engine, address_actions_writeLMem)

    actions_writeLMem.param_address = sizeBytes
    actions_writeLMem.param_nbytes = sizeBytes
    actions_writeLMem.instream_cpu_to_lmem = address_inB
    address_actions_writeLMem = client.send_LMemLoopback_writeLMem_actions_t(actions_writeLMem)
    client.LMemLoopback_writeLMem_run(max_engine, address_actions_writeLMem)

    puts "Running DFE."
    actions = LMemLoopback_actions_t_struct.new();
    actions.param_N = size;

    address_actions = client.send_LMemLoopback_actions_t(actions)
    client.LMemLoopback_run(max_engine, address_actions);

    puts "Reading DFE memory."
    actions_readLMem = LMemLoopback_readLMem_actions_t_struct.new();

    actions_readLMem.param_address = 2 * sizeBytes;
    actions_readLMem.param_nbytes = sizeBytes;
    actions_readLMem.outstream_lmem_to_cpu = address_outData;    

    address_actions_readLMem = client.send_LMemLoopback_readLMem_actions_t(actions_readLMem)
    client.LMemLoopback_readLMem_run(max_engine, address_actions_readLMem);

    # Unload DFE
    client.max_unload(max_engine)

    # Get output stream from server
    outData = client.receive_data_int32_t(address_outData, size)

    # Free allocated memory for streams on server
    client.free(address_inA)
    client.free(address_inB)
    client.free(address_outData)

    # Free allocated maxfile data
    client.LMemLoopback_free()

    # Close!
    transport.close()

    # Checking results
    status = check(size, outData, inA, inB);

    if (status == 1)
        puts "Test failed."
    else
        puts "Test passed!"
    end


rescue Thrift::Exception => tx
    puts 'Thrift::Exception: ', tx.message, "\n"
end
