#!/usr/bin/env ruby 

$:.push('../gen-rb')

require 'thrift'
require 'l_mem_loopback_service'

def check(size, outData, inA, inB)
    status = 0;
    for i in 0..(size-1)
        if (outData[i] != inA[i] + inB[i])
            puts outData[i].to_s + " != " + inA[i].to_s + " + " + inB[i].to_s;
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
    actions = client.max_actions_init(max_file, "writeLMem");
    client.max_set_param_uint64t(actions, "address", 0);
    client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
    client.max_queue_input(actions, "cpu_to_lmem", address_inA, sizeBytes);

    client.max_run(max_engine, actions);

    actions = client.max_actions_init(max_file, "writeLMem");
    client.max_set_param_uint64t(actions, "address", sizeBytes);
    client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
    client.max_queue_input(actions, "cpu_to_lmem", address_inB, sizeBytes);

    client.max_run(max_engine, actions);

    puts "Running DFE."
    actions = client.max_actions_init(max_file, "default");
    client.max_set_param_uint64t(actions, "N", size);

    client.max_run(max_engine, actions);

    puts "Reading DFE memory."
    actions = client.max_actions_init(max_file, "readLMem");
    client.max_set_param_uint64t(actions, "address", 2 * sizeBytes);
    client.max_set_param_uint64t(actions, "nbytes", sizeBytes);
    client.max_queue_output(actions, "lmem_to_cpu", address_outData, sizeBytes);    

    client.max_run(max_engine, actions);


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
        Kernel.exit(-1)
    else
        puts "Test passed!"
    end


rescue Thrift::Exception => tx
    puts 'Thrift::Exception: ', tx.message, "\n"
    Kernel.exit(-1)
end
