#!/usr/bin/env ruby 

$:.push('../gen-rb')

require 'thrift'
require 'moving_average_service'

def check(dataIn, dataOut, size)
    for i in 1..(size-2)
        if ((dataOut[i] - ((dataIn[i - 1] + dataIn[i] + dataIn[i + 1]) / 3)).abs > 0.0001)
            puts "Test failed! #{dataOut[i]} != #{(dataIn[i - 1] + dataIn[i] + dataIn[i + 1]) / 3}"
            Kernel.exit(-1)
            exit -1
        end
    end

    puts "Test passed!"
    return nil
end

begin

    include Com::Maxeler::MovingAverage

    port = 9090

    # Make socket
    transport = Thrift::BufferedTransport.new(Thrift::Socket.new('localhost', port))

    # Wrap in a protocol
    protocol = Thrift::BinaryProtocol.new(transport)

    # Create a client to use the protocol encoder
    client = MovingAverageService::Client.new(protocol)

    # Connect!
    transport.open()
 
    size = 384        

    # Generate input
    dataIn = Array.new(size)

    for i in 0..(size-1)
        dataIn[i] = Float(rand (0..99))
    end

    # Initialize maxfile
    max_file = client.MovingAverage_init()

    # Load DFE
    max_engine = client.max_load(max_file, '*')

    # Allocate and send input streams to server
    address_dataIn = client.malloc_float(size)
    client.send_data_float(address_dataIn, dataIn)

    # Allocate memory for output stream on server
    address_dataOut = client.malloc_float(size)

    puts "Running DFE."
    actions = MovingAverage_actions_t_struct.new()
    actions.param_N = size
    actions.instream_x = address_dataIn
    actions.outstream_y = address_dataOut
    address_actions = client.send_MovingAverage_actions_t(actions)
    client.MovingAverage_run(max_engine, address_actions)

    # Unload DFE
    client.max_unload(max_engine)

    # Get output stream from server
    dataOut = client.receive_data_float(address_dataOut, size)

    # Free allocated maxfile data
    client.MovingAverage_free()

    # Free allocated memory for streams on server
    client.free(address_dataIn)
    client.free(address_dataOut)

    # Close!
    transport.close()

    # Checking results
    check(dataIn, dataOut, size)

rescue Thrift::Exception => tx
    print 'Thrift::Exception: ', tx.message, "\n"
    Kernel.exit(-1)
end
