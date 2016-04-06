#!/usr/bin/env ruby

$LOAD_PATH.push('../gen-rb')

require 'thrift'
require 'moving_average_service'

def check(data_in, data_out, size)
  status = 0
  (1..(size - 2)).each do |i|
    next if (((data_in[i - 1] + data_in[i] + data_in[i + 1]) / 3) -
            data_out[i]).abs < 0.0001
    puts "Output data @ #{i} =  #{data_out[i]} "\
         "(expected #{(data_in[i - 1] + data_in[i] + data_in[i + 1]) / 3})"
    status += 1
  end

  status
end

begin

  include Com::Maxeler::MovingAverage

  start_time = Time.now
  start_dfe_time = start_time

  port = 9090

  # Make socket
  socket = Thrift::Socket.new('localhost', port)

  # Buffering is critical. Raw sockets are very slow
  transport = Thrift::BufferedTransport.new(socket)

  # Wrap in a protocol
  protocol = Thrift::BinaryProtocol.new(transport)

  # Create a client to use the protocol encoder
  client = MovingAverageService::Client.new(protocol)

  current_time = (Time.now - start_time).round(5)
  puts "Creating a client:\t\t\t\t#{current_time}s"

  # Connect!
  start_time = Time.now
  transport.open
  current_time = (Time.now - start_time).round(5)
  puts "Opening connection:\t\t\t\t#{current_time}s"

  start_time = Time.now
  size = 384

  # Generate input
  data_in = Array.new(size)
  (0..(size - 1)).each do |i|
    data_in[i] = Float(rand(0..99))
  end
  current_time = (Time.now - start_time).round(5)
  puts "Generating input data:\t\t\t\t#{current_time}s"

  # Initialize maxfile
  start_time = Time.now
  max_file = client.MovingAverage_init()
  current_time = (Time.now - start_time).round(5)
  puts "Initializing maxfile:\t\t\t\t#{current_time}s"

  # Load DFE
  start_time = Time.now
  max_engine = client.max_load(max_file, '*')
  current_time = (Time.now - start_time).round(5)
  puts "Loading DFE:\t\t\t\t\t#{current_time}s"

  # Allocate and send input streams to server
  start_time = Time.now
  address_data_in = client.malloc_float(size)
  client.send_data_float(address_data_in, data_in)
  current_time = (Time.now - start_time).round(5)
  puts "Sending input data:\t\t\t\t#{current_time}s"

  # Allocate memory for output stream on server
  start_time = Time.now
  address_data_out = client.malloc_float(size)
  current_time = (Time.now - start_time).round(5)
  puts "Allocating memory for output stream on server:\t#{current_time}s"

  # Action default
  start_time = Time.now
  actions = MovingAverage_actions_t_struct.new
  actions.param_N = size
  actions.instream_x = address_data_in
  actions.outstream_y = address_data_out
  address_actions = client.send_MovingAverage_actions_t(actions)
  client.MovingAverage_run(max_engine, address_actions)
  current_time = (Time.now - start_time).round(5)
  puts "Moving average time:\t\t\t\t#{current_time}s"

  # Unload DFE
  start_time = Time.now
  client.max_unload(max_engine)
  current_time = (Time.now - start_time).round(5)
  puts "Unloading DFE:\t\t\t\t\t#{current_time}s"

  # Get output stream from server
  start_time = Time.now
  data_out = client.receive_data_float(address_data_out, size)
  current_time = (Time.now - start_time).round(5)
  puts "Getting output stream:\t(size = #{size * 32} bit)\t#{current_time}s"

  # Free allocated memory for streams on server
  start_time = Time.now
  client.free(address_data_in)
  client.free(address_data_out)
  client.free(address_actions)
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated memory for streams on server:\t#{current_time}s"

  # Free allocated maxfile data
  start_time = Time.now
  client.MovingAverage_free()
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated maxfile data:\t\t\t#{current_time}s"

  # Checking results
  start_time = Time.now
  status = check(data_in, data_out, size)
  current_time = (Time.now - start_time).round(5)
  puts "Checking results:\t\t\t\t#{current_time}s"

  # Close!
  start_time = Time.now
  transport.close
  current_time = (Time.now - start_time).round(5)
  puts "Closing connection:\t\t\t\t#{current_time}s"

  current_time = (Time.now - start_dfe_time).round(5)
  puts "DFE moving average total time:\t\t\t#{current_time}s"

  if status == 0
    puts 'Test successful!'
  else
    puts "Test failed #{status} times!"
    Kernel.exit(-1)
  end

rescue Thrift::Exception => tx
  print 'Thrift::Exception: ', tx.message, "\n"
  Kernel.exit(-1)
end
