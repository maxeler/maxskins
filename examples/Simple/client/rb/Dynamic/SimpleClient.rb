#!/usr/bin/env ruby

$LOAD_PATH.push('../gen-rb')

require 'thrift'
require 'simple_service'

def check(data_out, expected, size)
  status = 0
  (0..(size - 1)).each do |i|
    if data_out[i] != expected[i]
      puts "#{data_out[i]} != #{expected[i]}"
      status += 1
    end
  end
  status
end

def simple_cpu(size, data_in, data_out)
  (0..(size - 1)).each do |i|
    data_out[i] = (data_in[i] * data_in[i] + data_in[i])
  end
end

begin
  include Com::Maxeler::Simple

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
  client = SimpleService::Client.new(protocol)

  current_time = (Time.now - start_time).round(5)
  puts "Creating a client:\t\t\t\t#{current_time}s"

  # Connect!
  start_time = Time.now
  transport.open
  current_time = (Time.now - start_time).round(5)
  puts "Opening connection:\t\t\t\t#{current_time}s"

  start_time = Time.now
  size = 384
  size_bytes = 4 * size

  # Generate input
  data_in = Array.new(size)
  (0..(size - 1)).each do |i|
    data_in[i] = i + 1
  end
  current_time = (Time.now - start_time).round(5)
  puts "Generating input data:\t\t\t\t#{current_time}s"

  # Initialize maxfile
  start_time = Time.now
  max_file = client.Simple_init
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
  actions = client.max_actions_init(max_file, 'default')
  client.max_set_param_uint64t(actions, 'N', size)
  client.max_queue_input(actions, 'x', address_data_in, size_bytes)
  client.max_queue_output(actions, 'y', address_data_out, size_bytes)
  client.max_run(max_engine, actions)
  current_time = (Time.now - start_time).round(5)
  puts "Simple time:\t\t\t\t\t#{current_time}s"

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
  client.free(actions)
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated memory for streams on server:\t#{current_time}s"

  # Free allocated maxfile data
  start_time = Time.now
  client.Simple_free
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated maxfile data:\t\t\t#{current_time}s"

  # Close!
  start_time = Time.now
  transport.close
  current_time = (Time.now - start_time).round(5)
  puts "Closing connection:\t\t\t\t#{current_time}s"

  current_time = (Time.now - start_dfe_time).round(5)
  puts "DFE simple total time:\t\t\t\t#{current_time}s"

  # CPU Output
  start_time = Time.now
  expected = Array.new(size)
  simple_cpu(size, data_in, expected)
  current_time = (Time.now - start_time).round(5)
  puts "CPU simple total time:\t\t\t\t#{current_time}s"

  # Checking results
  start_time = Time.now
  status = check(data_out, expected, size)
  current_time = (Time.now - start_time).round(5)
  puts "Checking results:\t\t\t\t#{current_time}s"

  if status == 0
    puts 'Test passed!'
  else
    puts "Test failed #{status} times!"
    Kernel.exit(-1)
  end
rescue Thrift::Exception => thrift_exception
  puts 'Thrift::Exception: ', thrift_exception.message, "\n"
  Kernel.exit(-1)
end
