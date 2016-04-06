#!/usr/bin/env ruby

$LOAD_PATH.push('../gen-rb')

require 'thrift'
require 'vector_addition_service'

def check(a, b, c, scalar, size)
  (0..(size - 1)).each do |i|
    if c[i] != a[i] + b[i] + scalar
      puts 'Test failed!'
      Kernel.exit(-1)
    end
  end

  puts 'Test passed!'
end

def vector_addition_cpu(size, first_vector, second_vector, scalar, data_out)
  (0..(size - 1)).each do |i|
    data_out[i] = (first_vector[i] + second_vector[i] + scalar)
  end
end

begin

  include Com::Maxeler::VectorAddition

  port = 9090

  start_time = Time.now
  start_dfe_time = start_time

  # Make socket
  socket = Thrift::Socket.new('localhost', port)

  # Buffering is critical. Raw sockets are very slow
  transport = Thrift::BufferedTransport.new(socket)

  # Wrap in a protocol
  protocol = Thrift::BinaryProtocol.new(transport)

  # Create a client to use the protocol encoder
  client = VectorAdditionService::Client.new(protocol)

  current_time = (Time.now - start_time).round(5)
  puts "Creating a client:\t\t\t\t#{current_time}s"

  # Connect!
  start_time = Time.now
  transport.open
  current_time = (Time.now - start_time).round(5)
  puts "Opening connection:\t\t\t\t#{current_time}s"

  # Generate input data
  start_time = Time.now
  size = 384
  scalar = 3

  a = Array.new(size)
  b = Array.new(size)

  (0..(size - 1)).each do |i|
    a[i] = rand(0..99)
    b[i] = rand(0..99)
  end
  current_time = (Time.now - start_time).round(5)
  puts "Generating input data:\t\t\t\t#{current_time}s"

  # Initialize maxfile
  start_time = Time.now
  max_file = client.VectorAddition_init
  current_time = (Time.now - start_time).round(5)
  puts "Initializing maxfile:\t\t\t\t#{current_time}s"

  # Load DFE
  start_time = Time.now
  max_engine = client.max_load(max_file, '*')
  current_time = (Time.now - start_time).round(5)
  puts "Loading DFE:\t\t\t\t\t#{current_time}s"

  # Allocate and send input streams to server
  start_time = Time.now
  address_a = client.malloc_int32_t(size)
  client.send_data_int32_t(address_a, a)

  address_b = client.malloc_int32_t(size)
  client.send_data_int32_t(address_b, b)
  current_time = (Time.now - start_time).round(5)
  puts "Sending input data:\t\t\t\t#{current_time}s"

  # Allocate memory for output stream on server
  start_time = Time.now
  address_c = client.malloc_int32_t(size)
  current_time = (Time.now - start_time).round(5)
  puts "Allocating memory for output stream on server:\t#{current_time}s"

  # Write vector a to LMem
  start_time = Time.now
  actions = client.max_actions_init(max_file, 'writeLMem')
  client.max_set_param_uint64t(actions, 'address', 0)
  client.max_set_param_uint64t(actions, 'nbytes', size * 4)
  client.max_queue_input(actions, 'cpu_to_lmem', address_a, size * 4)

  client.max_run(max_engine, actions)
  current_time = (Time.now - start_time).round(5)
  puts "Writing to LMem:\t\t\t\t#{current_time}s"

  # Action default
  start_time = Time.now
  actions = client.max_actions_init(max_file, 'default')
  client.max_set_param_uint64t(actions, 'N', size)
  client.max_set_param_uint64t(actions, 'A', scalar)
  client.max_queue_input(actions, 'y', address_b, size * 4)
  client.max_queue_output(actions, 's', address_c, size * 4)
  client.max_run(max_engine, actions)

  current_time = (Time.now - start_time).round(5)
  puts "Vector addition time:\t\t\t\t#{current_time}s"

  # Unload DFE
  start_time = Time.now
  client.max_unload(max_engine)
  current_time = (Time.now - start_time).round(5)
  puts "Unloading DFE:\t\t\t\t\t#{current_time}s"

  # Get output stream from server
  start_time = Time.now
  c = client.receive_data_int32_t(address_c, size)
  current_time = (Time.now - start_time).round(5)
  puts "Getting output stream:\t(size = #{size * 32} bit)\t#{current_time}s"

  # Free allocated memory for streams on server
  start_time = Time.now
  client.free(address_a)
  client.free(address_b)
  client.free(address_c)
  client.free(actions)
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated memory for streams on server:\t#{current_time}s"

  # Free allocated maxfile data
  start_time = Time.now
  client.VectorAddition_free
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated maxfile data:\t\t\t#{current_time}s"

  # Close!
  start_time = Time.now
  transport.close
  current_time = (Time.now - start_time).round(5)
  puts "Closing connection:\t\t\t\t#{current_time}s"

  current_time = (Time.now - start_dfe_time).round(5)
  puts "DFE vector addition total time:\t\t\t#{current_time}s"

  # CPU Output
  start_time = Time.now
  expected = Array.new(size)
  vector_addition_cpu(size, a, b, scalar, expected)
  current_time = (Time.now - start_time).round(5)
  puts "CPU vector addition total time:\t\t\t#{current_time}s"

  # Checking results
  check(a, b, c, scalar, size)

rescue Thrift::Exception => tx
  print 'Thrift::Exception: ', tx.message, "\n"
  Kernel.exit(-1)
end
