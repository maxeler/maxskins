#!/usr/bin/env ruby

$LOAD_PATH.push('../gen-rb')

require 'thrift'
require 'l_mem_loopback_service'

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

def lmem_loopback_cpu(in_a, in_b, size, data_out)
  (0..(size - 1)).each do |i|
    data_out[i] = (in_a[i] + in_b[i])
  end
end

begin

  include Com::Maxeler::LMemLoopback

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
  client = LMemLoopbackService::Client.new(protocol)
  current_time = (Time.now - start_time).round(5)
  puts "Creating a client:\t\t\t\t#{current_time}s"

  # Connect!
  start_time = Time.now
  transport.open
  current_time = (Time.now - start_time).round(5)
  puts "Opening connection:\t\t\t\t#{current_time}s"

  size = 384
  size_bytes = size * 4

  # Generate input
  start_time = Time.now
  in_a = Array.new(size)
  in_b = Array.new(size)

  (0..(size - 1)).each do |i|
    in_a[i] = i
    in_b[i] = size - i
  end
  current_time = (Time.now - start_time).round(5)
  puts "Generating input data:\t\t\t\t#{current_time}s"

  # Initialize maxfile
  start_time = Time.now
  max_file = client.LMemLoopback_init
  current_time = (Time.now - start_time).round(5)
  puts "Initializing maxfile:\t\t\t\t#{current_time}s"

  # Load DFE
  start_time = Time.now
  max_engine = client.max_load(max_file, '*')
  current_time = (Time.now - start_time).round(5)
  puts "Loading DFE:\t\t\t\t\t#{current_time}s"

  # Allocate and send input streams to server
  start_time = Time.now
  address_in_a = client.malloc_int32_t(size)
  client.send_data_int32_t(address_in_a, in_a)

  address_in_b = client.malloc_int32_t(size)
  client.send_data_int32_t(address_in_b, in_b)
  current_time = (Time.now - start_time).round(5)
  puts "Allocating memory for output stream on server:\t#{current_time}s"

  # Allocate memory for output stream on server
  start_time = Time.now
  address_out_data = client.malloc_int32_t(size)
  current_time = (Time.now - start_time).round(5)
  puts "Sending input data:\t\t\t\t#{current_time}s"

  # Write to LMem
  start_time = Time.now
  actions = client.max_actions_init(max_file, 'writeLMem')
  client.max_set_param_uint64t(actions, 'address', 0)
  client.max_set_param_uint64t(actions, 'nbytes', size_bytes)
  client.max_queue_input(actions, 'cpu_to_lmem', address_in_a, size_bytes)

  client.max_run(max_engine, actions)

  actions = client.max_actions_init(max_file, 'writeLMem')
  client.max_set_param_uint64t(actions, 'address', size_bytes)
  client.max_set_param_uint64t(actions, 'nbytes', size_bytes)
  client.max_queue_input(actions, 'cpu_to_lmem', address_in_b, size_bytes)

  client.max_run(max_engine, actions)
  current_time = (Time.now - start_time).round(5)
  puts "Writing to LMem:\t\t\t\t#{current_time}s"

  # Action default
  start_time = Time.now
  actions = client.max_actions_init(max_file, 'default')
  client.max_set_param_uint64t(actions, 'N', size)

  client.max_run(max_engine, actions)
  current_time = (Time.now - start_time).round(5)
  puts "LMemLoopback time:\t\t\t\t#{current_time}s"

  # Read from LMem
  start_time = Time.now
  actions = client.max_actions_init(max_file, 'readLMem')
  client.max_set_param_uint64t(actions, 'address', 2 * size_bytes)
  client.max_set_param_uint64t(actions, 'nbytes', size_bytes)
  client.max_queue_output(actions, 'lmem_to_cpu', address_out_data, size_bytes)

  client.max_run(max_engine, actions)
  current_time = (Time.now - start_time).round(5)
  puts "Reading from LMem:\t\t\t\t#{current_time}s"

  # Unload DFE
  start_time = Time.now
  client.max_unload(max_engine)
  current_time = (Time.now - start_time).round(5)
  puts "Unloading DFE:\t\t\t\t\t#{current_time}s"

  # Get output stream from server
  start_time = Time.now
  out_data = client.receive_data_int32_t(address_out_data, size)
  current_time = (Time.now - start_time).round(5)
  puts "Getting output stream:\t(size = #{size * 32} bit)\t#{current_time}s"

  # Free allocated memory for streams on server
  start_time = Time.now
  client.free(address_in_a)
  client.free(address_in_b)
  client.free(address_out_data)
  client.free(actions)
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated memory for streams on server:\t#{current_time}s"

  # Free allocated maxfile data
  start_time = Time.now
  client.LMemLoopback_free
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated maxfile data:\t\t\t#{current_time}s"

  # Close!
  start_time = Time.now
  transport.close
  current_time = (Time.now - start_time).round(5)
  puts "Closing connection:\t\t\t\t#{current_time}s"

  current_time = (Time.now - start_dfe_time).round(5)
  puts "DFE LMemLoopback total time:\t\t\t#{current_time}s"

  # CPU Output
  start_time = Time.now
  expected = Array.new(size)
  lmem_loopback_cpu(in_b, in_a, size, expected)
  current_time = (Time.now - start_time).round(5)
  puts "CPU LMemLoopback total time:\t\t\t#{current_time}s"

  # Checking results
  start_time = Time.now
  status = check(out_data, expected, size)
  current_time = (Time.now - start_time).round(5)
  puts "Checking results:\t\t\t\t#{current_time}s"

  if status == 0
    puts 'Test passed!'
  else
    puts "Test failed #{status} times!"
    Kernel.exit(-1)
  end

rescue Thrift::Exception => tx
  puts 'Thrift::Exception: ', tx.message, '\n'
  Kernel.exit(-1)
end
