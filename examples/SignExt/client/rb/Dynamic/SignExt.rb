#!/usr/bin/env ruby

$LOAD_PATH.push('../gen-rb')

require 'thrift'
require 'sign_ext_service'

begin

  include Com::Maxeler::SignExt

  port = 9090

  # Make socket
  socket = Thrift::Socket.new('localhost', port)

  # Buffering is critical. Raw sockets are very slow
  transport = Thrift::BufferedTransport.new(socket)

  # Wrap in a protocol
  protocol = Thrift::BinaryProtocol.new(transport)

  # Create a client to use the protocol encoder
  client = SignExtService::Client.new(protocol)

  # Connect!
  transport.open

  if ARGV.length != 2
    puts 'Usage: SignExt.rb dfe_ip remote_ip'
    Kernel.exit(-1)
  end

  dfe_ip_address = client.malloc_int64_t(5)
  client.inet_aton(ARGV[0], dfe_ip_address)

  remote_ip_address = client.malloc_int64_t(5)
  client.inet_aton(ARGV[1], remote_ip_address)

  netmask_address = client.malloc_int64_t(5)
  client.inet_aton('255.255.255.0', netmask_address)

  port = 2000

  # Initialize maxfile
  maxfile = client.SignExt_init

  # Load DFE
  engine = client.max_load(maxfile, '*')

  enumkey = Com::Maxeler::SignExt::Max_config_key_bool_t_struct.new
  enumkey.type = Com::Maxeler::SignExt::Max_config_key_bool_t_enum::
                 MAX_CONFIG_PRINTF_TO_STDOUT
  client.max_config_set_bool(enumkey, 1)

  actions = client.max_actions_init(maxfile, 'default')

  client.max_run(engine, actions)
  client.max_actions_free(actions)

  buffer_address = client.malloc_int64_t(1)
  buffer_size = 4096 * 512
  client.posix_memalign(buffer_address, 4096, buffer_size)

  buffer = client.receive_data_int64_t(buffer_address, 1)[0]

  to_cpu = client.max_framed_stream_setup(engine, 'toCPU',
                                          buffer, buffer_size, -1)

  enumconn = Com::Maxeler::SignExt::Max_net_connection_t_struct.new
  enumconn.type = Com::Maxeler::SignExt::Max_net_connection_t_enum::
                  MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1
  client.max_ip_config(engine, enumconn, dfe_ip_address, netmask_address)
  dfe_socket = client.max_udp_create_socket(engine, 'udpTopPort1')
  client.max_udp_bind(dfe_socket, port)
  client.max_udp_connect(dfe_socket, remote_ip_address, 0)

  puts "Listening on #{ARGV[0]} port #{port}"

  puts 'Waiting for kernel response...'

  f_address = client.malloc_int64_t(1)
  fsz_address = client.malloc_int64_t(1)
  num_message_rx = 0
  cond = true

  while cond
    if client.max_framed_stream_read(to_cpu, 1, f_address, fsz_address) == 1
      num_message_rx += 1

      fsz = client.receive_data_int64_t(fsz_address, 1)[0]
      puts "CPU: Got output frame #{num_message_rx} - size #{fsz} bytes"

      f = client.receive_data_int64_t(f_address, 1)[0]

      w = client.receive_data_int64_t(f, 3)

      (0..2).each do |i|
        wp = w[i] < 0 ? w[i] + 2**64 : w[i]
        puts "Frame [#{num_message_rx}] Word[#{i}]: 0x#{wp.to_s(16)}"
      end

      client.max_framed_stream_discard(to_cpu, 1)

      cond = false if w[0] == 0 && w[1] == 0 && w[2] == 0
    else
      sleep 1 / 100_000
    end
  end

  client.max_udp_close(dfe_socket)
  client.max_framed_stream_release(to_cpu)
  client.max_unload(engine)
  client.max_file_free(maxfile)
  client.free(dfe_ip_address)
  client.free(remote_ip_address)
  client.free(netmask_address)
  client.free(buffer_address)
  client.free(f_address)
  client.free(fsz_address)
  client.SignExt_free

  puts 'Done.'

  # Close!
  transport.close
  Kernel.exit(0)
rescue Thrift::Exception => thrift_exception
  puts 'Thrift::Exception: ', thrift_exception.message, "\n"
  Kernel.exit(-1)
end
