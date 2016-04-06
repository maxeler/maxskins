#!/usr/bin/env ruby

$LOAD_PATH.push('../gen-rb')

require 'thrift'
require 'correlation_service'

CORREALTION_MAX_NUM_VARIABLES = 6000
CORREALTION_MAX_NUM_TIMESERIES = 6000
CORREALTION_NUM_TOP_SCORES = 10
CORREALTION_NUM_PIPES = 12
CORREALTION_PCIE_ALIGNMENT = 16
CORREALTION_VECTORS_PER_BURST = 2

def random_data(data, num_timeseries, size_timeseries)
  # Generate input data
  (0..(num_timeseries - 1)).each do |i|
    (0..(size_timeseries - 1)).each do |j|
      data[i][j] = rand(0.0..1.0)
    end
  end
end

def calc_num_bursts(num_timeseries)
  # Calculate number of bursts for initializing LMem.
  num_vectors = 0
  (1..num_timeseries).each do |i|
    num_vectors += (i + (CORREALTION_NUM_PIPES - 1)) / CORREALTION_NUM_PIPES
  end

  ((num_vectors + (CORREALTION_VECTORS_PER_BURST - 1)) /
   CORREALTION_VECTORS_PER_BURST)
end

def prepare_data_for_dfe(data, size_timeseries, num_timeseries,
                         num_timesteps, window_size)
  # Precalculates and reorders data for the DFE.
  if num_timeseries > CORREALTION_MAX_NUM_TIMESERIES
    puts 'Number of Time series should be less or equal to '\
         "#{CORREALTION_MAX_NUM_TIMESERIES}. Terminating!"
    Kernel.exit(-1)
  end

  if window_size < 2
    puts 'Window size must be equal or greater than 2. Terminating!'
    Kernel.exit(-1)
  end

  if num_timesteps > size_timeseries
    puts 'Number of Time steps should be less or equal '\
         'to size of Time series. Terminating!'
    Kernel.exit(-1)
  end

  precalculations = Array.new(2 * num_timeseries * num_timesteps)
  data_pairs = Array.new(2 * num_timeseries * num_timesteps)

  sums = Array.new(size_timeseries, Array.new(num_timeseries))
  sums_sq = Array.new(size_timeseries, Array.new(num_timeseries))
  inv = Array.new(size_timeseries, Array.new(num_timeseries))

  # 2 DFE input streams: precalculations and data pairs
  (0..(num_timesteps - 1)).each do |i|
    (0..(num_timeseries - 1)).each do |j|
      old_val = i > window_size ? data [j][i - window_size] : 0
      new_val = data [j][i]

      if i == 0
        sums [i][j] = new_val
        sums_sq [i][j] = new_val * new_val
      else
        sums[i][j] = sums[i - 1][j] + new_val - old_val
        sums_sq[i][j] = (sums_sq[i - 1][j] + new_val * new_val -
                         old_val * old_val)
      end

      inv[i][j] = 1 / Math.sqrt(window_size * sums_sq[i][j] -
                                sums[i][j] * sums[i][j])

      # Precalculations REORDERED in DFE ORDER
      precalculations[2 * i * num_timeseries + 2 * j] = sums[i][j]
      precalculations[2 * i * num_timeseries + 2 * j + 1] = inv[i][j]

      # Data pairs REORDERED in DFE ORDER
      data_pairs[2 * i * num_timeseries + 2 * j] = new_val
      data_pairs[2 * i * num_timeseries + 2 * j + 1] = old_val
    end
  end
  [precalculations, data_pairs]
end

def calc_num_correlations(num_timeseries)
  # Calculates number  of correlations.
  (num_timeseries * (num_timeseries - 1)) / 2
end

def calc_index(i, j)
  # Calculates index of correlation between i and j
  if i == j
    puts 'i and j must not be the same!'
    -1
  end
  if i < j
    tmp = j
    j = i
    i = tmp
  end

  (i * (i - 1)) / 2 + j
end

def correlate_dfe(data, size_timeseries, num_timeseries, correlations)
  # Calculates correlations on DFE.

  include Com::Maxeler::Correlation

  start_time = Time.now

  port = 9090

  # Make socket
  transport = Thrift::BufferedTransport.new(Thrift::Socket.new('localhost',
                                                               port))

  # Wrap in a protocol
  protocol = Thrift::BinaryProtocol.new(transport)

  # Create a client to use the protocol encoder
  client = CorrelationService::Client.new(protocol)

  current_time = (Time.now - start_time).round(5)
  puts "Creating a client:\t\t\t\t#{current_time}s"

  # Connect!
  start_time = Time.now
  transport.open
  current_time = (Time.now - start_time).round(5)
  puts "Opening connection:\t\t\t\t#{current_time}s"

  num_timesteps = size_timeseries
  window_size = size_timeseries
  num_bursts = calc_num_bursts(num_timeseries)

  # Get loop length
  start_time = Time.now
  loop_length = Array.new(1)
  loop_length[0] = client.correlation_get_CorrelationKernel_loopLength
  current_time = (Time.now - start_time).round(5)
  puts "Getting Correlation Kernel loop_length:\t\t#{current_time}s"

  # Prepare data for DFE
  start_time = Time.now

  burst_size = 384 / 2 # for anything other than ISCA this should be 384
  in_mem_load = Array.new(num_bursts * burst_size, 0)

  precalculations, data_pairs = prepare_data_for_dfe(
    data, size_timeseries, num_timeseries, num_timesteps, window_size)

  current_time = (Time.now - start_time).round(5)
  puts "Data reordering time:\t\t\t\t#{current_time}s"

  # Allocate and send input streams to server
  start_time = Time.now
  loop_length_size = 1
  address_loop_length = client.malloc_int32_t(loop_length_size)
  client.send_data_int32_t(address_loop_length, loop_length)
  loop_length_time = (Time.now - start_time).round(5)
  puts "\tSending loop_length:\t\t(size = #{32 * loop_length_size} bit)"\
       "\t\t#{loop_length_time}s"

  start_time = Time.now
  in_mem_load_size = num_bursts * burst_size
  address_in_mem_load = client.malloc_int32_t(in_mem_load_size)
  client.send_data_int32_t(address_in_mem_load, in_mem_load)
  in_mem_load_time = (Time.now - start_time).round(5)
  puts "\tSending in_mem_load:\t\t(size = #{32 * in_mem_load_size} bit)"\
       "\t#{in_mem_load_time}s"

  start_time = Time.now
  precalculations_size = 2 * num_timeseries * num_timesteps
  address_precalculations = client.malloc_double(precalculations_size)
  client.send_data_double(address_precalculations, precalculations)
  precalculations_time = (Time.now - start_time).round(5)
  puts "\tSending Precalculations:\t"\
       "(size = #{64 * precalculations_size} bit)\t#{precalculations_time}s"

  start_time = Time.now
  data_pairs_size = 2 * num_timeseries * num_timesteps
  address_data_pairs = client.malloc_double(data_pairs_size)
  client.send_data_double(address_data_pairs, data_pairs)
  data_pairs_time = (Time.now - start_time).round(5)
  puts "\tSending data_pairs:\t\t(size = #{64 * data_pairs_size} bit)"\
       "\t#{data_pairs_time}s"

  current_time = (loop_length_time + in_mem_load_time +
                  precalculations_time + data_pairs_time).round(5)
  speed = ((32 * loop_length_size + 32 * in_mem_load_size +
            64 * precalculations_size + 64 * data_pairs_size) /
           (current_time * 1_000_000))
  puts "Sending input streams to server total time:\t#{current_time}s"\
       "\t\t(average speed = #{speed.round(5)}Mb/s)"

  # Allocate memory for output stream on server
  start_time = Time.now
  address_out_correlation = client.malloc_double(num_timesteps *
                                                 loop_length[0] *
                                                 CORREALTION_NUM_TOP_SCORES *
                                                 CORREALTION_NUM_PIPES +
                                                 num_bursts * 24)
  address_out_indices = client.malloc_int32_t(2 * num_timesteps *
                                              loop_length[0] *
                                              CORREALTION_NUM_TOP_SCORES *
                                              CORREALTION_NUM_PIPES)
  current_time = (Time.now - start_time).round(5)
  puts "Allocating memory for output stream on server:\t#{current_time}s"

  # Initialize LMem
  start_time = Time.now

  client.correlation_loadLMem(num_bursts, address_loop_length,
                              address_in_mem_load)

  current_time = (Time.now - start_time).round(5)
  puts "LMem initialization:\t\t\t\t#{current_time}s"

  # Executing correlation action
  start_time = Time.now

  client.correlation(num_bursts,              # scalar input
                     num_timesteps,           # scalar input
                     num_timeseries,          # scalar input
                     1,                       # scalar input
                     window_size,             # scalar input
                     address_precalculations, # streaming reordered input
                     address_data_pairs,      # streaming reordered input
                     address_out_correlation, # streaming unordered output
                     address_out_indices)     # streaming unordered output

  current_time = (Time.now - start_time).round(5)
  puts "Correlation time:\t\t\t\t#{current_time}s"

  # Get output stream from server
  start_time = Time.now
  out_correlation_size = num_timesteps * loop_length[0] *
                         CORREALTION_NUM_TOP_SCORES * CORREALTION_NUM_PIPES +
                         num_bursts * 24
  out_correlation = client.receive_data_double(address_out_correlation,
                                               out_correlation_size)
  out_correlation_time = (Time.now - start_time).round(5)
  puts "\tGet output stream Correlation:\t"\
       "(size = #{64 * out_correlation_size} bit)\t#{out_correlation_time}s"

  start_time = Time.now
  out_indices_size = 2 * num_timesteps * loop_length[0] *
                     CORREALTION_NUM_TOP_SCORES * CORREALTION_NUM_PIPES
  _ = client.receive_data_int32_t(address_out_indices, out_indices_size)
  out_indices_time = (Time.now - start_time).round(5)
  puts "\tGet output stream out_indices:\t"\
       "(size = #{32 * out_indices_size} bit)\t#{out_indices_time}s"

  start_time = Time.now
  loop_length_size = 1
  loop_length = client.receive_data_int32_t(address_loop_length,
                                            loop_length_size)
  loop_length_time = (Time.now - start_time).round(5)
  puts "\tGet output stream loop_length:\t"\
       "(size = #{32 * loop_length_size} bit)\t\t#{loop_length_time}s"

  current_time = (out_correlation_time + out_indices_time +
                  loop_length_time).round(5)
  speed = (64 * out_correlation_size + 32 * out_indices_size +
           32 * loop_length_size) / (current_time * 1_000_000)
  puts "Getting output stream from server total time:\t"\
       "#{current_time}s\t\t(average speed = #{speed.round(5)}Mb/s)"

  # Free allocated memory for streams on server
  start_time = Time.now
  client.free(address_loop_length)
  client.free(address_in_mem_load)
  client.free(address_precalculations)
  client.free(address_data_pairs)
  client.free(address_out_correlation)
  client.free(address_out_indices)
  current_time = (Time.now - start_time).round(5)
  puts "Freeing allocated memory for streams on server:\t#{current_time}s"

  # Close!
  start_time = Time.now
  transport.close
  current_time = (Time.now - start_time).round(5)
  puts "Closing connection:\t\t\t\t#{current_time}s"

  # Store data
  start_time = Time.now

  position = 0
  index = 0
  start = ((num_timesteps - 1) * loop_length[0] *
           CORREALTION_NUM_TOP_SCORES * CORREALTION_NUM_PIPES)

  (0..(num_timeseries - 1)).each do |i|
    (0..(i - 1)).each do |j|
      correlations[index + j] = out_correlation[start + position + j]
    end
    index += i
    position += ((i / 12) + 1) * 12
  end

  current_time = (Time.now - start_time).round(5)
  puts "Storing time:\t\t\t\t\t#{current_time}s"

rescue Thrift::Exception => tx
  puts 'Thrift::Exception: ', tx.message, "\n"
  Kernel.exit(-1)
end

def correlate_cpu(data, num_timeseries, window_size, num_timesteps)
  # Calculates correlations on CPU.
  num_of_correlations = calc_num_correlations(num_timeseries)
  correlations_cpu = Array.new(num_of_correlations)
  indices_step = Array.new(2 * num_of_correlations)

  sums = Array.new(num_timeseries, 0.0)
  sums_sq = Array.new(num_timeseries, 0.0)
  sums_xy = Array.new(calc_num_correlations(num_timeseries), 0.0)

  (0..(num_timesteps - 1)).each do |k|
    index_correlation = 0
    (0..(num_timeseries - 1)).each do |i|
      old_val = k >= window_size ? data[i][k - window_size] : 0
      new_val = data[i][k]
      sums[i] += new_val - old_val
      sums_sq[i] += new_val * new_val - old_val * old_val
    end
    (0..(num_timeseries - 1)).each do |i|
      old_x = k >= window_size ? data[i][k - window_size] : 0
      new_x = data[i][k]
      (0..(i - 1)).each do |j|
        old_y = k >= window_size ? data[j][k - window_size] : 0
        new_y = data[j][k]
        sums_xy[index_correlation] += new_x * new_y - old_x * old_y
        numerator = (window_size * sums_xy[index_correlation] -
                     sums[i] * sums[j])
        denominator = ((1 / Math.sqrt(window_size * sums_sq[i] -
                                      sums[i] * sums[i])) *
                       (1 / Math.sqrt(window_size * sums_sq[j] -
                                      sums[j] * sums[j])))
        correlations_cpu[index_correlation] = numerator * denominator
        indices_step[2 * index_correlation] = j
        indices_step[2 * index_correlation + 1] = i
        index_correlation += 1
      end
    end
  end
  [correlations_cpu, indices_step]
end

def check(correlations_dfe, correlations_cpu, num_timeseries, indices_step)
  # Checks if correlations_dfe and correlations_cpu are the same.
  (0..((num_timeseries * (num_timeseries - 1) / 2) - 1)).each do |i|
    j = calc_index(indices_step[2 * i], indices_step[2 * i + 1])
    if correlations_dfe[j] != correlations_cpu[i]
      puts 'Test failed!'
      Kernel.exit(-1)
    end
  end
  puts 'Test passed!'
end

begin
  if ARGV.length != 2
    puts 'Usage: CorrelationClient.rb <stream size> <number of streams>'
    Kernel.exit(-1)
  else
    size_timeseries = ARGV[0].to_i
    num_timeseries = ARGV[1].to_i
  end

  data = Array.new(num_timeseries, Array.new(size_timeseries))
  num_of_correlations = calc_num_correlations(num_timeseries)
  correlations_dfe = Array.new(num_of_correlations)

  random_data(data, num_timeseries, size_timeseries)

  start_time = Time.now
  correlate_dfe(data, size_timeseries, num_timeseries, correlations_dfe)
  current_time = (Time.now - start_time).round(5)
  puts "DFE correlation total time:\t#{current_time}s"

  start_time = Time.now
  correlations_cpu, indices_step = correlate_cpu(
    data, num_timeseries, size_timeseries, size_timeseries)
  current_time = (Time.now - start_time).round(5)
  puts "CPU correlation total time:\t#{current_time}s"

  check(correlations_dfe, correlations_cpu, num_timeseries, indices_step)
end
