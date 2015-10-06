using System;
using System.Linq;
using System.Net.Sockets;
using System.Collections.Generic;

using Thrift;
using Thrift.Protocol;
using Thrift.Transport;

using com.maxeler.MovingAverage;

internal class MovingAverageClient {
	public static void check (List<double> dataIn, List<double> dataOut, int size) {
		for (var i = 1; i < dataOut.Count - 1; i++) {
			float a = Convert.ToSingle(dataIn[i - 1]);
			float b = Convert.ToSingle(dataIn[i]);
			float c = Convert.ToSingle(dataIn[i + 1]);
			float res = Convert.ToSingle(a+b+c)/3;
			if (dataOut[i] != res) {
				Console.WriteLine("[{0}] {1} != {2}", i, dataOut[i], res);
				Console.WriteLine("Test failed!");
				Environment.Exit(-1);
			}
		}

		Console.WriteLine("Test successful!");
	}

	public static void Main(string[] args) {
		var transport = new TSocket("localhost", 9090);
		var protocol = new TBinaryProtocol(transport);
		var client = new MovingAverageService.Client(protocol);

		try {
			transport.Open();

			const int size = 384;
			int sizeBytes = size * 4;

			List<double> dataIn = new List<double>();

			// Generate input data
			Random random = new Random();
			for(int i = 0; i<size; i++) {
				dataIn.Add(random.Next(0, 100));
			}

			// Initialize maxfile
			var maxfile = client.MovingAverage_init();

			// Load DFE
			var engine = client.max_load(maxfile, "*");

			// Allocate and send input streams to server
			var address_dataIn = client.malloc_float(size);
			client.send_data_float(address_dataIn, dataIn);

			// Allocate memory for output stream on server
			var address_dataOut = client.malloc_float(size);

			Console.WriteLine("Running DFE");
			var actions = client.max_actions_init(maxfile, "default");
			client.max_set_param_uint64t(actions, "N", size);
			client.max_queue_input(actions, "x", address_dataIn, sizeBytes);
			client.max_queue_output(actions, "y", address_dataOut, sizeBytes);

			client.max_run(engine, actions);

			// Unload DFE
			client.max_unload (engine);

			// Get output stream from server
			List<double> dataOut = client.receive_data_float(address_dataOut, size);

			// Free allocated memory for streams on server
			client.free(address_dataIn);
			client.free(address_dataOut);

			// Free allocated maxfile data
			client.MovingAverage_free();

			// Checking results
			check(dataIn, dataOut, size);

			transport.Close();

		} catch (SocketException e) {
			Console.WriteLine("Could not connect to the server: {0}.", e.Message);
			Environment.Exit(-1);
		} catch (Exception e) {
			Console.WriteLine("An error occured: {0}", e.Message);
			Environment.Exit(-1);
		}
	}
}
