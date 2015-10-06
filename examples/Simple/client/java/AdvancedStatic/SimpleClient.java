import com.maxeler.Simple.*;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;

import java.util.List;
import java.util.ArrayList;

public class SimpleClient {
     public static int check (List<Double> dataOutDFE, List<Double> dataOutCPU, int size) {
	 int status = 0;

	 for(int i=0; i < size; i++) {
	     if(!dataOutDFE.get(i).equals(dataOutCPU.get(i))) {
		 System.out.println("Output data @ " + i  + " = " + 
				    dataOutDFE.get(i) + " (expected " +  
				    dataOutCPU.get(i) + ")");
		 status = 1;
	     }
	 }
	 return status;

     }

    public static List<Double> SimpleCPU (int size, List<Double> dataIn) {
	List<Double> dataOut = new ArrayList<Double>();

	for (int i=0 ; i<size ; i++) {
	    dataOut.add(dataIn.get(i)*dataIn.get(i) + dataIn.get(i));
	}

	return dataOut;
    }

    public static List<Double> SimpleDFE (int size, List<Double> dataIn) {
	List<Double> dataOut = new ArrayList<Double>();
	try {
	    // Connect!
	    TTransport transport;
            transport = new TSocket("localhost", 9090);
            transport.open();

            TProtocol protocol = new  TBinaryProtocol(transport);
            SimpleService.Client client = new SimpleService.Client(protocol);

	    // Initialize maxfile
            long maxfile = client.Simple_init();

            // Load DFE
            long engine = client.max_load(maxfile, "*");

	    // Allocate and send input streams to server
            long address_dataIn = client.malloc_float(size);
            client.send_data_float(address_dataIn, dataIn);

	    // Allocate memory for output stream on server
            long address_dataOut = client.malloc_float(size);

	    System.out.println("Running DFE");
	    
	    Simple_actions_t_struct action = new Simple_actions_t_struct(size, address_dataIn, address_dataOut);
            long address_action = client.send_Simple_actions_t(action);
            client.Simple_run(engine, address_action);
 
            // Unload DFE
            client.max_unload (engine);

	    // Get output stream from server
            dataOut = client.receive_data_float(address_dataOut, size);

	    // Free allocated memory for streams on server
            client.free(address_dataIn);
            client.free(address_dataOut);

	    // Free allocated maxfile data
            client.Simple_free();

	    transport.close();

	} catch (TException x) {
            x.printStackTrace();
	    System.exit(-1);
        }

	return dataOut;
    }

    public static void main (String [] args) {
	final int size = 1024;
	List<Double> dataIn = new ArrayList<Double>();

	for(int i = 0; i < size; i++) {
	    dataIn.add((double)(i + 1));
	}

	// CPU Output
	List<Double> dataOutCPU = SimpleCPU(size, dataIn);

	// DFE Output
        List<Double> dataOutDFE = SimpleDFE(size, dataIn);

	// Checking results
	if (check(dataOutDFE, dataOutCPU, size) == 1) {
	    System.out.println("Test failed.");
            System.exit(-1);
	} else {
	    System.out.println("Test passed!");
	}
    }
}
