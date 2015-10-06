import com.maxeler.PassThrough.*;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;

import java.util.List;
import java.util.ArrayList;

public class PassThroughClient {
     public static int check (List<Integer> dataOutDFE, List<Integer> dataOutCPU, int size) {
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

    public static List<Integer> PassThroughCPU (int size, List<Integer> dataIn) {
	List<Integer> dataOut = new ArrayList<Integer>();

	for (int i=0 ; i<size ; i++) {
	    dataOut.add(dataIn.get(i));
	}

	return dataOut;
    }

    public static List<Integer> PassThroughDFE (int size, List<Integer> dataIn) {
	List<Integer> dataOut = new ArrayList<Integer>();
	try {
	    // Connect!
	    TTransport transport;
            transport = new TSocket("localhost", 9090);
            transport.open();

            TProtocol protocol = new  TBinaryProtocol(transport);
            PassThroughService.Client client = new PassThroughService.Client(protocol);

	    // Allocate and send input streams to server
            long address_dataIn = client.malloc_int32_t(size);
            client.send_data_int32_t(address_dataIn, dataIn);

	    // Allocate memory for output stream on server
            long address_dataOut = client.malloc_int32_t(size);

	    System.out.println("Running DFE");
	    client.PassThrough(size, address_dataIn, address_dataOut);

	    // Get output stream from server
            dataOut = client.receive_data_int32_t(address_dataOut, size);

	    // Free allocated memory for streams on server
            client.free(address_dataIn);
            client.free(address_dataOut);

	    transport.close();

	} catch (TException x) {
            x.printStackTrace();
	    System.exit(-1);
        }

	return dataOut;
    }

    public static void main (String [] args) {
	final int size = 1024;
	List<Integer> dataIn = new ArrayList<Integer>();

	for(int i = 0; i < size; i++) {
	    dataIn.add(i + 1);
	}

	// CPU Output
	List<Integer> dataOutCPU = PassThroughCPU(size, dataIn);

	// DFE Output
        List<Integer> dataOutDFE = PassThroughDFE(size, dataIn);

	// Checking results
	if (check(dataOutDFE, dataOutCPU, size) == 1) {
	    System.out.println("Test failed.");
            System.exit(-1);
	} else {
	    System.out.println("Test passed!");
	}
    }
}
