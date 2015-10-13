import com.maxeler.LMemLoopback.*;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;

import java.util.List;
import java.util.ArrayList;

public class LMemLoopbackClient {
    public static int check (int size, List<Integer> outData, List<Integer> inA, List<Integer> inB) {
	 int status = 0;

	 for(int i=0; i < size; i++) {
	     if(!outData.get(i).equals(inA.get(i) + inB.get(i))) {
		 System.out.println("[" + i + "] Verification error, out: " + 
				    outData.get(i) + " != expected: " +  
				    (inA.get(i) + inB.get(i)));
		 status = 1;
	     }
	 }
	 return status;

     }

    public static List<Integer> LMemLoopbackDFE (int size, List<Integer> inA, List<Integer> inB) {
	List<Integer> outData = new ArrayList<Integer>();
	int sizeBytes = size * 4;
	try {
	    // Connect!
	    TTransport transport;
            transport = new TSocket("localhost", 9090);
            transport.open();

            TProtocol protocol = new  TBinaryProtocol(transport);
            LMemLoopbackService.Client client = new LMemLoopbackService.Client(protocol);

	    // Allocate and send input streams to server
	    long address_inA = client.malloc_int32_t(size);
	    client.send_data_int32_t(address_inA, inA);

	    long address_inB = client.malloc_int32_t(size);
	    client.send_data_int32_t(address_inB, inB);

	    // Allocate memory for output stream on server
	    long address_outData = client.malloc_int32_t(size);

	    System.out.println("Loading DFE memory.");
	    client.LMemLoopback_writeLMem(0, sizeBytes, address_inA);
	    client.LMemLoopback_writeLMem(sizeBytes, sizeBytes, address_inB);

	    System.out.println("Running DFE.");
	    client.LMemLoopback(size);

	    System.out.println("Reading DFE memory.");
	    client.LMemLoopback_readLMem(2 * sizeBytes, sizeBytes, address_outData);

	    // Get output stream from server
	    outData = client.receive_data_int32_t(address_outData, size);

	    // Free allocated memory for streams on server
	    client.free(address_inA);
	    client.free(address_inB);
	    client.free(address_outData);

	    // Close!
            transport.close();

        } catch (TException x) {
            x.printStackTrace();
	    System.exit(-1);
        }

	return outData;
    }

    public static void main (String [] args) {
	final int size = 384;
	List<Integer> inA = new ArrayList<Integer>();
	List<Integer> inB = new ArrayList<Integer>();

	for(int i = 0; i < size; i++) {
	    inA.add(i);
	    inB.add(size - i);
	}

	// DFE Output
        List<Integer> outData = LMemLoopbackDFE(size, inA, inB);

        // Checking results
        if (check(size, outData, inA, inB) == 1) {
            System.out.println("Test failed.");
	    System.exit(-1);
	} else {
	    System.out.println("Test passed!");
	}
    }
}
