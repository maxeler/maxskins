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

            // Initialize maxfile
            long maxfile = client.LMemLoopback_init();

            // Load DFE
            long engine = client.max_load(maxfile, "*");

            // Allocate and send input streams to server
            long address_inA = client.malloc_int32_t(size);
            client.send_data_int32_t(address_inA, inA);

            long address_inB = client.malloc_int32_t(size);
            client.send_data_int32_t(address_inB, inB);

            // Allocate memory for output stream on server
	    long address_outData = client.malloc_int32_t(size);

	    System.out.println("Loading DFE memory.");
	    LMemLoopback_writeLMem_actions_t_struct actions_lmem_write = new LMemLoopback_writeLMem_actions_t_struct();

	    actions_lmem_write.setParam_address(0);
	    actions_lmem_write.setParam_nbytes(sizeBytes);
	    actions_lmem_write.setInstream_cpu_to_lmem(address_inA);

	    long address_actions_lmem_write = client.send_LMemLoopback_writeLMem_actions_t(actions_lmem_write);
	    client.LMemLoopback_writeLMem_run(engine, address_actions_lmem_write);

	    actions_lmem_write.setParam_address(sizeBytes);
	    actions_lmem_write.setParam_nbytes(sizeBytes);
	    actions_lmem_write.setInstream_cpu_to_lmem(address_inB);	

	    address_actions_lmem_write = client.send_LMemLoopback_writeLMem_actions_t(actions_lmem_write);
	    client.LMemLoopback_writeLMem_run(engine, address_actions_lmem_write);
	    client.free(address_actions_lmem_write);

	    System.out.println("Running DFE.");
	    LMemLoopback_actions_t_struct actions = new LMemLoopback_actions_t_struct();

	    actions.setParam_N(size);

	    long address_actions = client.send_LMemLoopback_actions_t(actions);
	    client.LMemLoopback_run(engine, address_actions);
	    client.free(address_actions);

	    System.out.println("Reading DFE memory.");
	    LMemLoopback_readLMem_actions_t_struct actions_lmem_read = new LMemLoopback_readLMem_actions_t_struct();

	    actions_lmem_read.setParam_address(2 * sizeBytes);
	    actions_lmem_read.setParam_nbytes(sizeBytes);
	    actions_lmem_read.setOutstream_lmem_to_cpu(address_outData);	

	    long address_actions_lmem_read = client.send_LMemLoopback_readLMem_actions_t(actions_lmem_read);
	    client.LMemLoopback_readLMem_run(engine, address_actions_lmem_read);
	    client.free(address_actions_lmem_read);

	    // Unload DFE
	    client.max_unload (engine);

	    // Get output stream from server
	    outData = client.receive_data_int32_t(address_outData, size);

	    // Free allocated memory for streams on server
	    client.free(address_inA);
	    client.free(address_inB);
	    client.free(address_outData);

	    // Free allocated maxfile data
	    client.LMemLoopback_free();

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
