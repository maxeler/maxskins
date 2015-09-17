import com.maxeler.VectorAddition.*;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;

import java.util.Random;
import java.util.List;
import java.util.ArrayList;

public class VectorAdditionClient {
    public static void check(List<Integer> x, List<Integer> y, List<Integer> s, int scalar, int size) {
        int status = 0;

        for (int i=0; i<size; i++) {
            if (s.get(i) != x.get(i) + y.get(i) + scalar) {
                System.out.println("Test failed!");
                status = 1;
                break;
            }
        }

        if (status==0)
            System.out.println("Test successful!");
    }

    public static void main (String [] args) {

        try {

            TTransport transport;
            transport = new TSocket("localhost", 9090);
            transport.open();

            TProtocol protocol = new  TBinaryProtocol(transport);
            VectorAdditionService.Client client = new VectorAdditionService.Client(protocol);

            final int size = 384;

            List<Integer> x = new ArrayList<Integer>();
            List<Integer> y = new ArrayList<Integer>();
            final int scalar = 3;

            // Random generator
            Random rn = new Random();
            int minimum = 0;
            int maximum = 1000;    
            int range = maximum - minimum + 1;

            // Generate input data
            for (int i=0; i<size; i++) {
                x.add(rn.nextInt(range) + minimum);
                y.add(rn.nextInt(range) + minimum);
            }
           
            // Allocate and send input streams to server
            long address_x = client.malloc_int32_t(size);
            client.send_data_int32_t(address_x, x);

            long address_y = client.malloc_int32_t(size);
            client.send_data_int32_t(address_y, y);

            // Allocate memory for output stream on server
            long address_s = client.malloc_int32_t(size);

            // Action writeLMem
            System.out.println("Writing to LMem.");
            client.VectorAddition_writeLMem(0,size*4,address_x);

            // Action default
            System.out.println("Running on DFE.");
            client.VectorAddition(scalar, size, address_y, address_s);
 
            // Get output stream from server
            List<Integer> s = client.receive_data_int32_t(address_s, size);
    
            // Free allocated memory for streams on server
            client.free(address_x);
            client.free(address_y);
            client.free(address_s);
        
            // Checking results
            check(x, y, s, scalar, size);

            transport.close();

        } catch (TException x) {
            x.printStackTrace();
        }
    }
       
}


