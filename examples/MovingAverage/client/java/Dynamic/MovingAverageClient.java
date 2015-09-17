import com.maxeler.MovingAverage.*;

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

public class MovingAverageClient {
    public static void check (List<Double> dataIn, List<Double> dataOut, int size) {
        int status = 0;

        for (int i=1; i < size - 1; i++) {
            double x = (Double) dataIn.get(i - 1);
            float xf = (float) x;
            double y = (Double) dataIn.get(i);
            float yf = (float) y;
            double z = (Double) dataIn.get(i + 1);
            float zf = (float) z;
            if (dataOut.get(i) != (xf + yf + zf) / 3) {
                System.out.println("Test failed!" + dataOut.get(i) + " ! = " + ((xf + yf + zf) / 3));
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
            MovingAverageService.Client client = new MovingAverageService.Client(protocol);

            final int size = 384;

            List<Double> dataIn = new ArrayList<Double>();
            int sizeBytes = size * 4;

            // Random generatordataOut
            Random rn = new Random();
            int minimum = 0;
            int maximum = 1000;    
            int range = maximum - minimum + 1;

            // Generate input data
            for (int i=0; i<size; i++) {
                dataIn.add((double)rn.nextInt(range) + minimum);
            }
           
            // Initialize maxfile
            long maxfile = client.MovingAverage_init();

            // Load DFE
            long engine = client.max_load(maxfile, "*");

            // Allocate and send input streams to server
            long address_dataIn = client.malloc_float(size);
            client.send_data_float(address_dataIn, dataIn);

            // Allocate memory for output stream on server
            long address_dataOut = client.malloc_float(size);

            // Action default
            System.out.println("Running DFE");
 
            long actions;

            actions = client.max_actions_init(maxfile, "default");
            client.max_set_param_uint64t(actions, "N", size);
            client.max_queue_input(actions, "x", address_dataIn, sizeBytes);
            client.max_queue_output(actions, "y", address_dataOut, sizeBytes);

            client.max_run(engine, actions);
 
            // Unload DFE
            client.max_unload (engine);

            // Get output stream from server
            List<Double> dataOut = client.receive_data_float(address_dataOut, size);
    
            // Free allocated memory for streams on server
            client.free(address_dataIn);
            client.free(address_dataOut);

            // Free allocated maxfile data
            client.MovingAverage_free();
        
            // Checking results
            check(dataIn, dataOut, size);

            transport.close();

        } catch (TException x) {
            x.printStackTrace();
        }
    }
       
}

