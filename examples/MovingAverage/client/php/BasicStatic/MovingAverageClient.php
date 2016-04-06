<?php 
set_include_path(get_include_path() . PATH_SEPARATOR . getenv('PHP_THRIFT_LIB'));

require_once 'Thrift/ClassLoader/ThriftClassLoader.php';

use Thrift\ClassLoader\ThriftClassLoader;

$GEN_DIR = '../gen-php';

$loader = new ThriftClassLoader();
$loader->registerNamespace('Thrift', getenv('PHP_THRIFT_LIB'));
$loader->registerDefinition('com\maxeler\MovingAverage', $GEN_DIR);
$loader->register();

use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TSocket;
use Thrift\Transport\TBufferedTransport;
use Thrift\Exception\TException;

use com\maxeler\MovingAverage\MovingAverageServiceClient;

function check($dataIn, $dataOut, $size) {
        $status = 0;
        for($i = 1; $i < $size - 1; $i++) {
                $a = (float) $dataIn[$i - 1];
                $b = (float) $dataIn[$i];
                $c = (float) $dataIn[$i + 1];
                $res = (float)(($a + $b + $c) / 3);
                if(abs($dataOut[$i] - $res) > 0.00001) {
                        $status++;
                        echo "Output data @ ".$i." = ".$dataOut[$i]." (expected ".$res.")\r\n";
                }
        }
        return $status;
}

try {
        $startTime = microtime(true);
        $startDFETime = $startTime;

        # Make socket
        $socket = new TSocket('localhost', 9090);

        # Buffering is critical. Raw sockets are very slow
        $transport = new TBufferedTransport($socket);

        # Wrap in a protocol
        $protocol = new TBinaryProtocol($transport);

        # Create a client to use the protocol encoder
        $client = new MovingAverageServiceClient($protocol);
        
        echo "Creating a client:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Connect!
        $startTime = microtime(true);
        $transport->open();
        echo "Opening connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        $startTime = microtime(true);
        $size = 384;
        # Generate input data
        $dataIn = array();
        for($i = 0; $i < $size; $i++) {
                $dataIn[$i] = rand(0, 100);
        }
        echo "Generating input data:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Allocate and send input streams to server
        $startTime = microtime(true);
        $address_dataIn = $client->malloc_float($size);
        $client->send_data_float($address_dataIn, $dataIn);
        echo "Sending input data:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Allocate memory for output stream on server
        $startTime = microtime(true);
        $address_dataOut = $client->malloc_float($size);
        echo "Allocating memory for output stream on server:\t".(microtime(true) - $startTime)."s\r\n";

        # Action default
        $startTime = microtime(true);
        $client->MovingAverage($size, $address_dataIn, $address_dataOut);
        echo "Moving average time:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Get output stream from server
        $startTime = microtime(true);
        $dataOut = $client->receive_data_float($address_dataOut, $size);
        echo "Getting output stream:\t(size = ".($size * 32)." bit)\t".(microtime(true) - $startTime)."s\r\n";

        # Free allocated memory for streams on server
        $startTime = microtime(true);
        $client->free($address_dataIn);
        $client->free($address_dataOut);
        echo "Freeing allocated memory for streams on server:\t".(microtime(true) - $startTime)."s\r\n";

        # Checking results
        $startTime = microtime(true);
        $status = check($dataIn, $dataOut, $size);
        echo "Checking results:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        $startTime = microtime(true);
        $transport->close();
        echo "Closing connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        echo "DFE moving average total time:\t\t\t".(microtime(true) - $startDFETime)."s\r\n";

        if ($status == 0) { 
                echo "Test successful!\r\n";
        } else {
                echo "Test failed ", $status, " times!\r\n";
                exit (-1);
        }

} catch (TException $tx) {
        echo "ThriftException: ".$tx->getMessage()."\r\n";
        exit(-1);
}

?>
