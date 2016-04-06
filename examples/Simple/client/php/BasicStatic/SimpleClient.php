<?php 
set_include_path(get_include_path() . PATH_SEPARATOR . getenv('PHP_THRIFT_LIB'));

require_once 'Thrift/ClassLoader/ThriftClassLoader.php';

use Thrift\ClassLoader\ThriftClassLoader;

$GEN_DIR = '../gen-php';

$loader = new ThriftClassLoader();
$loader->registerNamespace('Thrift', getenv('PHP_THRIFT_LIB'));
$loader->registerDefinition('com\maxeler\Simple', $GEN_DIR);
$loader->register();

use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TSocket;
use Thrift\Transport\TBufferedTransport;
use Thrift\Exception\TException;

use com\maxeler\Simple\SimpleServiceClient;

function check ($dataOutDFE, $dataOutCPU, $size) {
    $status = 0;

    for($i=0; $i < $size; $i++) {
        if($dataOutDFE[$i] != $dataOutCPU[$i]) {
            echo "Output data @ ".$i." = ".$dataOutDFE[$i]." (expected ".$dataOutCPU[$i].")\r\n";
            $status++;
        }
    }
        return $status;

}

function SimpleCPU ($size, $dataIn) {
    for ($i = 0 ; $i < $size ; $i++) {
        $dataOut[$i] = $dataIn[$i] * $dataIn[$i] + $dataIn[$i];
    }

    return $dataOut;
}

function SimpleDFE ($size, $dataIn) {
    try {
	$startTime = microtime(true);
	
	# Make socket
        $socket = new TSocket('localhost', 9090);

	# Buffering is critical. Raw sockets are very slow
        $transport = new TBufferedTransport($socket);

	# Wrap in a protocol
        $protocol = new TBinaryProtocol($transport);

	# Create a client to use the protocol encoder
        $client = new SimpleServiceClient($protocol);

	echo "Creating a client:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";
	
	# Connect!
        $startTime = microtime(true);
        $transport->open();
	echo "Opening connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

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
        $client->Simple($size, $address_dataIn, $address_dataOut);
	echo "Simple time:\t\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Get output stream from server
	$startTime = microtime(true);
        $dataOut = $client->receive_data_float($address_dataOut, $size);
	echo "Getting output stream:\t(size = ".($size * 32)." bit)\t".(microtime(true) - $startTime)."s\r\n";

        # Free allocated memory for streams on server
	$startTime = microtime(true);
        $client->free($address_dataIn);
        $client->free($address_dataOut);
	echo "Freeing allocated memory for streams on server:\t".(microtime(true) - $startTime)."s\r\n";

	$startTime = microtime(true);
        $transport->close();
	echo "Closing connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

    } catch (TException $tx) {
        echo "ThriftException: ".$tx->getMessage()."\r\n";
        exit(-1);
    }

        return $dataOut;
}

$startTime = microtime(true);
$size = 1024;

for($i = 0; $i < $size; $i++) {
    $dataIn[$i] = $i + 1;
}
echo "Generating input data:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

# DFE Output
$startTime = microtime(true);
$dataOutDFE = SimpleDFE($size, $dataIn);
echo "DFE pass through total time:\t\t\t".(microtime(true) - $startTime)."s\r\n";

# CPU Output
$startTime = microtime(true);
$dataOutCPU = SimpleCPU($size, $dataIn);
echo "CPU pass through total time:\t\t\t".(microtime(true) - $startTime)."s\r\n";

# Checking results
$startTime = microtime(true);
$status = check($dataOutDFE, $dataOutCPU, $size);
echo "Checking results:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

if ($status == 0) { 
    echo "Test successful!\r\n";
} else {
    echo "Test failed ", $status, " times!\r\n";
    exit (-1);
}
