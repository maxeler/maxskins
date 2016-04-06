<?php 
set_include_path(get_include_path() . PATH_SEPARATOR . getenv('PHP_THRIFT_LIB'));

require_once 'Thrift/ClassLoader/ThriftClassLoader.php';

use Thrift\ClassLoader\ThriftClassLoader;

$GEN_DIR = '../gen-php';

$loader = new ThriftClassLoader();
$loader->registerNamespace('Thrift', getenv('PHP_THRIFT_LIB'));
$loader->registerDefinition('com\maxeler\VectorAddition', $GEN_DIR);
$loader->register();

use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TSocket;
use Thrift\Transport\TBufferedTransport;
use Thrift\Exception\TException;

use com\maxeler\VectorAddition\VectorAdditionServiceClient;

function check($dataOutDFE, $dataOutCPU, $size) {
    $status = 0;

    for($i=0; $i < $size; $i++) {
        if($dataOutDFE[$i] != $dataOutCPU[$i]) {
            echo "Output data @ ".$i." = ".$dataOutDFE[$i]." (expected ".$dataOutCPU[$i].")\r\n";
            $status++;
        }
    }
        return $status;
}

function VectorAdditionCPU ($x, $y, $scalar, $size) {
    for ($i = 0 ; $i < $size ; $i++) {
        $dataOut[$i] = $x[$i] + $y[$i] + $scalar;
    }

    return $dataOut;
}

function VectorAdditionDFE ($x, $y, $scalar, $size) {
    try {
	$startTime = microtime(true);
	
	# Make socket
        $socket = new TSocket('localhost', 9090);

	# Buffering is critical. Raw sockets are very slow
        $transport = new TBufferedTransport($socket);

	# Wrap in a protocol
        $protocol = new TBinaryProtocol($transport);

	# Create a client to use the protocol encoder
        $client = new VectorAdditionServiceClient($protocol);

	echo "Creating a client:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";
	
	# Connect!
        $startTime = microtime(true);
        $transport->open();
	echo "Opening connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Allocate and send input streams to server
	$startTime = microtime(true);
	$address_x = $client->malloc_int32_t($size);
	$client->send_data_int32_t($address_x, $x);

	$address_y = $client->malloc_int32_t($size);
	$client->send_data_int32_t($address_y, $y);
	echo "Sending input data:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Allocate memory for output stream on server
	$startTime = microtime(true);
	$address_s = $client->malloc_int32_t($size);
	echo "Allocating memory for output stream on server:\t".(microtime(true) - $startTime)."s\r\n";

	# Action writeLMem
	$startTime = microtime(true);
	$client->VectorAddition_writeLMem(0, $size * 4, $address_x);
	echo "Writing to LMem:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";
	
	# Action default
        $startTime = microtime(true);
	$client->VectorAddition($scalar, $size, $address_y, $address_s);
	echo "Vector addition time:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Get output stream from server
	$startTime = microtime(true);
	$s = $client->receive_data_int32_t($address_s, $size);
	echo "Getting output stream:\t(size = ".($size * 32)." bit)\t".(microtime(true) - $startTime)."s\r\n";

        # Free allocated memory for streams on server
	$startTime = microtime(true);
	$client->free($address_x);
	$client->free($address_y);
	$client->free($address_s);
	echo "Freeing allocated memory for streams on server:\t".(microtime(true) - $startTime)."s\r\n";

        # Close
	$startTime = microtime(true);
        $transport->close();
	echo "Closing connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

    } catch (TException $tx) {
        echo "ThriftException: ".$tx->getMessage()."\r\n";
        exit(-1);
    }

        return $s;
}

# Generate input data
$startTime = microtime(true);
$size = 384;

$x = array();
$y = array();
$scalar = 3;

for($i = 0; $i < $size; $i++) {
    $x[$i] = rand(0, 1000);
    $y[$i] = rand(0, 1000);
}
echo "Generating input data:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

# DFE Output
$startTime = microtime(true);
$dataOutDFE = VectorAdditionDFE($x, $y, $scalar, $size);
echo "DFE vector addition total time:\t\t\t".(microtime(true) - $startTime)."s\r\n";

# CPU Output
$startTime = microtime(true);
$dataOutCPU = VectorAdditionCPU($x, $y, $scalar, $size);
echo "CPU vector addition time:\t\t\t".(microtime(true) - $startTime)."s\r\n";

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
?>
