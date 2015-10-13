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
	for($i = 1; $i < $size - 1; $i++) {
		$a = (float) $dataIn[$i - 1];
		$b = (float) $dataIn[$i];
		$c = (float) $dataIn[$i + 1];
		$res = (float)(($a + $b + $c) / 3);
		if(abs($dataOut[$i] - $res) > 0.00001) {
			echo "[".$i."] ".$dataOut[$i]." != ".$res."\r\n";
			echo "Test failed!\r\n";
			exit(-1);
		}
	}
	echo "Test successful!\r\n";
}


try {
	$socket = new TSocket('localhost', 9090);
	$transport = new TBufferedTransport($socket);
	$protocol = new TBinaryProtocol($transport);
	$client = new MovingAverageServiceClient($protocol);

	$transport->open();

	$size = 384;

	# Generate input data
	$dataIn = array();
	for($i = 0; $i < $size; $i++) {
		$dataIn[$i] = rand(0, 100);
	}

	# Allocate and send input streams to server
	$address_dataIn = $client->malloc_float($size);
	$client->send_data_float($address_dataIn, $dataIn);

	# Allocate memory for output stream on server
	$address_dataOut = $client->malloc_float($size);

	echo "Running DFE\r\n";
	$client->MovingAverage($size, $address_dataIn, $address_dataOut);

	# Get output stream from server
	$dataOut = $client->receive_data_float($address_dataOut, $size);

	# Free allocated memory for streams on server
	$client->free($address_dataIn);
	$client->free($address_dataOut);

	# Checking results
	check($dataIn, $dataOut, $size);

	$transport->close();
	
} catch (TException $tx) {
	echo "ThriftException: ".$tx->getMessage()."\r\n";
	exit(-1);
}

?>
