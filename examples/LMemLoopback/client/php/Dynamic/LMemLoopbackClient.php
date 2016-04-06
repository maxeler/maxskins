<?php 
set_include_path(get_include_path() . PATH_SEPARATOR . getenv('PHP_THRIFT_LIB'));

require_once 'Thrift/ClassLoader/ThriftClassLoader.php';

use Thrift\ClassLoader\ThriftClassLoader;

$GEN_DIR = '../gen-php';

$loader = new ThriftClassLoader();
$loader->registerNamespace('Thrift', getenv('PHP_THRIFT_LIB'));
$loader->registerDefinition('com\maxeler\LMemLoopback', $GEN_DIR);
$loader->register();

use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TSocket;
use Thrift\Transport\TBufferedTransport;
use Thrift\Exception\TException;

use com\maxeler\LMemLoopback\LMemLoopbackServiceClient;

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

function LMemLoopbackCPU ($size, $inA, $inB) {
    for ($i = 0 ; $i < $size ; $i++) {
        $dataOut[$i] = $inA[$i] + $inB[$i];
    }

    return $dataOut;
}

function LMemLoopbackDFE($size, $inA, $inB) {
    $sizeBytes = $size * 4;
    try {
	$startTime = microtime(true);
	# Make socket
        $socket = new TSocket('localhost', 9090);

	# Buffering is critical. Raw sockets are very slow
        $transport = new TBufferedTransport($socket);

	# Wrap in a protocol
        $protocol = new TBinaryProtocol($transport);

	# Create a client to use the protocol encoder
        $client = new LMemLoopbackServiceClient($protocol);

	echo "Creating a client:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Connect!
        $startTime = microtime(true);
        $transport->open();
	echo "Opening connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Initialize maxfile
	$startTime = microtime(true);
        $maxfile = $client->LMemLoopback_init();
	echo "Initializing maxfile:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Load DFE
	$startTime = microtime(true);
        $engine = $client->max_load($maxfile, "*");
	echo "Loading DFE connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Allocate and send input streams to server
	$startTime = microtime(true);
        $address_inA = $client->malloc_int32_t($size);
        $client->send_data_int32_t($address_inA, $inA);

        $address_inB = $client->malloc_int32_t($size);
        $client->send_data_int32_t($address_inB, $inB);
	echo "Sending input data:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Allocate memory for output stream on server
	$startTime = microtime(true);
        $address_outData = $client->malloc_int32_t($size);
	echo "Allocating memory for output stream on server:\t".(microtime(true) - $startTime)."s\r\n";

        # Writing to LMem
	$startTime = microtime(true);
        $actions = $client->max_actions_init($maxfile, "writeLMem");
        $client->max_set_param_uint64t($actions, "address", 0);
        $client->max_set_param_uint64t($actions, "nbytes", $sizeBytes);
        $client->max_queue_input($actions, "cpu_to_lmem", $address_inA, $sizeBytes);

        $client->max_run($engine, $actions);

        $actions = $client->max_actions_init($maxfile, "writeLMem");
        $client->max_set_param_uint64t($actions, "address", $sizeBytes);
        $client->max_set_param_uint64t($actions, "nbytes", $sizeBytes);
        $client->max_queue_input($actions, "cpu_to_lmem", $address_inB, $sizeBytes);

        $client->max_run($engine, $actions);
	echo "Writing to LMem:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Action default
	$startTime = microtime(true);
        $actions = $client->max_actions_init($maxfile, "default");
        $client->max_set_param_uint64t($actions, "N", $size);

        $client->max_run($engine, $actions);
	echo "LMemLoopback time:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Reading from LMem
	$startTime = microtime(true);
        $actions = $client->max_actions_init($maxfile, "readLMem");
        $client->max_set_param_uint64t($actions, "address", 2 * $sizeBytes);
        $client->max_set_param_uint64t($actions, "nbytes", $sizeBytes);
        $client->max_queue_output($actions, "lmem_to_cpu", $address_outData, $sizeBytes);       

        $client->max_run($engine, $actions);
        $client->free($actions);
	echo "Reading from LMem:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Unload DFE
	$startTime = microtime(true);
        $client->max_unload($engine);
	echo "Unloading DFE:\t\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Get output stream from server
	$startTime = microtime(true);
        $outData = $client->receive_data_int32_t($address_outData, $size);
	echo "Getting output stream:\t(size = ".($size * 32)." bit)\t".(microtime(true) - $startTime)."s\r\n";

        # Free allocated memory for streams on server
	$startTime = microtime(true);
        $client->free($address_inA);
        $client->free($address_inB);
        $client->free($address_outData);
	echo "Freeing allocated memory for streams on server:\t".(microtime(true) - $startTime)."s\r\n";

        # Free allocated maxfile data
	$startTime = microtime(true);
        $client->LMemLoopback_free();
	echo "Freeing allocated maxfile data:\t\t\t".(microtime(true) - $startTime)."s\r\n";
            
        # Close!
	$startTime = microtime(true);
        $transport->close();
	echo "Closing connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

    } catch (TException $tx) {
        echo "ThriftException: ".$tx->getMessage()."\r\n";
        exit(-1);
    }
    
    return $outData;
}

# Generate input data
$startTime = microtime(true);
$size = 384;

for($i = 0; $i < $size; $i++) {
    $inA[$i] = $i;
    $inB[$i] = $size - $i;
}
echo "Generating input data:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

# DFE Output
$startTime = microtime(true);
$dataOutDfe = LMemLoopbackDFE($size, $inA, $inB);
echo "LMemLoopback DFE total time:\t\t\t".(microtime(true) - $startTime)."s\r\n";

# CPU Output
$startTime = microtime(true);
$dataOutCpu = LMemLoopbackCPU($size, $inA, $inB);
echo "LMemLoopback CPU total time:\t\t\t".(microtime(true) - $startTime)."s\r\n";

# Checking results
$startTime = microtime(true);
$status = check($dataOutDfe, $dataOutCpu, $size);
echo "Checking results:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

if ($status == 0) { 
    echo "Test successful!\r\n";
} else {
    echo "Test failed ", $status, " times!\r\n";
    exit (-1);
}
