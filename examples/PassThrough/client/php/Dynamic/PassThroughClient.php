<?php 
set_include_path(get_include_path() . PATH_SEPARATOR . getenv('PHP_THRIFT_LIB'));

require_once 'Thrift/ClassLoader/ThriftClassLoader.php';

use Thrift\ClassLoader\ThriftClassLoader;

$GEN_DIR = '../gen-php';

$loader = new ThriftClassLoader();
$loader->registerNamespace('Thrift', getenv('PHP_THRIFT_LIB'));
$loader->registerDefinition('com\maxeler\PassThrough', $GEN_DIR);
$loader->register();

use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TSocket;
use Thrift\Transport\TBufferedTransport;
use Thrift\Exception\TException;

use com\maxeler\PassThrough\PassThroughServiceClient;

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

function PassThroughCPU ($size, $dataIn) {
    for ($i = 0 ; $i < $size ; $i++) {
        $dataOut[$i] = $dataIn[$i];
    }

    return $dataOut;
}

function PassThroughDFE ($size, $dataIn) {
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
        $client = new PassThroughServiceClient($protocol);
        
        echo "Creating a client:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Connect!
        $startTime = microtime(true);
        $transport->open();
        echo "Opening connection:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";
        
        # Initialize maxfile
        $startTime = microtime(true);
        $maxfile = $client->PassThrough_init();
        echo "Initializing maxfile:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Load DFE
        $startTime = microtime(true);
        $engine = $client->max_load($maxfile, "*");
        echo "Loading DFE:\t\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

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
        $actions = $client->max_actions_init($maxfile, "default");
        $client->max_set_param_uint64t($actions, "N", $size);
        $client->max_queue_input($actions, "x", $address_dataIn, $sizeBytes);
        $client->max_queue_output($actions, "y", $address_dataOut, $sizeBytes);
        $client->max_run($engine, $actions);
        echo "Pass through time:\t\t\t\t".(microtime(true) - $startTime)."s\r\n";
 
        # Unload DFE
        $startTime = microtime(true);
        $client->max_unload($engine);
        echo "Unloading DFE:\t\t\t\t\t".(microtime(true) - $startTime)."s\r\n";

        # Get output stream from server
        $startTime = microtime(true);
        $dataOut = $client->receive_data_float($address_dataOut, $size);
        echo "Getting output stream:\t(size = ".($size * 32)." bit)\t".(microtime(true) - $startTime)."s\r\n";

        # Free allocated memory for streams on server
        $startTime = microtime(true);
        $client->free($address_dataIn);
        $client->free($address_dataOut);
        echo "Freeing allocated memory for streams on server:\t".(microtime(true) - $startTime)."s\r\n";

        # Free allocated maxfile data
        $startTime = microtime(true);
        $client->PassThrough_free();
        echo "Freeing allocated maxfile data:\t\t\t".(microtime(true) - $startTime)."s\r\n";

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
$dataOutDFE = PassThroughDFE($size, $dataIn);
echo "DFE pass through total time:\t\t\t".(microtime(true) - $startTime)."s\r\n";

# CPU Output
$startTime = microtime(true);
$dataOutCPU = PassThroughCPU($size, $dataIn);
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
