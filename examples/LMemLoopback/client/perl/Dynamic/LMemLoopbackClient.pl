#!/usr/bin/perl -w

use strict;
use warnings;

use lib '../gen-perl';

use Thrift;
use Thrift::BinaryProtocol;
use Thrift::Socket;
use Thrift::BufferedTransport;

use com::maxeler::LMemLoopback::LMemLoopbackService;
use com::maxeler::LMemLoopback::Types;

use Data::Dumper;

use Time::HiRes qw/ time /;

sub check {
    my($dataOutDFE, $dataOutCPU, $size) = @_;
    my $status = 0;

    for(my $i = 0; $i < $size; $i++) {
        if($$dataOutDFE[$i] != $$dataOutCPU[$i]) {
            print "Output data @ ".$i." = ".$$dataOutDFE[$i]." (expected ".$$dataOutCPU[$i].")\r\n";
            $status++;
        }
    }
    return $status;    
}

sub LMemLoopbackCPU {
    my ($size, $inA, $inB) = @_;
    my @dataOut = ();

    for (my $i = 0; $i < $size; $i++) {
        $dataOut[$i] = $$inA[$i] + $$inB[$i];
    }

    return \@dataOut;
}
sub LMemLoopbackDFE {
    my ($size, $inA, $inB) = @_;
    my @outData = ();
    my $sizeBytes = $size * 4;
    eval {
	my $startTime = time;
        # Make socket
        my $socket    = new Thrift::Socket('localhost', 9090);

        # Buffering is critical. Raw sockets are very slow
        my $transport = new Thrift::BufferedTransport($socket);

        # Wrap in a protocol
        my $protocol  = new Thrift::BinaryProtocol($transport);

        # Create a client to use the protocol encoder
        my $client    = new com::maxeler::LMemLoopback::LMemLoopbackServiceClient($protocol);
	print "Creating a client:\t\t\t\t", (time - $startTime), "s\n";

        # Connect!
	$startTime = time;
        $transport->open();
	print "Opening connection:\t\t\t\t", (time - $startTime), "s\n";

        # Initialize maxfile
	$startTime = time;
        my $max_file = $client->LMemLoopback_init();
	print "Initializing maxfile:\t\t\t\t", (time - $startTime), "s\n";

        # Load DFE
	$startTime = time;
        my $engine = $client->max_load($max_file, '*');
	print "Loading DFE:\t\t\t\t\t", (time - $startTime), "s\n";  

        # Allocate and send input streams to server
	$startTime = time;
        my $address_inA = $client->malloc_int32_t($size);
        $client->send_data_int32_t($address_inA, $inA);

        my $address_inB = $client->malloc_int32_t($size);
        $client->send_data_int32_t($address_inB, $inB);
	print "Sending input data:\t\t\t\t", (time - $startTime), "s\n";

        # Allocate memory for output stream on server
	$startTime = time;
        my $address_outData = $client->malloc_int32_t($size);
        print "Allocating memory for output stream on server:\t", (time - $startTime), "s\n";

        # Write to LMem
	$startTime = time;
        my $actions = $client->max_actions_init($max_file, "writeLMem");
        $client->max_set_param_uint64t($actions, "address", 0);
        $client->max_set_param_uint64t($actions, "nbytes", $sizeBytes);
        $client->max_queue_input($actions, "cpu_to_lmem", $address_inA, $sizeBytes);

        $client->max_run($engine, $actions);

        $actions = $client->max_actions_init($max_file, "writeLMem");
        $client->max_set_param_uint64t($actions, "address", $sizeBytes);
        $client->max_set_param_uint64t($actions, "nbytes", $sizeBytes);
        $client->max_queue_input($actions, "cpu_to_lmem", $address_inB, $sizeBytes);

        $client->max_run($engine, $actions);
	print "Writing to LMem:\t\t\t\t", (time - $startTime), "s\n";

        # Action default
	$startTime = time;
        $actions = $client->max_actions_init($max_file, "default");
        $client->max_set_param_uint64t($actions, "N", $size);

        $client->max_run($engine, $actions);
	print "LMemLoopback time:\t\t\t\t", (time - $startTime), "s\n";

        # Read from LMem
	$startTime = time;
        $actions = $client->max_actions_init($max_file, "readLMem");
        $client->max_set_param_uint64t($actions, "address", 2 * $sizeBytes);
        $client->max_set_param_uint64t($actions, "nbytes", $sizeBytes);
        $client->max_queue_output($actions, "lmem_to_cpu", $address_outData, $sizeBytes);       

        $client->max_run($engine, $actions);
        $client->free($actions);
	print "Reading from LMem:\t\t\t\t", (time - $startTime), "s\n";

        # Unload DFE
	$startTime = time;
        $client->max_unload($engine);
	print "Unoading DFE:\t\t\t\t\t", (time - $startTime), "s\n";

        # Get output stream from server
	$startTime = time;
        my $outData_ref = $client->receive_data_int32_t($address_outData, $size);
        @outData = @$outData_ref;
	print "Getting output stream:\t(size = ", ($size * 32), " bit)\t", (time - $startTime), "s\n";

        # Free allocated memory for streams on server
	$startTime = time;
        $client->free($address_inA);
        $client->free($address_inB);
        $client->free($address_outData);
	print "Freeing allocated memory for streams on server:\t", (time - $startTime), "s\n";

        # Free allocated maxfile data
	$startTime = time;
        $client->LMemLoopback_free();
	print "Freeing allocated maxfile data:\t\t\t", (time - $startTime), "s\n";

        # Close!
	$startTime = time;
        $transport->close();
	print "Closing connection:\t\t\t\t", (time - $startTime), "s\n";

    }; if($@){
        warn(Dumper($@));
    }

    return \@outData;
}
my $startTime = time;
my $size = 384;
my @inA = ();
my @inB = ();
my @dataOutCPU = ();
my @dataOutDFE = ();

for(my $i = 0; $i < $size; $i++) {
    $inA[$i] = $i;
    $inB[$i] = $size - $i;
}
print "Generating input data:\t\t\t\t", (time - $startTime), "s\n";

# DFE Output
$startTime = time;
my $dataOutDFE_ref = LMemLoopbackDFE($size, \@inA, \@inB);
print "DFE LMemLoopback total time:\t\t\t", (time - $startTime), "s\n";

# CPU Output
$startTime = time;
my $dataOutCPU_ref = LMemLoopbackCPU($size, \@inA, \@inB);
print "CPU LMemLoopback total time:\t\t\t", (time - $startTime), "s\n";

@dataOutCPU = @$dataOutCPU_ref;
@dataOutDFE = @$dataOutDFE_ref;

# Checking results
$startTime = time;
my $status = check(\@dataOutDFE, \@dataOutCPU, $size);
print "Checking results:\t\t\t\t", (time - $startTime), "s\n";

if ($status == 0) {
    print "Test successful!\n";
} else {
        print "Test failed ", $status, " times!\n";
        exit -1;
}
 
