#!/usr/bin/perl -w

use strict;
use warnings;

use lib '../gen-perl';

use Thrift;
use Thrift::BinaryProtocol;
use Thrift::Socket;
use Thrift::BufferedTransport;

use com::maxeler::Simple::SimpleService;
use com::maxeler::Simple::Types;

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

sub SimpleCPU {
    my ($size, $dataIn) = @_;
    my @dataOut = ();

    for (my $i = 0; $i < $size; $i++) {
        $dataOut[$i] = $$dataIn[$i] * $$dataIn[$i] + $$dataIn[$i];
    }

    return \@dataOut;
}

sub SimpleDFE {
    my ($size, $dataIn) = @_;
    my @dataOut = ();
    eval {
	my $startTime = time;
	
        # Make socket
        my $socket    = new Thrift::Socket('localhost', 9090);

        # Buffering is critical. Raw sockets are very slow
        my $transport = new Thrift::BufferedTransport($socket);

        # Wrap in a protocol
        my $protocol  = new Thrift::BinaryProtocol($transport);

        # Create a client to use the protocol encoder
        my $client    = new com::maxeler::Simple::SimpleServiceClient($protocol);

	print "Creating a client:\t\t\t\t", (time - $startTime), "s\n";

        # Connect!
	$startTime = time;
        $transport->open();
	print "Opening connection:\t\t\t\t", (time - $startTime), "s\n";

        # Initialize maxfile
	$startTime = time;
        my $max_file = $client->Simple_init();
	print "Initializing maxfile:\t\t\t\t", (time - $startTime), "s\n";

        # Load DFE
	$startTime = time;
        my $engine = $client->max_load($max_file, '*');  
	print "Loading DFE:\t\t\t\t\t", (time - $startTime), "s\n";
            
        # Allocate and send input streams to server
	$startTime = time;
        my $address_dataIn = $client->malloc_float($size);
        $client->send_data_float($address_dataIn, $dataIn);
	print "Sending input data:\t\t\t\t", (time - $startTime), "s\n";

        # Allocate memory for output stream on server
	$startTime = time;
        my $address_dataOut = $client->malloc_float($size);
	print "Allocating memory for output stream on server:\t", (time - $startTime), "s\n";
	
	# Action default        
        my $action = new com::maxeler::Simple::Simple_actions_t_struct();
        $action->{param_N} = $size;
        $action->{instream_x} = $address_dataIn;
        $action->{outstream_y}  = $address_dataOut;
        my $address_action = $client->send_Simple_actions_t($action);
        $client->Simple_run($engine, $address_action);
	print "Simple time:\t\t\t\t\t", (time - $startTime), "s\n";

        # Unload DFE
	$startTime = time;
        $client->max_unload($engine);
	print "Unloading DFE:\t\t\t\t\t", (time - $startTime), "s\n";

        # Get output stream from server
	$startTime = time;
        my $dataOut_ref = $client->receive_data_float($address_dataOut, $size);
        @dataOut = @$dataOut_ref;
	print "Getting output stream:\t(size = ", ($size * 32), " bit)\t", (time - $startTime), "s\n";

        # Free allocated memory for streams on server
	$startTime = time;
        $client->free($address_dataIn);
        $client->free($address_dataOut);
	print "Freeing allocated memory for streams on server:\t", (time - $startTime), "s\n";

        # Free allocated maxfile data
	$startTime = time;
        $client->Simple_free();
	print "Freeing allocated maxfile data:\t\t\t", (time - $startTime), "s\n";
        
	$startTime = time;
        $transport->close();
	print "Closing connection:\t\t\t\t", (time - $startTime), "s\n";

    }; if($@){
        warn(Dumper($@));
    }

    return \@dataOut;
}

my $startTime = time;
my $size = 1024;
my @dataIn = ();
my @dataOutCPU = ();
my @dataOutDFE = ();

for(my $i = 0; $i < $size; $i++) {
    $dataIn[$i] = $i + 1;
}
print "Generating input data:\t\t\t\t", (time - $startTime), "s\n";

# DFE Output
$startTime = time;
my $dataOutDFE_ref = SimpleDFE($size, \@dataIn);
print "DFE simple total time:\t\t\t\t", (time - $startTime), "s\n";

# CPU Output
$startTime = time;
my $dataOutCPU_ref = SimpleCPU($size, \@dataIn);
print "CPU simple total time:\t\t\t\t", (time - $startTime), "s\n";

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
