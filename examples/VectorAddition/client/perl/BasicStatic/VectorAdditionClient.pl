#!/usr/bin/perl -w

use strict;
use warnings;

use lib '../gen-perl';

use Thrift;
use Thrift::BinaryProtocol;
use Thrift::Socket;
use Thrift::BufferedTransport;

use com::maxeler::VectorAddition::VectorAdditionService;
use com::maxeler::VectorAddition::Types;

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

sub VectorAdditionCPU {
    my ($size, $x, $y, $scalar) = @_;
    my @dataOut = ();

    for (my $i = 0; $i < $size; $i++) {
        $dataOut[$i] = $$x[$i] + $$y[$i] + $scalar;
    }

    return \@dataOut;
}

sub VectorAdditionDFE {
    my ($size, $x, $y, $scalar) = @_;
    my @dataOut = ();

    eval {
        my $sizeBytes = $size * 4;
	my $startTime = time;
	
        # Make socket
        my $socket    = new Thrift::Socket('localhost', 9090);

        # Buffering is critical. Raw sockets are very slow
        my $transport = new Thrift::BufferedTransport($socket);

        # Wrap in a protocol
        my $protocol  = new Thrift::BinaryProtocol($transport);

        # Create a client to use the protocol encoder
        my $client    = new com::maxeler::VectorAddition::VectorAdditionServiceClient($protocol);

	print "Creating a client:\t\t\t\t", (time - $startTime), "s\n";

        # Connect!
	$startTime = time;
        $transport->open();
	print "Opening connection:\t\t\t\t", (time - $startTime), "s\n";
            
        # Allocate and send input streams to server
	$startTime = time;
        my $address_x = $client->malloc_int32_t($size);
        $client->send_data_int32_t($address_x, $x);

        my $address_y = $client->malloc_int32_t($size);
        $client->send_data_int32_t($address_y, $y);

	print "Sending input data:\t\t\t\t", (time - $startTime), "s\n";

        # Allocate memory for output stream on server
	$startTime = time;
        my $address_dataOut = $client->malloc_int32_t($size);
	print "Allocating memory for output stream on server:\t", (time - $startTime), "s\n";

        # Writing first vector to LMem
	$startTime = time;
        $client->VectorAddition_writeLMem(0, $sizeBytes, $address_x);
	print "Writing to LMem:\t\t\t\t", (time - $startTime), "s\n";
	
	# Action default
	$startTime = time;
        $client->VectorAddition($scalar, $size, $address_y, $address_dataOut);
	print "Vector addition time:\t\t\t\t", (time - $startTime), "s\n";

        # Get output stream from server
	$startTime = time;
        my $dataOut_ref = $client->receive_data_int32_t($address_dataOut, $size);
        @dataOut = @$dataOut_ref;
	print "Getting output stream:\t(size = ", ($size * 32), " bit)\t", (time - $startTime), "s\n";

        # Free allocated memory for streams on server
	$startTime = time;
        $client->free($address_x);
        $client->free($address_y);
        $client->free($address_dataOut);
	print "Freeing allocated memory for streams on server:\t", (time - $startTime), "s\n";
	
	$startTime = time;
        $transport->close();
	print "Closing connection:\t\t\t\t", (time - $startTime), "s\n";

    }; if($@){
        warn(Dumper($@));
    }

    return \@dataOut;
}

my $startTime = time;
my $size = 384;
my $scalar = 3;
my @firstVector = ();
my @secondVector = ();
my @dataOutCPU = ();
my @dataOutDFE = ();

for(my $i = 0; $i < $size; $i++) {
    $firstVector[$i] = int(rand(100));
    $secondVector[$i] = int(rand(100));
}
print "Generating input data:\t\t\t\t", (time - $startTime), "s\n";

# DFE Output
$startTime = time;
my $dataOutDFE_ref = VectorAdditionDFE($size, \@firstVector, \@secondVector, $scalar);
print "DFE Vector addition total time:\t\t\t", (time - $startTime), "s\n";

# CPU Output
$startTime = time;
my $dataOutCPU_ref = VectorAdditionCPU($size, \@firstVector, \@secondVector, $scalar);
print "CPU Vector addition total time:\t\t\t", (time - $startTime), "s\n";

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


