#!/usr/bin/perl -w

use strict;
use warnings;

use lib '../gen-perl';

use Thrift;
use Thrift::BinaryProtocol;
use Thrift::Socket;
use Thrift::BufferedTransport;

use com::maxeler::PassThrough::PassThroughService;
use com::maxeler::PassThrough::Types;

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

sub PassThroughCPU {
    my ($size, $dataIn) = @_;
    my @dataOut = ();

    for (my $i = 0; $i < $size; $i++) {
        $dataOut[$i] = $$dataIn[$i];
    }

    return \@dataOut;
}

sub PassThroughDFE {
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
        my $client    = new com::maxeler::PassThrough::PassThroughServiceClient($protocol);

        print "Creating a client:\t\t\t\t", (time - $startTime), "s\n";

        # Connect!
        $startTime = time;
        $transport->open();
        print "Opening connection:\t\t\t\t", (time - $startTime), "s\n";
            
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
        $startTime = time;
        $client->PassThrough($size, $address_dataIn, $address_dataOut);
        print "Pass through time:\t\t\t\t", (time - $startTime), "s\n";

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

        $startTime = time;
        $transport->close();
        print "Closing connection:\t\t\t\t", (time - $startTime), "s\n";

    }; if($@){
        warn(Dumper($@));
    }

    return \@dataOut;
}
# Input
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
my $dataOutDFE_ref = PassThroughDFE($size, \@dataIn);
print "DFE pass through total time:\t\t\t", (time - $startTime), "s\n";

# CPU Output
$startTime = time;
my $dataOutCPU_ref = PassThroughCPU($size, \@dataIn);
print "CPU pass through total time:\t\t\t", (time - $startTime), "s\n";

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
