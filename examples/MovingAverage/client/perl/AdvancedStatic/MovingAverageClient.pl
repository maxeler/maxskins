#!/usr/bin/perl -w

use strict;
use warnings;

use lib '../gen-perl';

use Thrift;
use Thrift::BinaryProtocol;
use Thrift::Socket;
use Thrift::BufferedTransport;

use com::maxeler::MovingAverage::MovingAverageService;
use com::maxeler::MovingAverage::Types;

use Data::Dumper;

use Time::HiRes qw/ time /;

sub check {
    my ($dataIn_ref, $dataOut_ref, $size) = @_;
    my @dataIn = @$dataIn_ref;
    my @dataOut = @$dataOut_ref;

    my $status = 0;

    for (my $i = 1; $i < $size - 1; $i++) {
        my $a = unpack('f', pack('f', $dataIn[$i - 1]));
        my $b = unpack('f', pack('f', $dataIn[$i]));
        my $c = unpack('f', pack('f', $dataIn[$i + 1]));
        my $res = unpack('f', pack('f', ($a + $b + $c) / 3));
        if ($dataOut[$i] != $res) {
            print "Output data @ $i = $dataOut[$i] (expected $res)\n";
            $status++;
        }
    }

    return $status;
}

my $startTime = time;
my $startDFETime = $startTime;

# Make socket
my $socket    = new Thrift::Socket('localhost', 9090);

# Buffering is critical. Raw sockets are very slow
my $transport = new Thrift::BufferedTransport($socket);

# Wrap in a protocol
my $protocol  = new Thrift::BinaryProtocol($transport);

# Create a client to use the protocol encoder
my $client    = new com::maxeler::MovingAverage::MovingAverageServiceClient($protocol);

print "Creating a client:\t\t\t\t", (time - $startTime), "s\n";

eval{
    # Connect!
    $startTime = time;
    $transport->open();
    print "Opening connection:\t\t\t\t", (time - $startTime), "s\n";

    $startTime = time;
    my $size = 384;
    # Generate two random vectors
    my @dataIn = ();
    for (my $i = 0; $i < $size; $i++) {
        $dataIn[$i] = int(rand(100));
    }
    print "Generating input data:\t\t\t\t", (time - $startTime), "s\n";

    # Initialize maxfile
    $startTime = time;
    my $max_file = $client->MovingAverage_init();
    print "Initializing maxfile:\t\t\t\t", (time - $startTime), "s\n";

    # Load DFE
    $startTime = time;
    my $max_engine = $client->max_load($max_file, '*');
    print "Loading DFE:\t\t\t\t\t", (time - $startTime), "s\n";

    # Allocate and send input streams to server
    $startTime = time;
    my $address_dataIn = $client->malloc_float($size);
    $client->send_data_float($address_dataIn, \@dataIn);
    print "Sending input data:\t\t\t\t", (time - $startTime), "s\n";

    # Allocate memory for output stream on server
    $startTime = time;
    my $address_dataOut = $client->malloc_float($size);
    print "Allocating memory for output stream on server:\t", (time - $startTime), "s\n";

    # Action default
    $startTime = time;
    my $actions = new com::maxeler::MovingAverage::MovingAverage_actions_t_struct();
    $actions->{param_N} = $size;
    $actions->{instream_x} = $address_dataIn;
    $actions->{outstream_y} = $address_dataOut;
    my $address_actions = $client->send_MovingAverage_actions_t($actions);
    $client->MovingAverage_run($max_engine, $address_actions);
    print "Moving average time:\t\t\t\t", (time - $startTime), "s\n";

    # Unload DFE
    $startTime = time;
    $client->max_unload($max_engine);
    print "Unloading DFE:\t\t\t\t\t", (time - $startTime), "s\n";

    # Get output stream from server
    $startTime = time;
    my $dataOut_ref = $client->receive_data_float($address_dataOut, $size);
    my @dataOut = @$dataOut_ref;
    print "Getting output stream:\t(size = ", ($size * 32), " bit)\t", (time - $startTime), "s\n";

    # Free allocated memory for streams on server
    $startTime = time;
    $client->free($address_dataIn);
    $client->free($address_dataOut);
    print "Freeing allocated memory for streams on server:\t", (time - $startTime), "s\n";

    # Free allocated maxfile data
    $startTime = time;
    $client->MovingAverage_free();
    print "Freeing allocated maxfile data:\t\t\t", (time - $startTime), "s\n";

    $startTime = time;
    my $status = check(\@dataIn, \@dataOut, $size);
    print "Checking results:\t\t\t\t", (time - $startTime), "s\n";
    
    # Close!
    $startTime = time;
    $transport->close();
    print "Closing connection:\t\t\t\t", (time - $startTime), "s\n";

    print "DFE moving average total time:\t\t\t", (time - $startDFETime), "s\n";

    if ($status == 0) { 
        print "Test successful!\n";
    } else {
        print "Test failed ", $status, " times!\n";
        exit -1;
    }

}; if($@){
   warn(Dumper($@));
}
