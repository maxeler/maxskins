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

sub check {
    my ($dataIn_ref, $dataOut_ref, $size) = @_;
    my @dataIn = @$dataIn_ref;
    my @dataOut = @$dataOut_ref;

    for (my $i = 1; $i < $size - 1; $i++) {
        my $a = unpack('f', pack('f', $dataIn[$i - 1]));
        my $b = unpack('f', pack('f', $dataIn[$i]));
        my $c = unpack('f', pack('f', $dataIn[$i + 1]));
        my $res = unpack('f', pack('f', ($a + $b + $c) / 3));
        if ($dataOut[$i] != $res) {
            print "Test failed! [$i] $dataOut[$i] != $res\n"
        }
    }
    print "Test passed!\n";
}

# Make socket
my $socket    = new Thrift::Socket('localhost', 9090);

# Buffering is critical. Raw sockets are very slow
my $transport = new Thrift::BufferedTransport($socket);

# Wrap in a protocol
my $protocol  = new Thrift::BinaryProtocol($transport);

# Create a client to use the protocol encoder
my $client    = new com::maxeler::MovingAverage::MovingAverageServiceClient($protocol);

eval{
    # Connect!
    $transport->open();

    # Scalar inputs 
    my $size = 384;

    # Generate two random vectors
    my @dataIn = ();
    for (my $i = 0; $i < $size; $i++) {
        $dataIn[$i] = int(rand(100));
    }

    # Initialize maxfile
    my $max_file = $client->MovingAverage_init();

    # Load DFE
    my $max_engine = $client->max_load($max_file, '*');  

    # Allocate and send input streams to server
    my $address_dataIn = $client->malloc_float($size);
    $client->send_data_float($address_dataIn, \@dataIn);

    # Allocate memory for output stream on server
    my $address_dataOut = $client->malloc_float($size);

    print "Running DFE.\n";
    my $actions = new com::maxeler::MovingAverage::MovingAverage_actions_t_struct();
    $actions->{param_N} = $size;
    $actions->{instream_x} = $address_dataIn;
    $actions->{outstream_y} = $address_dataOut;
    my $address_actions = $client->send_MovingAverage_actions_t($actions);
    $client->MovingAverage_run($max_engine, $address_actions);

    # Unload DFE
    $client->max_unload($max_engine);

    # Get output stream from server
    my $dataOut_ref = $client->receive_data_float($address_dataOut, $size);
    my @dataOut = @$dataOut_ref;

    # Free allocated memory for streams on server
    $client->free($address_dataIn);
    $client->free($address_dataOut);

    # Free allocated maxfile data
    $client->MovingAverage_free();

    # Close!
    $transport->close();

    check(\@dataIn, \@dataOut, $size);

}; if($@){
   warn(Dumper($@));
}
