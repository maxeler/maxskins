#!/usr/bin/perl -w

use strict;
use warnings;
use Time::HiRes qw(usleep);

use lib '../gen-perl';

use Thrift;
use Thrift::BinaryProtocol;
use Thrift::Socket;
use Thrift::BufferedTransport;

use com::maxeler::SignExt::SignExtService;
use com::maxeler::SignExt::Types;

use Data::Dumper;

eval {
    # Make socket
    my $socket = new Thrift::Socket('localhost', 9090);

    # Buffering is critical. Raw sockets are very slow
    my $transport = new Thrift::BufferedTransport($socket);

    # Wrap in a protocol
    my $protocol = new Thrift::BinaryProtocol($transport);

    # Create a client to use the protocol encoder
    my $client = new com::maxeler::SignExt::SignExtServiceClient($protocol);

    # Connect!
    $transport->open();

    if($#ARGV != 1) {
        print "Usage: $0 dfe_ip remote_ip\r\n";
        exit -1;
    }

    my $dfe_ip_address = $client->malloc_int64_t(5);
    
    $client->inet_aton("$ARGV[0]", $dfe_ip_address);
    
    my $remote_ip_address = $client->malloc_int64_t(5);
    $client->inet_aton("$ARGV[1]", $remote_ip_address);

    my $netmask_address = $client->malloc_int64_t(5);
    $client->inet_aton("255.255.255.0", $netmask_address);
    
    my $port = 2000;

    # Initialize maxfile
    my $maxfile = $client->SignExt_init();

    # Load DFE
    my $engine = $client->max_load($maxfile, '*');  

    my $enumkey = new com::maxeler::SignExt::max_config_key_bool_t_struct();
    $enumkey->{type} = com::maxeler::SignExt::max_config_key_bool_t_enum::MAX_CONFIG_PRINTF_TO_STDOUT;
    $client->max_config_set_bool($enumkey, 1);

    my $actions = $client->max_actions_init($maxfile, "default");

    $client->max_run($engine, $actions);
    $client->max_actions_free($actions);

    my $buffer_address = $client->malloc_int64_t(1);
    my $bufferSize = 4096 * 512;
    $client->posix_memalign($buffer_address, 4096, $bufferSize);

    my $buffer_ref = $client->receive_data_int64_t($buffer_address, 1);
    my @buffer_vec = @$buffer_ref;
    my $buffer = $buffer_vec[0];

    my $toCpu = $client->max_framed_stream_setup($engine, "toCPU", $buffer, $bufferSize, -1);

    my $enumconn = new com::maxeler::SignExt::max_net_connection_t_struct();
    $enumconn->{type} = com::maxeler::SignExt::max_net_connection_t_enum::MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1;
    $client->max_ip_config($engine, $enumconn, $dfe_ip_address, $netmask_address);
    my $dfe_socket = $client->max_udp_create_socket($engine, "udpTopPort1");
    $client->max_udp_bind($dfe_socket, $port);
    $client->max_udp_connect($dfe_socket, $remote_ip_address, 0);

    print "Listening on ".$ARGV[0]." port ".$port."\r\n";
            
    print "Waiting for kernel response...\r\n";

    my $f_address = $client->malloc_int64_t(1);
    my $fsz_address = $client->malloc_int64_t(1);
    my $numMessageRx = 0;
    my $cond = 1;
            
    while($cond == 1) {
        if ($client->max_framed_stream_read($toCpu, 1, $f_address, $fsz_address) == 1) {
            $numMessageRx += 1;

            my $fsz_ref = $client->receive_data_int64_t($fsz_address, 1);
            my @fsz_vec = @$fsz_ref;
            my $fsz = $fsz_vec[0];
            print "CPU: Got output frame ".$numMessageRx." - size ".$fsz." bytes\r\n";
        
            my $f_ref = $client->receive_data_int64_t($f_address, 1);
            my @f_vec = @$f_ref;
            my $f = $f_vec[0];

            my $w_ref = $client->receive_data_int64_t($f, 3);
            my @w = @$w_ref;
 
            for (my $i = 0; $i < 3; $i++) {
                use bignum qw/hex oct/;
                my $wp;
                if($w[$i] < 0) {
                    $wp = $w[$i] + 2**64;
                } else {
                    $wp = $w[$i];
                }
                
                printf("Frame [%d] Word[%d]: 0x%X\r\n", $numMessageRx, $i, $wp);
            }
                    
            $client->max_framed_stream_discard($toCpu, 1);
                    
            if ($w[0] == 0 && $w[1] == 0 && $w[2] == 0) {
                $cond = 0;
            }

        } else {
            usleep(1/100);
        }
    }
            
    $client->max_udp_close($dfe_socket);
    $client->max_framed_stream_release($toCpu);
    $client->max_unload($engine);
    $client->max_file_free($maxfile);
    $client->free($dfe_ip_address);
    $client->free($remote_ip_address);
    $client->free($netmask_address);
    $client->free($buffer_address);
    $client->free($f_address);
    $client->free($fsz_address);
    $client->SignExt_free();

    print "Done.\r\n";

    # Close!
    $transport->close();
    exit 0;
}; if($@){
    warn(Dumper($@));
}
