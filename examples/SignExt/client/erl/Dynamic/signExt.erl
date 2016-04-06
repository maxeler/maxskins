-module(signExt).

-include("signExtService_thrift.hrl").

-export([t/1]).

-define(Thread_sleep_time, 5).
-define(Package_size, 3).

stream_read(_To_cpu, _Frame_address, _Fsz_address, Returned, _Client)   when (Returned == 1)  ->    1;
stream_read(To_cpu, Frame_address, Fsz_address, Returned, Client)       when (Returned == 0)  ->
                                                    timer:sleep(?Thread_sleep_time),             
				                    {Client1, {ok, Stream_return}} = thrift_client:call(Client, max_framed_stream_read, [To_cpu, 1, Frame_address, Fsz_address]),
                                                    stream_read(To_cpu, Frame_address, Fsz_address, Stream_return, Client1).

process_data(Fsz_address, Frame_address, Client, To_cpu, NumMessageRx)    ->
						    {Client1, {ok, Fsz_array}} = thrift_client:call(Client, receive_data_int64_t, [Fsz_address, 1]), 
						    Fsz = lists:nth(1, Fsz_array),
						    io:format("CPU: Got output frame ~w - size ~w bytes~n", [NumMessageRx, Fsz]),
						    {Client2, {ok, Frame_array}} = thrift_client:call(Client1, receive_data_int64_t, [Frame_address, 1]), 
						    Frame = lists:nth(1, Frame_array),						    
	                                            {Client3, {ok, Word_array}} = thrift_client:call(Client2, receive_data_int64_t, [Frame, ?Package_size]),						    
	                                            process_words(Word_array, 1, 3, NumMessageRx),
	                                            {Client4, {ok, ok}} = thrift_client:call(Client3, max_framed_stream_discard, [To_cpu, 1]),
						    Func_isZero = fun(X) -> X == 0 end, 
	                                            Is_finished = lists:all(Func_isZero, Word_array),
	                                            if 
	                                              Is_finished == true -> 1;
	                                              true -> stream_read(To_cpu, Frame_address, Fsz_address, 0, Client4),                                                              
	                                                      process_data(Fsz_address, Frame_address, Client, To_cpu, NumMessageRx+1)
	                                            end.                                                  


process_words(_Word_array, _I, 0, _NumMessageRx)           ->   1;
process_words(Word_array, I, Package_size, NumMessageRx)   ->   
                                                    Word = lists:nth(I, Word_array),
					            if 
                                                        Word >= 0 -> io:format("FRAME [~w] WORD[~w]: 0x~.16b~n", [NumMessageRx, I, Word]);
                                                        true -> Positive_word = Word + (1 bsl 64),
                                                                io:format("FRAME [~w] WORD[~w]: 0x~.16b~n", [NumMessageRx, I, Positive_word])                                                              
                                                    end,
                                                    process_words (Word_array, (I+1), (Package_size-1), NumMessageRx). 

t(Args) ->    

    % Buffer size
    Buffer_size = 4096 * 512,

    % Buffer aligment
    Buffer_alignment = 4096,

    Listening_port = 2000,

    StartTime0 = erlang:timestamp(),
    Port = 9090,
    %io:format("Args: ~p\n", [Args]),
    {ok, Client0} = thrift_client_util:new("localhost",
                                           Port,
                                           signExtService_thrift,
                                           []),
    
    io:format("Creating a client and opening connection:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime0) / 1000000]),

    % DfeIpAddress
    {Client1, {ok, Dfe_ip_address}} = thrift_client:call(Client0, malloc_int64_t, [5]),    
    {Client2, {ok, _Dfe_ip_address_2}} = thrift_client:call(Client1, inet_aton, [lists:nth(1, Args), Dfe_ip_address]),
    
    % RemoteIpAddress
    {Client3, {ok, Remote_ip_address}} = thrift_client:call(Client2, malloc_int64_t, [5]),
    {Client4, {ok, _Remote_ip_address_2}} = thrift_client:call(Client3, inet_aton, [lists:nth(2, Args), Remote_ip_address]),
    
    % Subnet mask
    {Client5, {ok, Netmask_address}} = thrift_client:call(Client4, malloc_int64_t, [5]),
    {Client6, {ok, _Netmask_address_2}} = thrift_client:call(Client5, inet_aton, ["255.255.255.0", Netmask_address]),
    
    % Initialize maxfile
    StartTime2 = erlang:timestamp(),
    {Client7, {ok, Maxfile}} = thrift_client:call(Client6, 'SignExt_init', []),
    io:format("Initializing maxfile:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime2) / 1000000]),

    % Load DFE
    StartTime3 = erlang:timestamp(),
    {Client8, {ok, Engine}} = thrift_client:call(Client7, max_load, [Maxfile, "*"]),
    io:format("Loading DFE:\t\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime3) / 1000000]),

    % SetEnum   type = signExt_max_config_key_bool_t_enum_MAX_CONFIG_PRINTF_TO_STDOUT (4)        
    EnumKey = #max_config_key_bool_t_struct {type = 4},
    {Client9, {ok, ok}} = thrift_client:call(Client8, max_config_set_bool, [EnumKey, 1]),
        
    % Set actions
    {Client10, {ok, Actions}} = thrift_client:call(Client9, max_actions_init, [Maxfile, "default"]),
    
    % Run actions
    {Client11, {ok, ok}} = thrift_client:call(Client10, max_run, [Engine, Actions]),
    
    % Free actions
    {Client12, {ok, ok}} = thrift_client:call(Client11, max_actions_free, [Actions]),
    
    % Buffer address
    {Client13, {ok, Buffer_address}} = thrift_client:call(Client12, malloc_int64_t, [1]),

    {Client14, {ok, _Buffer_address_aligned}} = thrift_client:call(Client13, posix_memalign, [Buffer_address, Buffer_alignment, Buffer_size]),
    
    % Get buffer   
    {Client15, {ok, Buffer_array}} = thrift_client:call(Client14, receive_data_int64_t, [Buffer_address, 1]),
    Buffer = lists:nth(1, Buffer_array),
    
    % Framed stream setup
    {Client16, {ok, To_cpu}} = thrift_client:call(Client15, max_framed_stream_setup, [Engine, "toCPU", Buffer, Buffer_size, -1]),
    
    % type = signExt_max_net_connection_t_enum_MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1 (2)
    Enumconn = #max_net_connection_t_struct {type = 2}, 
    
    % Ip config    
    {Client17, {ok, ok}} = thrift_client:call(Client16, max_ip_config, [Engine, Enumconn, Dfe_ip_address, Netmask_address]),
    
    % Udp socket create  
    {Client18, {ok, Dfe_socket}} = thrift_client:call(Client17, max_udp_create_socket, [Engine, "udpTopPort1"]),
    
    % Socket bind
    {Client19, {ok, ok}} = thrift_client:call(Client18, max_udp_bind, [Dfe_socket, Listening_port]),
    
    % Udp connect
    {Client20, {ok, ok}} = thrift_client:call(Client19, max_udp_connect, [Dfe_socket, Remote_ip_address, 0]),

    io:format("Listening on port ~w \n", [Listening_port]),
    io:format("Waiting for kernel response...\n"),

    % Allocate memory for frame address    
    {Client21, {ok, Frame_address}} = thrift_client:call(Client20, malloc_int64_t, [1]),

    % Allocate memory for fzs_address
    {Client22, {ok, Fsz_address}} = thrift_client:call(Client21, malloc_int64_t, [1]),
    
    % Main loop wait for packages

    NumMessageRx = 0,
     
    _Status = stream_read(To_cpu, Frame_address, Fsz_address, 0, Client22),
    
    % Process data
    process_data(Fsz_address, Frame_address, Client22, To_cpu, NumMessageRx),

    % Close sockets
    StartTime10 = erlang:timestamp(),
    {Client26, {ok, ok}} = thrift_client:call(Client22, max_udp_close, [Dfe_socket]),

    {Client27, {ok, ok}} = thrift_client:call(Client26, max_framed_stream_release, [To_cpu]),
    io:format("Closing sockets:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime10) / 1000000]),

    % Free allocated memory
    StartTime11 = erlang:timestamp(),
    {Client28, {ok, ok}} = thrift_client:call(Client27, free, [Remote_ip_address]),
    {Client29, {ok, ok}} = thrift_client:call(Client28, free, [Netmask_address]),
    {Client30, {ok, ok}} = thrift_client:call(Client29, free, [Buffer_address]),
    {Client31, {ok, ok}} = thrift_client:call(Client30, free, [Dfe_ip_address]),

    io:format("Freeing allocated memory for streams on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime11) / 1000000]),

    % Free allocated maxfile data
    StartTime12 = erlang:timestamp(),
    {Client32, {ok, ok}} = thrift_client:call(Client31, 'SignExt_free', []),
    io:format("Freeing allocated maxfile data:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime12) / 1000000]),

    % Close!
    StartTime13 = erlang:timestamp(),
    {_Client33, ok} = thrift_client:close(Client32),
    io:format("Closing connection:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime13) / 1000000]),


    ok.

