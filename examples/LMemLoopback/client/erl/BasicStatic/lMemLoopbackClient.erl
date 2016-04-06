-module(lMemLoopbackClient).

-include("lMemLoopbackService_thrift.hrl").

-export([t/0]).

check ([], [], Status, _)                                                                                    ->    Status;
check (OutDFE, OutCPU, Status, Iter) when (hd(OutDFE) - hd(OutCPU)) * (hd(OutDFE) - hd(OutCPU)) > 0.00001    ->    io:format("Output data @ ~w = ~w (expected ~w)~n", [Iter, hd(OutDFE), hd(OutCPU)]),
                                                                                                                   check (tl(OutDFE), tl(OutCPU), (Status + 1), (Iter + 1));
check (OutDFE, OutCPU, Status, Iter) when (hd(OutDFE) - hd(OutCPU)) * (hd(OutDFE) - hd(OutCPU)) < 0.00001    ->    check (tl(OutDFE), tl(OutCPU), Status, Iter + 1) .

lMemLoopBackCPU ([], [])     ->    [] ;
lMemLoopBackCPU ([A | InA], [B | InB])    ->    [A + B | lMemLoopBackCPU (InA, InB)] .

lMemLoopBackDFE(Size, InA, InB) ->
    DataOutDFE = 
        try
            SizeBytes = Size * 4,
            StartTime0 = erlang:timestamp(),
            Port = 9090,
            {ok, Client0} = thrift_client_util:new("localhost",
                                                   Port,
                                                   lMemLoopbackService_thrift,
                                                   []),
            io:format("Creating a client and opening connection:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime0) / 1000000]),

            % Allocate and send input streams to server
            StartTime1 = erlang:timestamp(),    
            {Client1, {ok, Address_inA}} = thrift_client:call(Client0, malloc_int32_t, [Size]),
            {Client2, {ok, ok}} = thrift_client:call(Client1, send_data_int32_t, [Address_inA, InA]),

            {Client3, {ok, Address_inB}} = thrift_client:call(Client2, malloc_int32_t, [Size]),
            {Client4, {ok, ok}} = thrift_client:call(Client3, send_data_int32_t, [Address_inB, InB]),
            io:format("Sending input data:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime1) / 1000000]),

            % Allocate memory for output stream on server
            StartTime2 = erlang:timestamp(),
            {Client5, {ok, Address_dataOut}} = thrift_client:call(Client4, malloc_int32_t, [Size]),
            io:format("Allocating memory for output stream on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime2) / 1000000]),

            % Writing to LMem
            StartTime3 = erlang:timestamp(),
            {Client6, {ok, ok}} = thrift_client:call(Client5, 'LMemLoopback_writeLMem' , [0, SizeBytes, Address_inA]),
            {Client7, {ok, ok}} = thrift_client:call(Client6, 'LMemLoopback_writeLMem' , [SizeBytes, SizeBytes, Address_inB]),
            io:format("Writing to LMmem:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime3) / 1000000]),

            % Action default
            StartTime4 = erlang:timestamp(),
            {Client8, {ok, ok}} = thrift_client:call(Client7, 'LMemLoopback', [Size]),
            io:format("LMemLoopback time:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime4) / 1000000]),

            % Reading from LMem
            StartTime5 = erlang:timestamp(),
            {Client9, {ok, ok}} = thrift_client:call(Client8, 'LMemLoopback_readLMem' , [2 * SizeBytes, SizeBytes, Address_dataOut]),
            io:format("Reading from  LMmem:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime5) / 1000000]),

            % Get output stream from server
            StartTime6 = erlang:timestamp(),
            {Client10, {ok, DataOut}} = thrift_client:call(Client9, receive_data_int32_t, [Address_dataOut, Size]),
            io:format("Getting output stream:\t(size = ~ws bit)\t~ws~n", [Size * 32, timer:now_diff(erlang:timestamp(), StartTime6) / 1000000]),

            % Free allocated memory for streams on server
            StartTime7 = erlang:timestamp(),
            {Client11, {ok, ok}} = thrift_client:call(Client10, free, [Address_inA]),
            {Client12, {ok, ok}} = thrift_client:call(Client11, free, [Address_inB]),
            {Client13, {ok, ok}} = thrift_client:call(Client12, free, [Address_dataOut]),
            io:format("Freeing allocated memory for streams on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime7) / 1000000]),

            % Close!
            StartTime8 = erlang:timestamp(),
            {_Client14, ok} = thrift_client:close(Client13),
            io:format("Closing connection:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime8) / 1000000]),

            DataOut
        catch
            error:Reason ->
                io:format("Error reason: ~p~n", [Reason]),
                erlang:exit(-1)
        end,
    
        DataOutDFE.

t() ->
    StartTime1 = erlang:timestamp(),
    Size = 384,
    InA = lists:seq(0, (Size-1)),
    InB = [ Size - A || A <- InA],
    io:format("Generating input data:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime1) / 1000000]),

    % DFE Output
    StartTime2 = erlang:timestamp(),
    DataOutDFE = lMemLoopBackDFE(Size, InA, InB),
    io:format("DFE LMemLoopback total time:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime2) / 1000000]),

    % CPU Output
    StartTime3 = erlang:timestamp(),
    DataOutCPU = lMemLoopBackCPU(InA, InB),
    io:format("CPU LMemLoopback total time:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime3) / 1000000]),

    % Checking results
    StartTime4 = erlang:timestamp(),
    Status = check(DataOutDFE, DataOutCPU, 0, 0),
    io:format("Checking results:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime4) / 1000000]),

    if Status =:= 0    ->    io:format("Test successful!~n");
       true            ->    io:format("Test failed ~w times!~n", [Status]),
                             erlang:exit(-1)
    end,
    
    ok.
