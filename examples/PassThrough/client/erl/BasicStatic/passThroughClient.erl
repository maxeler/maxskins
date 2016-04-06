-module(passThroughClient).

-include("passThroughService_thrift.hrl").

-export([t/0]).

check ([], [], Status, _)                                                                                    ->    Status;
check (OutDFE, OutCPU, Status, Iter) when (hd(OutDFE) - hd(OutCPU)) * (hd(OutDFE) - hd(OutCPU)) > 0.00001    ->    io:format("Output data @ ~w = ~w (expected ~w)~n", [Iter, hd(OutDFE), hd(OutCPU)]),
                                                                                                                   check (tl(OutDFE), tl(OutCPU), (Status + 1), (Iter + 1));
check (OutDFE, OutCPU, Status, Iter) when (hd(OutDFE) - hd(OutCPU)) * (hd(OutDFE) - hd(OutCPU)) < 0.00001    ->    check (tl(OutDFE), tl(OutCPU), Status, Iter + 1) .

passThroughCPU ([DataIn | []])    ->    [DataIn] ;
passThroughCPU ([DataIn | L])     ->    [DataIn | passThroughCPU(L)] .

passThroughDFE(Size, DataIn) ->
    DataOutDFE = 
        try
            StartTime0 = erlang:timestamp(),
            Port = 9090,
            {ok, Client0} = thrift_client_util:new("localhost",
                                                   Port,
                                                   passThroughService_thrift,
                                                   []),
            io:format("Createing a client and opening connection:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime0) / 1000000]),

            % Allocate and send input streams to server
            StartTime1 = erlang:timestamp(),    
            {Client1, {ok, Address_dataIn}} = thrift_client:call(Client0, malloc_int32_t, [Size]),
            {Client2, {ok, ok}} = thrift_client:call(Client1, send_data_int32_t, [Address_dataIn, DataIn]),
            io:format("Sending input data:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime1) / 1000000]),

            % Allocate memory for output stream on server
            StartTime3 = erlang:timestamp(),
            {Client3, {ok, Address_dataOut}} = thrift_client:call(Client2, malloc_int32_t, [Size]),
            io:format("Allocating memory for output stream on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime3) / 1000000]),

            % Action default
            StartTime4 = erlang:timestamp(),
            {Client4, {ok, ok}} = thrift_client:call(Client3, 'PassThrough', [Size, Address_dataIn, Address_dataOut]),
            io:format("Pass through time:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime4) / 1000000]),

            % Get output stream from server
            StartTime5 = erlang:timestamp(),
            {Client5, {ok, DataOut}} = thrift_client:call(Client4, receive_data_int32_t, [Address_dataOut, Size]),
            io:format("Getting output stream:\t(size = ~ws bit)\t~ws~n", [Size * 32, timer:now_diff(erlang:timestamp(), StartTime5) / 1000000]),

            % Free allocated memory for streams on server
            StartTime6 = erlang:timestamp(),
            {Client6, {ok, ok}} = thrift_client:call(Client5, free, [Address_dataIn]),
            {Client7, {ok, ok}} = thrift_client:call(Client6, free, [Address_dataOut]),
            io:format("Freeing allocated memory for streams on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime6) / 1000000]),

            StartTime8 = erlang:timestamp(),
            {_Client8, ok} = thrift_client:close(Client7),
            io:format("Closing connection:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime8) / 1000000]),

            DataOut
        catch
            error:Reason ->
                io:format("Error reason: ~p~n", [Reason]),
                erlang:exit(-1)
        end,
    
        DataOutDFE.

t() ->
    Size = 1024,
    DataIn = lists:seq(1, Size),

    % DFE Output
    StartTime1 = erlang:timestamp(),
    DataOutDFE = passThroughDFE(Size, DataIn),
    io:format("DFE pass through total time:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime1) / 1000000]),

    % CPU Output
    StartTime0 = erlang:timestamp(),
    DataOutCPU = passThroughCPU(DataIn),
    io:format("CPU pass through total time:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime0) / 1000000]),

    % Checking results
    StartTime2 = erlang:timestamp(),
    Status = check(DataOutDFE, DataOutCPU, 0, 0),
    io:format("Checking results:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime2) / 1000000]),

    if Status =:= 0    ->    io:format("Test successful!~n");
       true            ->    io:format("Test failed ~w times!~n", [Status]),
                             erlang:exit(-1)
    end,
    
    ok.
