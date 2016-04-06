-module(movingAverageClient).

-include("movingAverageService_thrift.hrl").
-include("movingAverage_types.hrl").

-export([t/0]).

movingAverageCPU ([X, Y, Z | []])   ->    [(X + Y + Z) / 3] ;
movingAverageCPU ([X, Y, Z | L])    ->    [(X + Y + Z) / 3 | movingAverageCPU([Y, Z | L])] .

check ([], [], Status, _)                                                                                    ->    Status;
check (OutDFE, OutCPU, Status, Iter) when (hd(OutDFE) - hd(OutCPU)) * (hd(OutDFE) - hd(OutCPU)) > 0.00001    ->    io:format("Output data @ ~w = ~w (expected ~w)~n", [Iter, hd(OutDFE), hd(OutCPU)]),
                                                                                                                   check (tl(OutDFE), tl(OutCPU), (Status + 1), (Iter + 1));
check (OutDFE, OutCPU, Status, Iter) when (hd(OutDFE) - hd(OutCPU)) * (hd(OutDFE) - hd(OutCPU)) < 0.00001    ->    check (tl(OutDFE), tl(OutCPU), Status, Iter + 1) .

t() ->
    try
        StartTime0 = erlang:timestamp(),
        Port = 9090,
        {ok, Client0} = thrift_client_util:new("localhost",
                                               Port,
                                               movingAverageService_thrift,
                                               []),
        io:format("Createing a client and opening connection:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime0) / 1000000]),

        % Generate input data
        StartTime1 = erlang:timestamp(),
        Size = 384,
	SizeBytes = Size * 4,
        DataIn = [random:uniform(1000) || _ <- lists:seq(1, Size)],
        io:format("Generating input data:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime1) / 1000000]),

        % Initialize maxfile
	StartTime2 = erlang:timestamp(),
	{Client1, {ok, Maxfile}} = thrift_client:call(Client0, 'MovingAverage_init', []),
	io:format("Initializing maxfile:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime2) / 1000000]),

        % Load DFE
	StartTime3 = erlang:timestamp(),
	{Client2, {ok, Engine}} = thrift_client:call(Client1, max_load, [Maxfile, "*"]),
	io:format("Loading DFE:\t\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime3) / 1000000]),

        % Allocate and send input streams to server
        StartTime4 = erlang:timestamp(),
        {Client3, {ok, Address_dataIn}} = thrift_client:call(Client2, malloc_float, [Size]),
        {Client4, {ok, ok}} = thrift_client:call(Client3, send_data_float, [Address_dataIn, DataIn]),
        io:format("Sending input data:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime4) / 1000000]),

        % Allocate memory for output stream on server
        StartTime5 = erlang:timestamp(),
        {Client5, {ok, Address_dataOut}} = thrift_client:call(Client4, malloc_float, [Size]),
        io:format("Allocating memory for output stream on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime5) / 1000000]),

        % Action default
        StartTime6 = erlang:timestamp(),
        {Client6, {ok, Actions}} = thrift_client:call(Client5, max_actions_init, [Maxfile, "default"]),
	{Client7, {ok, ok}} = thrift_client:call(Client6, max_set_param_uint64t, [Actions, "N", Size]),
	{Client8, {ok, ok}} = thrift_client:call(Client7, max_queue_input, [Actions, "x", Address_dataIn, SizeBytes]),
	{Client9, {ok, ok}} = thrift_client:call(Client8, max_queue_output, [Actions, "y", Address_dataOut, SizeBytes]),
	{Client10, {ok, ok}} = thrift_client:call(Client9, max_run, [Engine, Actions]),
	
	io:format("Moving average time:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime6) / 1000000]),

        % Unload DFE
	StartTime7 = erlang:timestamp(),
        {Client11, {ok, ok}} = thrift_client:call(Client10, max_unload, [Engine]),
	io:format("Unloading DFE:\t\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime7) / 1000000]),

        % Get output stream from server
        StartTime8 = erlang:timestamp(),
        {Client12, {ok, DataOut}} = thrift_client:call(Client11, receive_data_float, [Address_dataOut, Size]),
        io:format("Getting output stream:\t(size = ~ws bit)\t~ws~n", [Size * 32, timer:now_diff(erlang:timestamp(), StartTime8) / 1000000]),

        % Free allocated memory for streams on server
        StartTime9 = erlang:timestamp(),
        {Client13, {ok, ok}} = thrift_client:call(Client12, free, [Address_dataIn]),
        {Client14, {ok, ok}} = thrift_client:call(Client13, free, [Address_dataOut]),
        io:format("Freeing allocated memory for streams on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime9) / 1000000]),

        % Free allocated maxfile data
	StartTime10 = erlang:timestamp(),
	{Client15, {ok, ok}} = thrift_client:call(Client14, 'MovingAverage_free', []),
	io:format("Freeing allocated maxfile data:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime10) / 1000000]),
    
        % Checking results
        StartTime11 = erlang:timestamp(),
        DataOutCPU = movingAverageCPU(DataIn),
        {DataOutDropedLast, _} = lists:split(length(DataOut) - 1, DataOut),
        DataOutDFE = tl(DataOutDropedLast),
        Status = check(DataOutDFE, DataOutCPU, 0, 0),
        io:format("Checking results:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime11) / 1000000]),

        StartTime12 = erlang:timestamp(),
        {_Client16, ok} = thrift_client:close(Client15),
        io:format("Closing connection:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime12) / 1000000]),

        io:format("DFE moving average total time:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime0) / 1000000]),

        if Status =:= 0    ->    io:format("Test successful!~n");
           true            ->    io:format("Test failed ~w times!~n", [Status]),
                                 erlang:exit(-1)
        end
    
    catch
        error:Reason ->
	    io:format("Error reason: ~p~n", [Reason]),
	    erlang:exit(-1)
    end,
    ok.

