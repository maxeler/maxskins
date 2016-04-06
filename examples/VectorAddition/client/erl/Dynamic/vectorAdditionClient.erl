-module(vectorAdditionClient).

-include("vectorAdditionService_thrift.hrl").

-export([t/0]).

check ([], [], Status, _)                                                                                    ->    Status;
check (OutDFE, OutCPU, Status, Iter) when (hd(OutDFE) - hd(OutCPU)) * (hd(OutDFE) - hd(OutCPU)) > 0.00001    ->    io:format("Output data @ ~w = ~w (expected ~w)~n", [Iter, hd(OutDFE), hd(OutCPU)]),
                                                                                                                   check (tl(OutDFE), tl(OutCPU), (Status + 1), (Iter + 1));
check (OutDFE, OutCPU, Status, Iter) when (hd(OutDFE) - hd(OutCPU)) * (hd(OutDFE) - hd(OutCPU)) < 0.00001    ->    check (tl(OutDFE), tl(OutCPU), Status, Iter + 1) .

vectorAdditionCPU ([], [], Scalar)     ->    [] ;
vectorAdditionCPU ([X | Xs], [Y | Ys], Scalar)    ->    [X + Y + Scalar | vectorAdditionCPU (Xs, Ys, Scalar)] .

vectorAdditionDFE(Size, X, Y, Scalar) ->
    DataOutDFE = 
        try
            SizeBytes = Size * 4,
            StartTime0 = erlang:timestamp(),
            Port = 9090,
            {ok, Client0} = thrift_client_util:new("localhost",
                                                   Port,
                                                   vectorAdditionService_thrift,
                                                   []),
            io:format("Creating a client and opening connection:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime0) / 1000000]),

            % Initialize maxfile
            StartTime2 = erlang:timestamp(),
            {Client1, {ok, Maxfile}} = thrift_client:call(Client0, 'VectorAddition_init', []),
            io:format("Initializing maxfile:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime2) / 1000000]),

            % Load DFE
	    StartTime3 = erlang:timestamp(),
            {Client2, {ok, Engine}} = thrift_client:call(Client1, max_load, [Maxfile, "*"]),
            io:format("Loading DFE:\t\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime3) / 1000000]),

            % Allocate and send input streams to server
            StartTime4 = erlang:timestamp(),    
            {Client3, {ok, Address_x}} = thrift_client:call(Client2, malloc_int32_t, [Size]),
            {Client4, {ok, ok}} = thrift_client:call(Client3, send_data_int32_t, [Address_x, X]),

            {Client5, {ok, Address_y}} = thrift_client:call(Client4, malloc_int32_t, [Size]),
            {Client6, {ok, ok}} = thrift_client:call(Client5, send_data_int32_t, [Address_y, Y]),
            io:format("Sending input data:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime4) / 1000000]),

            % Allocate memory for output stream on server
            StartTime5 = erlang:timestamp(),
            {Client7, {ok, Address_dataOut}} = thrift_client:call(Client6, malloc_int32_t, [Size]),
            io:format("Allocating memory for output stream on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime5) / 1000000]),

            % Writing to LMem
            StartTime6 = erlang:timestamp(),
            {Client8, {ok, Actions_Lmem}} = thrift_client:call(Client7, max_actions_init, [Maxfile, "writeLMem"]),
            {Client9, {ok, ok}} = thrift_client:call(Client8, max_set_param_uint64t, [Actions_Lmem, "address", 0]),
            {Client10, {ok, ok}} = thrift_client:call(Client9, max_set_param_uint64t, [Actions_Lmem, "nbytes", SizeBytes]),
            {Client11, {ok, ok}} = thrift_client:call(Client10, max_queue_input, [Actions_Lmem, "cpu_to_lmem", Address_x, SizeBytes]),
            {Client12, {ok, ok}} = thrift_client:call(Client11, max_run, [Engine, Actions_Lmem]),
            io:format("Writing to LMmem:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime6) / 1000000]),

            % Action default
            StartTime7 = erlang:timestamp(),
            {Client13, {ok, Actions}} = thrift_client:call(Client12, max_actions_init, [Maxfile, "default"]),
            {Client14, {ok, ok}} = thrift_client:call(Client13, max_set_param_uint64t, [Actions, "N", Size]),
            {Client15, {ok, ok}} = thrift_client:call(Client14, max_set_param_uint64t, [Actions, "A", Scalar]),
            {Client16, {ok, ok}} = thrift_client:call(Client15, max_queue_input, [Actions, "y", Address_y, SizeBytes]),
            {Client17, {ok, ok}} = thrift_client:call(Client16, max_queue_output, [Actions, "s", Address_dataOut, SizeBytes]),
            {Client18, {ok, ok}} = thrift_client:call(Client17, max_run, [Engine, Actions]),
            io:format("Vector addition time:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime7) / 1000000]),

            % Unload DFE
	    StartTime8 = erlang:timestamp(),
            {Client19, {ok, ok}} = thrift_client:call(Client18, max_unload, [Engine]),
	    io:format("Unloading DFE:\t\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime8) / 1000000]),

            % Get output stream from server
            StartTime9 = erlang:timestamp(),
            {Client20, {ok, DataOut}} = thrift_client:call(Client19, receive_data_int32_t, [Address_dataOut, Size]),
            io:format("Getting output stream:\t(size = ~ws bit)\t~ws~n", [Size * 32, timer:now_diff(erlang:timestamp(), StartTime9) / 1000000]),

            % Free allocated memory for streams on server
            StartTime10 = erlang:timestamp(),
            {Client21, {ok, ok}} = thrift_client:call(Client20, free, [Address_x]),
            {Client22, {ok, ok}} = thrift_client:call(Client21, free, [Address_y]),
            {Client23, {ok, ok}} = thrift_client:call(Client22, free, [Address_dataOut]),
            io:format("Freeing allocated memory for streams on server:\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime10) / 1000000]),

            % Free allocated maxfile data
	    StartTime11 = erlang:timestamp(),
	    {Client24, {ok, ok}} = thrift_client:call(Client23, 'VectorAddition_free', []),
	    io:format("Freeing allocated maxfile data:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime11) / 1000000]),

            % Close!
            StartTime12 = erlang:timestamp(),
            {_Client25, ok} = thrift_client:close(Client24),
            io:format("Closing connection:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime12) / 1000000]),

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
    X = [random:uniform(1000) || _ <- lists:seq(1, Size)],
    Y = [random:uniform(1000) || _ <- lists:seq(1, Size)],
    Scalar = 3,
    io:format("Generating input data:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime1) / 1000000]),

    % DFE Output
    StartTime2 = erlang:timestamp(),
    DataOutDFE = vectorAdditionDFE(Size, X ,Y, Scalar),
    io:format("DFE vector addition total time:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime2) / 1000000]),

    % CPU Output
    StartTime3 = erlang:timestamp(),
    DataOutCPU = vectorAdditionCPU(X, Y, Scalar),
    io:format("CPU vector addition total time:\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime3) / 1000000]),

    % Checking results
    StartTime4 = erlang:timestamp(),
    Status = check(DataOutDFE, DataOutCPU, 0, 0),
    io:format("Checking results:\t\t\t\t~ws~n", [timer:now_diff(erlang:timestamp(), StartTime4) / 1000000]),

    if Status =:= 0    ->    io:format("Test successful!~n");
       true            ->    io:format("Test failed ~w times!~n", [Status]),
                             erlang:exit(-1)
    end,
    
    ok.
