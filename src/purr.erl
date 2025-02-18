-module(purr).
-behaviour(gen_server).
-export([start/0]).

-export([start_link/2]).
-export([rpc/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SYNCWORD, <<"PuRRpUrrPurrLFG">>).
-define(MESSAGEPREFIX, <<"purr">>).
-define(SYNCINTERVAL, 500).

-record(state, {transport, transport_state, mode, reader_pid, syncword, receive_buffer, calls, next_call_id}).
-record(rpc, {call_id, module, function, arguments}).
-record(rpc_reply, {call_id, reply}).

start() ->
    start_link(atomvm_uart_transport, ["UART1", [{rx, 17}, {tx, 18}]]),
    timer:sleep(100),
    request_loop(0).

request_loop(I) ->
    timer:sleep(10),
    try
        Reply = rpc(lists, seq, [1, 10], 5000),
        io:format("~p: reply ~p: ~p ~n", [?MODULE, I, Reply])
    catch
        exit:Reason ->
            io:format("~p: call ~p failed: ~p ~n", [?MODULE, I, Reason])
    end,
    request_loop(I + 1).

start_link(Transport, TransportParameters) ->
    {ok, _Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Transport, TransportParameters], []).

rpc(Module, Function, Arguments) ->
    rpc(Module, Function, Arguments, 5000).
rpc(Module, Function, Arguments, Timeout) ->
    gen_server:call(?MODULE, {rpc, Module, Function, Arguments, Timeout}, Timeout).

init([Transport, TransportParameters]) ->
    TransportState = Transport:init(TransportParameters),
    ServerPid = self(),
    ReaderPid = spawn_link(fun() -> read_loop(Transport, TransportState, ServerPid) end),
    InitialState = #state{
        transport = Transport,
        transport_state = TransportState,
        mode = syncing,
        syncword = ?SYNCWORD,
        reader_pid = ReaderPid,
        receive_buffer = <<>>,
        calls = maps:new(),
        next_call_id = 1
    },
    self() !  send_sync,
    {ok, InitialState}.

handle_call({rpc, Module, Function, Arguments, Timeout}, Caller, State = #state{transport = Transport, transport_state = TransportState, mode = online, calls = Calls, next_call_id = CallId}) ->
    Call = #rpc{
        call_id = CallId,
        module = Module,
        function = Function,
        arguments = Arguments
    },
    CallBinary = encode(Call),
    Payload = <<?MESSAGEPREFIX/binary, (size(CallBinary)):32/integer, CallBinary/binary>>,
    Transport:write(TransportState, Payload),
    NextCalls = maps:put(CallId, Caller, Calls),
    % if the call never returns we need to make sure
    % the call data is cleaned up
    erlang:send_after(Timeout + 50, self(), {clean_call, CallId}),
    {noreply, State#state{next_call_id = CallId + 1, calls = NextCalls}};
handle_call({rpc, _, _, _, _}, _From, State) ->
    {reply, {error, not_online}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(send_sync, State = #state{transport = Transport, transport_state = TransportState, mode = Mode}) ->
    % io:format("~p: state: ~p~n", [?MODULE, State]),
    Transport:write(TransportState, ?SYNCWORD),
    erlang:send_after(?SYNCINTERVAL, self(), send_sync),
    case Mode of
        syncing ->
            io:format("~p: syncing...~n", [?MODULE]);
        _ ->
            noop
    end,
    {noreply, State};
handle_info({received, Data}, State = #state{mode = syncing, syncword = Syncword}) ->
    case sync_word:find(Data, Syncword) of
        {continue, SyncWordRest} ->
            {noreply, State#state{syncword = SyncWordRest}};
        match ->
            {noreply, State#state{mode = online, syncword = ?SYNCWORD}};
        {match, AdditionalData} ->
            self() ! {received, AdditionalData},
            {noreply, State#state{mode = online, syncword = ?SYNCWORD}};
        mismatch ->
            io:format("~p: failed to syn~n", [?MODULE]),
            {noreply, State}
    end;
handle_info({received, Data}, State = #state{mode = online, receive_buffer = ReceiveBuffer, calls = Calls}) ->
    ReceivedData = <<ReceiveBuffer/binary, Data/binary>>,
    case maybe_parse_received_data(ReceivedData) of
        {rpc, RPC = #rpc{}, RestReceived} ->
            ServerPid = self(),
            spawn(fun() -> call_rpc(RPC, ServerPid) end),
            {noreply, State#state{receive_buffer = RestReceived}};
        {reply, #rpc_reply{call_id = CallId, reply = Reply}, RestReceived} ->
            % we got a reply and potentially more data

            {ok, Caller} = maps:find(CallId, Calls),
            NextCalls = maps:remove(CallId, Calls),
            % io:format("~p: Replying to ~p~n", [?MODULE, CallId]),
            gen_server:reply(Caller, Reply),
            {noreply, State#state{receive_buffer = RestReceived, calls = NextCalls}};
        incomplete ->
            % we are still waiting for the full reply
            {noreply, State};
        synced ->
            % it's a sync message
            % lets continue
            {noreply, State};
        error ->
            % something went wrong, we loop the data in sync mode
            % io:format("~p: cant parse ~p, syncing~n", [?MODULE, ReceivedData]),

            self() ! {received, ReceivedData},
            {noreply,State#state{receive_buffer = <<>>, mode = syncing}}
    end;
handle_info({reply, Data}, State = #state{transport = Transport, transport_state = TransportState, mode = online}) ->
    Transport:write(TransportState, Data),
    {noreply, State};
handle_info(Msg = {reply, _}, State = #state{mode = syncing}) ->
    % we are currently syncing
    % we put the message back in the inbox
    % for later
    % io:format("~p: message while syncing: ~p ~p~n", [?MODULE, Msg, State]),
    self() ! Msg,
    {noreply, State};
handle_info({clean_call, CallId}, State = #state{calls = Calls}) ->
    case Calls of
        #{ CallId := _} ->
            % io:format("~p: cleaning up call ~p~n", [?MODULE, CallId]),
            NextCalls = maps:remove(CallId, Calls),
            {noreply, State#state{calls = NextCalls}};
        _ ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% read loop
read_loop(Transport, TransportState, ReplyPid) ->
    {ok, Data} = Transport:read(TransportState),
    ReplyPid ! {received, Data},
    read_loop(Transport, TransportState, ReplyPid).

%% receiving data
maybe_parse_received_data(?SYNCWORD) ->
    synced;
maybe_parse_received_data(<<MessagePrefix:(byte_size(?MESSAGEPREFIX))/binary, Payload/binary>>) ->
    case MessagePrefix of
        ?MESSAGEPREFIX -> parse_received_data(Payload);
        _ -> error
    end;
maybe_parse_received_data(_) ->
    incomplete.

parse_received_data(<<Length:32/integer, BinaryData:Length/binary, RestReceived/binary>>) ->
   MaybeData =
   try decode(BinaryData) of
        Data -> Data
   catch error:Error ->
        {error, Error}
   end,
   case MaybeData of
        {error, _} ->
           error;
        Reply = #rpc_reply{} ->
           {reply, Reply, RestReceived};
        Rpc = #rpc{} ->
           {rpc, Rpc, RestReceived}
   end;
parse_received_data(_) ->
    incomplete.

%% running local RPCs
call_rpc(#rpc{call_id = CallId, module = Module, function = Function, arguments = Arguments}, ReplyPid) ->
    CallReply =
    try apply(Module, Function, Arguments) of
        Result ->
            {ok, Result}
        catch
            error:Error ->
                {error, {error, Error}};
            throw:Throw ->
                {error, {throw, Throw}};
            exit:Exit ->
                {error, {exit, Exit}}
    end,
    Reply = #rpc_reply{call_id = CallId, reply = CallReply},
    ReplyBinary = encode(Reply),
    Payload = <<?MESSAGEPREFIX/binary, (size(ReplyBinary)):32/integer, ReplyBinary/binary>>,
    ReplyPid ! {reply, Payload}.

encode(Term) ->
    term_to_binary(Term).
decode(Binary) ->
    binary_to_term(Binary).
