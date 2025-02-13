-module(purr_atomvm).
-behaviour(gen_server).
-export([start/0]).

-export([start_link/0]).
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

-record(state, {uart, mode, reader_pid, syncword, receive_buffer, calls, next_call_id}).
-record(rpc, {call_id, module, function, arguments}).
-record(rpc_reply, {call_id, reply}).

start() ->
    start_link(),
    request_loop(0).

request_loop(I) ->
    timer:sleep(50),
    try
        Reply = rpc(math, pow, [I, 1], 200),
        io:format("~p: reply: ~p ~n", [?MODULE, Reply])
    catch
        exit:Reason ->
            io:format("~p: call ~p failed: ~p ~n", [?MODULE, I, Reason])
    end,
    request_loop(I + 1).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

rpc(Module, Function, Arguments) ->
    rpc(Module, Function, Arguments, 5000).
rpc(Module, Function, Arguments, Timeout) ->
    gen_server:call(?MODULE, {rpc, Module, Function, Arguments}, Timeout).

init([]) ->
    UART = uart:open("UART1", [{rx, 17}, {tx, 18}]),
    ServerPid = self(),
    ReaderPid = spawn_link(fun() -> read_loop(UART, ServerPid) end),
    InitialState = #state{
        uart = UART,
        mode = syncing,
        syncword = ?SYNCWORD,
        reader_pid = ReaderPid,
        receive_buffer = <<>>,
        calls = maps:new(),
        next_call_id = 1
    },
    self() !  send_sync,
    {ok, InitialState}.

handle_call({rpc, Module, Function, Arguments}, Caller, State = #state{mode = online, uart = UART, calls = Calls, next_call_id = NextCallId}) ->
    Call = #rpc{
        call_id = NextCallId,
        module = Module,
        function = Function,
        arguments = Arguments
    },
    CallBinary = encode(Call),
    Payload = <<?MESSAGEPREFIX/binary, (size(CallBinary)):32/integer, CallBinary/binary>>,
    uart:write(UART, Payload),
    NextCalls = maps:put(NextCallId, Caller, Calls),
    {noreply, State#state{next_call_id = NextCallId + 1, calls = NextCalls}};
handle_call({rpc, _, _, _}, _From, State) ->
    {reply, {error, not_online}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(send_sync, State = #state{uart = UART, mode = Mode}) ->
            io:format("~p: state: ~p~n", [?MODULE, State]),
    uart:write(UART, [?SYNCWORD]),
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
            io:format("~p: Replying to ~p~n", [?MODULE, CallId]),
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
            io:format("~p: cant parse ~p, syncing~n", [?MODULE, ReceivedData]),

            self() ! {received, ReceivedData},
            {noreply,State#state{receive_buffer = <<>>, mode = syncing}}
    end;
handle_info({reply, Data}, State = #state{mode = online, uart = UART}) ->
    uart:write(UART, Data),
    {noreply, State};
handle_info(Msg = {reply, _}, State = #state{mode = syncing}) ->
    % we are currently syncing
    % we put the message back in the inbox
    % for later
    io:format("~p: message while syncing: ~p ~p~n", [?MODULE, Msg, State]),
    self() ! Msg,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% read loop
read_loop(UART, ReplyPid) ->
    {ok, Data} = uart:read(UART),
    ReplyPid ! {received, Data},
    read_loop(UART, ReplyPid).

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
   catch error:Error -> {erorr, Error}
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

