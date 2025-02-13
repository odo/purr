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
-define(SYNCINTERVAL, 250).

-record(state, {uart, mode, reader_pid, syncword, receive_buffer = <<>>}).
-record(rpc, {caller, module, function, arguments}).
-record(rpc_reply, {caller, reply}).

start() ->
    start_link(),
    receive
        never -> ok
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

rpc(Module, Function, Arguments) ->
    gen_server:call(?MODULE, {rpc, Module, Function, Arguments}).

init([]) ->
    UART = uart:open("UART1", [{rx, 17}, {tx, 18}]),
    ServerPid = self(),
    ReaderPid = spawn_link(fun() -> read_loop(UART, ServerPid) end),
    InitialState = #state{
        uart = UART,
        mode = syncing,
        syncword = ?SYNCWORD,
        reader_pid = ReaderPid
    },
    self() !  send_sync,
    {ok, InitialState}.

handle_call({rpc, Module, Function, Arguments}, Caller, State = #state{mode = online, uart = UART}) ->
    Call = #rpc{
        caller = Caller,
        module = Module,
        function = Function,
        arguments = Arguments
    },
    CallBinary = encode(Call),
    Payload = <<?MESSAGEPREFIX/binary, (size(CallBinary)):32/integer, CallBinary/binary>>,
    uart:write(UART, Payload),
    {noreply, State};
handle_call({rpc, _, _, _}, _From, State) ->
    {reply, {error, not_online}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(send_sync, State = #state{uart = UART, mode = Mode}) ->
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
handle_info({received, Data}, State = #state{mode = online, receive_buffer = ReceiveBuffer}) ->
    ReceivedData = <<ReceiveBuffer/binary, Data/binary>>,
    case maybe_parse_received_data(ReceivedData) of
        {rpc, RPC = #rpc{}, RestReceived} ->
            ServerPid = self(),
            spawn(fun() -> call_rpc(RPC, ServerPid) end),
            {noreply, State#state{receive_buffer = RestReceived}};
        {reply, #rpc_reply{caller = Caller, reply = Reply}, RestReceived} ->
            % we got a reply and potentially more data
            gen_server:reply(Caller, Reply),
            {noreply, State#state{receive_buffer = RestReceived}};
        incomplete ->
            % we are still waiting for the full reply
            {noreply, State};
        error ->
            % something went wrong, we loop the data in sync mode
            self() ! {received, ReceivedData},
            {noreply,State#state{receive_buffer = <<>>, mode = syncing}}
    end;
handle_info({reply, Data}, State = #state{mode = online, uart = UART}) ->
    uart:write(UART, Data),
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
call_rpc(#rpc{caller = Caller, module = Module, function = Function, arguments = Arguments}, ReplyPid) ->
    try apply(Module, Function, Arguments) of
        Result ->
            Reply = #rpc_reply{caller = Caller, reply = {ok, Result}},
            ReplyBinary = encode(Reply),
            Payload = <<?MESSAGEPREFIX/binary, (size(ReplyBinary)):32/integer, ReplyBinary/binary>>,

            ReplyPid ! {reply, Payload}
        catch
            error:Error ->
                ReplyPid ! {error, error, Error};
            throw:Throw ->
                ReplyPid ! {error, throw, Throw};
            exit:Exit ->
                ReplyPid ! {error, exit, Exit}
    end.

encode(Term) ->
    term_to_binary(Term).
decode(Binary) ->
    binary_to_term(Binary).

