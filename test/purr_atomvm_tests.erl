-module(purr_atomvm_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Setup ---------------------------------------------------------------------

syncword_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun test_init/0,
        fun test_plain_syncing/0,
        fun test_syncing_with_prefix/0,
        fun test_syncing_in_parts/0,
        fun test_failed_syncing/0,
        fun test_roundtrip/0,
        fun test_double_roundtrip/0
    ]}.

setup() ->
    TestTable = ets:new(test_table, [set, public, named_table]),
    meck:new(uart, [non_strict]),
    meck:expect(uart, open, fun(_, _) -> uart_mock end),
    meck:expect(uart, read, fun(_) -> receive never -> return end end),
    meck:expect(uart, write, fun(_, Data) -> ets:insert(TestTable, {uart_write, Data}) end),
    ok.

teardown(_) ->
    meck:unload(uart),
    ets:delete(test_table),
    ok.

%--- Tests ---------------------------------------------------------------------

test_init() ->
    {ok, InitState} = purr_atomvm:init([]),
    ?assertMatch(syncing, mode(InitState)).

test_plain_syncing() ->
    {noreply, SyncState} = purr_atomvm:handle_info({received, <<"PuRRpUrrPurrLFG">>}, init_state()),
    ?assertMatch(online, mode(SyncState)).

test_syncing_with_prefix() ->
    {noreply, SyncState} = purr_atomvm:handle_info({received, <<"foo bar PuRRpUrrPurrLFG">>}, init_state()),
    ?assertMatch(online, mode(SyncState)).

test_syncing_in_parts() ->
    {noreply, HalfSyncState} = purr_atomvm:handle_info({received, <<"prefix PuRRpUrr">>}, init_state()),
    ?assertMatch(syncing, mode(HalfSyncState)),
    {noreply, SyncState} = purr_atomvm:handle_info({received, <<"PurrLFG postfix">>}, HalfSyncState),
    ?assertMatch(online, mode(SyncState)).

test_failed_syncing() ->
    {noreply, SyncState} = purr_atomvm:handle_info({received, <<"PuRRpXrrPurrLFG">>}, init_state()),
    ?assertMatch(syncing, mode(SyncState)).

test_roundtrip() ->
    GenServerRef = make_ref(),
    % requesting side
    {noreply, RequestingState} = purr_atomvm:handle_call({rpc, lists, seq, [1, 7]}, {self(), GenServerRef}, online_state()),
    {ok, RpcRequest} = get_uart_write(),

    %replying side
    {noreply, _} = purr_atomvm:handle_info({received, RpcRequest}, online_state()),
    {reply, RpcReply} = get_rpc_result(),
    purr_atomvm:handle_info({reply, RpcReply}, online_state()),
    {ok, RpcReply} = get_uart_write(),

    % requesting side
    purr_atomvm:handle_info({received, RpcReply}, RequestingState),
    {GenServerRef, {ok, [1,2,3,4,5,6,7]}} = get_rpc_result().

test_double_roundtrip() ->
    % requesting side
    % request1
    GenServerRef1 = make_ref(),
    {noreply, RequestingStateA} = purr_atomvm:handle_call({rpc, lists, seq, [1, 7]}, {self(), GenServerRef1}, online_state()),
    {ok, RpcRequest1} = get_uart_write(),
    % request2
    GenServerRef2 = make_ref(),
    {noreply, RequestingStateB} = purr_atomvm:handle_call({rpc, lists, seq, [1, 3]}, {self(), GenServerRef2}, RequestingStateA),
    {ok, RpcRequest2} = get_uart_write(),

    % we are concatenating the two requests in and split them
    % at a random point
    % each request is 35 bytes long
    ConcatRequest = <<RpcRequest2/binary, RpcRequest1/binary>>,
    <<RequestFragment1:50/binary, RequestFragment2:20/binary>> = ConcatRequest,

    %replying side
    {noreply, ReplyingStateA} = purr_atomvm:handle_info({received, RequestFragment1}, online_state()),
    {reply, RpcReply1} = get_rpc_result(),
    {noreply, ReplyingStateB} = purr_atomvm:handle_info({reply, RpcReply1}, ReplyingStateA),
    {ok, RpcReply1} = get_uart_write(),

    % requesting side
    {noreply, RequestingStateC} = purr_atomvm:handle_info({received, RpcReply1}, RequestingStateB),
    {GenServerRef2, {ok, [1,2,3]}} = get_rpc_result(),

    %replying side
    {noreply, ReplyingStateC} = purr_atomvm:handle_info({received, RequestFragment2}, ReplyingStateB),
    {reply, RpcReply2} = get_rpc_result(),
    {noreply, _ReplyingStateD} = purr_atomvm:handle_info({reply, RpcReply2}, ReplyingStateC),
    {ok, RpcReply2} = get_uart_write(),

    % requesting side
    {noreply, _RequestingStateD} = purr_atomvm:handle_info({received, RpcReply2}, RequestingStateC),
    {GenServerRef1, {ok, [1,2,3,4,5,6,7]}} = get_rpc_result().



% internal
init_state() ->
    {ok, State} = purr_atomvm:init([]),
    State.

online_state() ->
    {noreply, SyncState} = purr_atomvm:handle_info({received, <<"PuRRpUrrPurrLFG">>}, init_state()),
    SyncState.

mode(State) ->
    element(3, State).

get_uart_write() ->
    case ets:lookup(test_table, uart_write) of
    [{uart_write, Data}] ->
        ets:delete(test_table, uart_write),
        {ok, Data};
    [] ->
        {error, no_data}
    end.

get_rpc_result() ->
    receive
        send_sync ->
            get_rpc_result();
        Any ->
            Any
    after
        10 ->
            {error, no_reply}
    end.
