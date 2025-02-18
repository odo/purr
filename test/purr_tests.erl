-module(purr_tests).

-include_lib("eunit/include/eunit.hrl").
-behaviour(purr_transport).
-export([
        init/1,
        read/1,
        write/2
        ]).

%--- We are defining our test transport ---------------------------------------------------------------------

init(_) ->
    nil.
read(_) ->
    receive
        never -> return
    end.
write(_, Data) ->
    ets:insert(test_table, {transport_write, Data}).

%--- Setup ---------------------------------------------------------------------

syncword_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun test_init/0,
        fun test_plain_syncing/0,
        fun test_syncing_with_prefix/0,
        fun test_syncing_in_parts/0,
        fun test_failed_syncing/0,
        fun test_roundtrip/0,
        fun test_double_roundtrip/0,
        fun test_recovery/0
    ]}.

setup() ->
    ets:new(test_table, [set, public, named_table]),
    ok.

teardown(_) ->
    ets:delete(test_table),
    ok.

%--- Tests ---------------------------------------------------------------------

test_init() ->
    {ok, InitState} = purr:init([?MODULE, nil]),
    ?assertMatch(syncing, mode(InitState)).

test_plain_syncing() ->
    {noreply, SyncState} = purr:handle_info({received, <<"PuRRpUrrPurrLFG">>}, init_state()),
    ?assertMatch(online, mode(SyncState)).

test_syncing_with_prefix() ->
    {noreply, SyncState} = purr:handle_info({received, <<"foo bar PuRRpUrrPurrLFG">>}, init_state()),
    ?assertMatch(online, mode(SyncState)).

test_syncing_in_parts() ->
    {noreply, HalfSyncState} = purr:handle_info({received, <<"prefix PuRRpUrr">>}, init_state()),
    ?assertMatch(syncing, mode(HalfSyncState)),
    {noreply, SyncState} = purr:handle_info({received, <<"PurrLFG postfix">>}, HalfSyncState),
    ?assertMatch(online, mode(SyncState)).

test_failed_syncing() ->
    {noreply, SyncState} = purr:handle_info({received, <<"PuRRpXrrPurrLFG">>}, init_state()),
    ?assertMatch(syncing, mode(SyncState)).

test_roundtrip() ->
    GenServerRef = make_ref(),
    % requesting side
    {noreply, RequestingState} = purr:handle_call({rpc, lists, seq, [1, 7], 1000}, {self(), GenServerRef}, online_state()),
    {ok, RpcRequest} = get_transport_write(),

    %replying side
    {noreply, _} = purr:handle_info({received, RpcRequest}, online_state()),
    {reply, RpcReply} = get_rpc_result(),
    purr:handle_info({reply, RpcReply}, online_state()),
    {ok, RpcReply} = get_transport_write(),

    % requesting side
    purr:handle_info({received, RpcReply}, RequestingState),
    {GenServerRef, {ok, [1,2,3,4,5,6,7]}} = get_rpc_result().

test_double_roundtrip() ->
    % requesting side
    % request1
    GenServerRef1 = make_ref(),
    {noreply, RequestingStateA} = purr:handle_call({rpc, lists, seq, [1, 7], 1000}, {self(), GenServerRef1}, online_state()),
    {ok, RpcRequest1} = get_transport_write(),
    % request2
    GenServerRef2 = make_ref(),
    {noreply, RequestingStateB} = purr:handle_call({rpc, lists, seq, [1, 3], 1000}, {self(), GenServerRef2}, RequestingStateA),
    {ok, RpcRequest2} = get_transport_write(),

    % we are concatenating the two requests in and split them
    % at a random point
    % each request is 35 bytes long
    ConcatRequest = <<RpcRequest2/binary, RpcRequest1/binary>>,
    <<RequestFragment1:50/binary, RequestFragment2:20/binary>> = ConcatRequest,

    %replying side
    {noreply, ReplyingStateA} = purr:handle_info({received, RequestFragment1}, online_state()),
    {reply, RpcReply1} = get_rpc_result(),
    {noreply, ReplyingStateB} = purr:handle_info({reply, RpcReply1}, ReplyingStateA),
    {ok, RpcReply1} = get_transport_write(),

    % requesting side
    {noreply, RequestingStateC} = purr:handle_info({received, RpcReply1}, RequestingStateB),
    {GenServerRef2, {ok, [1,2,3]}} = get_rpc_result(),

    %replying side
    {noreply, ReplyingStateC} = purr:handle_info({received, RequestFragment2}, ReplyingStateB),
    {reply, RpcReply2} = get_rpc_result(),
    {noreply, _ReplyingStateD} = purr:handle_info({reply, RpcReply2}, ReplyingStateC),
    {ok, RpcReply2} = get_transport_write(),

    % requesting side
    {noreply, _RequestingStateD} = purr:handle_info({received, RpcReply2}, RequestingStateC),
    {GenServerRef1, {ok, [1,2,3,4,5,6,7]}} = get_rpc_result().

test_recovery() ->
    % requesting side
    % request1
    GenServerRef1 = make_ref(),
    {noreply, RequestingStateA} = purr:handle_call({rpc, lists, seq, [1, 7], 1000}, {self(), GenServerRef1}, online_state()),
    {ok, _RpcRequest1Original} = get_transport_write(),
    % request2
    GenServerRef2 = make_ref(),
    {noreply, RequestingStateB} = purr:handle_call({rpc, lists, seq, [1, 3], 1000}, {self(), GenServerRef2}, RequestingStateA),
    {ok, RpcRequest2} = get_transport_write(),

    % we are messing up the first request
    % at a random point
    % each request is 35 bytes long
    RpcRequest1 = <<"this is a completely unreadable message no one can read!">>,

    % replying side
    {noreply, ReplyingStateA} = purr:handle_info({received, RpcRequest1}, online_state()),
    % the faulty request is barfed back at us
    % so we feed it back and add a sync word
    {received, RpcRequest1} = get_rpc_result(),
    {noreply, ReplyingStateB} = purr:handle_info({received, RpcRequest1}, ReplyingStateA),
    {noreply, ReplyingStateC} = purr:handle_info({received, <<"PuRRpUrrPurrLFG">>}, ReplyingStateB),

    % we confirm that the requesting side does not get anything (it will time out)
    {error, no_data} = get_transport_write(),

    % we can now continue with the second in tact request
    {noreply, ReplyingStateD} = purr:handle_info({received, RpcRequest2}, ReplyingStateC),

    {reply, RpcReply2} = get_rpc_result(),
    {noreply, _ReplyingStateE} = purr:handle_info({reply, RpcReply2}, ReplyingStateD),
    {ok, RpcReply2} = get_transport_write(),

    % requesting side
    {noreply, _RequestingStateC} = purr:handle_info({received, RpcReply2}, RequestingStateB),
    {GenServerRef2, {ok, [1,2,3]}} = get_rpc_result().


% internal
init_state() ->
    {ok, State} = purr:init([?MODULE, nil]),
    State.

online_state() ->
    {noreply, SyncState} = purr:handle_info({received, <<"PuRRpUrrPurrLFG">>}, init_state()),
    SyncState.

mode(State) ->
    element(4, State).

get_transport_write() ->
    case ets:lookup(test_table, transport_write) of
        [{transport_write, Data}] ->
            ets:delete(test_table, transport_write),
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
