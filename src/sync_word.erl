-module(sync_word).

-export([find/2]).

find(Same, Same) ->
    match;
find(<<>>, _) ->
    mismatch;
find(<<_:1/binary, PostfixReceived/binary>> = Received, SyncWord) ->
    case {binary:longest_common_prefix([Received, SyncWord]), size(Received), size(SyncWord)} of
        {0, _, _} ->
            % not even the first byte matches
            % so we assume a garbage prefix andkeep trying without
            find(PostfixReceived, SyncWord);
        {ML, RL, SL} when (ML < RL) and (ML < SL) ->
            % neither side fully matches
            mismatch;
        {ML, RL, SL} when (ML < RL) and (ML == SL) ->
            % the start sequence matched but there is a rest of received data
            <<_:ML/binary, ReceivedRest/binary>> = Received,
            {match, ReceivedRest};
        {ML, RL, SL} when (ML == RL) and (ML < SL) ->
            % the received data matched but there is a rest from the start sequence remaining
            <<_:ML/binary, SyncWordRest/binary>> = SyncWord,
            {continue, SyncWordRest}
    end.

