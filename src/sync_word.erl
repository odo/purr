-module(sync_word).

-export([find/2]).

find(Received, SyncWord) ->
    find(Received, SyncWord, SyncWord).

find(Same, Same, _) ->
    match;
find(<<FirstByte:1/binary, ReceivedRest/binary>>, <<FirstByte:1/binary, SyncWordRest/binary>>, OriginalSyncWord) ->
    % the first bytes match, lets continue
    find(ReceivedRest, SyncWordRest, OriginalSyncWord);
find(ReceivedRest, <<>>, _) ->
    % the sync word matched but there is a rest of received data
    {match, ReceivedRest};
find(<<>>, CompleteSyncWord, CompleteSyncWord) ->
    mismatch;
find(<<>>, SyncWordRest, _) ->
    % the received data matched but there is a rest from the sync word remaining
    {continue, SyncWordRest};
find(<<_:1/binary, ReceivedRest/binary>>, OriginalSyncWord, OriginalSyncWord) ->
    % we did not reach the sync word yet
    % so we keep discarding prefixes
    find(ReceivedRest, OriginalSyncWord, OriginalSyncWord);
find(_, _, _) ->
    mismatch.
