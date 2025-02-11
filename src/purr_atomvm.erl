%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(purr_atomvm).
-export([start/0]).

-define(SYNCWORD, <<"PurrpUrrpuRRLFG">>).

start() ->
    UART = uart:open("UART1", [{rx, 17}, {tx, 18}]),
    loop(UART, ?SYNCWORD, [?SYNCWORD]).

loop(UART, StartSequence, [ToSend | ToSendRest]) ->
    timer:sleep(500),
    io:format("Sending...~p~n", [ToSend]),
    uart:write(UART, [ToSend]),
    {ok, Received} = uart:read(UART),
    io:format("Received...~p~n", [Received]),
    Reply = find_sync_word(Received, StartSequence),
    io:format("Reply...~p~n", [Reply]),
    case Reply of
        match ->
            Reply;
        mismatch ->
            loop(UART, ?SYNCWORD, ToSendRest);
        {match, _ReceiveRest} ->
            Reply;
        {continue, StartSequenceRest} ->
            loop(UART, StartSequenceRest, ToSendRest)
    end.


find_sync_word(Same, Same) ->
    match;
find_sync_word(Received, StartSequence) ->
    case {binary:longest_common_prefix([Received, StartSequence]), length(Received), length(StartSequence)} of
        {ML, RL, SL} when (ML < RL) and (ML < SL) ->
            % neither side fully matches
            mismatch;
        {ML, RL, SL} when (ML < RL) and (ML == SL) ->
            % the start sequence matched but there is a rest of received data
            <<StartSequence, ReceivedRest>> = Received,
            {match, ReceivedRest};
        {ML, RL, SL} when (ML == RL) and (ML < SL) ->
            % the received data matched but there is a rest from the start sequence remaining
            <<Received, StartSequenceRest>> = StartSequence,
            {continue, StartSequenceRest}
    end.

