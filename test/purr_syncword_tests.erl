-module(purr_syncword_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Setup ---------------------------------------------------------------------

syncword_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun test_match/0,
        fun test_mismatch1/0,
        fun test_mismatch2/0,
        fun test_mismatch3/0,
        fun test_undershoot/0,
        fun test_overshoot/0,
        fun test_prefix/0,
        fun test_multi_part_match/0,
        fun test_multi_part_mismatch/0
    ]}.

setup() ->
    ok.

teardown(_) ->
    ok.

%--- Tests ---------------------------------------------------------------------

test_match() ->
    ?assertMatch(match, sync_word:find(<<"same">>, <<"same">>)).

test_mismatch1() ->
    ?assertMatch(mismatch, sync_word:find(<<"syXc">>, <<"sync">>)).

test_mismatch2() ->
    ?assertMatch(mismatch, sync_word:find(<<"syX">>, <<"sync">>)).

test_mismatch3() ->
    ?assertMatch(mismatch, sync_word:find(<<"syXco">>, <<"sync">>)).

test_undershoot() ->
    ?assertMatch({continue, <<"nc">>}, sync_word:find(<<"sy">>, <<"sync">>)).

test_overshoot() ->
    ?assertMatch({match, <<"andmore">>}, sync_word:find(<<"syncandmore">>, <<"sync">>)).

test_prefix() ->
    ?assertMatch({match, <<"andmore">>}, sync_word:find(<<"prefixsyncandmore">>, <<"sync">>)),
    ?assertMatch({continue, <<"nc">>}, sync_word:find(<<"prefixsy">>, <<"sync">>)),
    ?assertMatch(mismatch, sync_word:find(<<"prefixsyXnc">>, <<"sync">>)).

test_multi_part_match() ->
    {continue, Part} = sync_word:find(<<"sy">>, <<"sync">>),
    ?assertMatch(Part, <<"nc">>),
    ?assertMatch(match,  sync_word:find(<<"nc">>, Part)),
    ?assertMatch({match, <<"plus">>},  sync_word:find(<<"ncplus">>, Part)).

test_multi_part_mismatch() ->
    {continue, Part} = sync_word:find(<<"sy">>, <<"sync">>),
    ?assertMatch(Part, <<"nc">>),
    ?assertMatch(mismatch,  sync_word:find(<<"nXY">>, Part)).

