-module(purr_atomvm_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Setup ---------------------------------------------------------------------

syncword_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun test_init/0
    ]}.

setup() ->
    meck:new(uart, [non_strict]),
    meck:expect(uart, open, fun(_, _) -> uart_mock end),
    meck:expect(uart, read, fun(_) -> receive never -> return end end),
    meck:expect(uart, write, fun(_, _) -> ok end),
    ok.

teardown(_) ->
    meck:unload(uart),
    ok.

%--- Tests ---------------------------------------------------------------------

test_init() ->
    ?assertMatch(x, purr_atomvm:init([])).
