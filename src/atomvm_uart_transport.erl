-module(atomvm_uart_transport).
-behaviour(purr_transport).
-export([
        init/1,
        read/1,
        write/2
        ]).

init(Arguments) ->
    apply(uart, open, Arguments).

read(State) ->
    uart:read(State).

write(State, Data) ->
    uart:write(State, Data).
