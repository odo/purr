# Purr: RPC library from Erlang-driven microcontrollers

This library provides a way to do remote procedure call (RPC) between Erlang driven microcontrollers or computers.

> [!NOTE]
> This project is under decelopment and is not recemmended for production yet.

## Characteristics
### Fault tolerance
If the connection is disrupted or data is corrupted in traffic, Purr will recover.

### No Head-of-line blocking
If a long-running call is requested, quicker calls that are issued later can be replied to earlier.

## Supported transports:
### UART on ESP32 on AtomVM
[AtomVM](http://atomvm.net) is a virtual machine for microcontroller like the [ESP32](https://www.espressif.com/en/products/socs/esp32).
Purr uses the [UART driver](https://github.com/atomvm/AtomVM/blob/7caa5663675a4cbe38fbf8ac45cc0f4e9b58f71d/libs/eavmlib/src/uart.erl) to implement
communication over a universal asynchronous receiver-transmitter (UART) interface.

## Usage
`purr` is a gen_server that is started with the transport module and setup arguments
(UART unit and pins in the example below).

Calls are made with `purr:rpc/3` and `purr:rpc/4` with module, function and arguments as parameters.
Note that the modules that are called don't need to be available on the calling side.

```erlang
purr:start_link(atomvm_uart_transport, ["UART1", [{rx, 17}, {tx, 18}]]),
{ok, [1,2,3]} = purr:rpc(lists, seq, [1, 3]).
```
