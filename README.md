# Scheme R5RS &emsp; [![License Badge]][License] [![Test Badge]][Test] [![Deploy Badge]][Deploy]

[License Badge]: https://img.shields.io/badge/license-MIT%2FApache--2.0-blue?style=flat&logo=appveyor
[License]: LICENSE-MIT
[Test Badge]: https://github.com/strtok/lisp/actions/workflows/test.yml/badge.svg
[Test]: https://github.com/strtok/lisp/actions/workflows/test.yml
[Deploy Badge]: https://github.com/strtok/lisp/actions/workflows/deploy.yml/badge.svg
[Deploy]: https://github.com/strtok/lisp/actions/workflows/deploy.yml
This is a work in progress [Scheme R5RS](https://schemers.org/Documents/Standards/R5RS/) virtual machine for rust, featuring:

* a lexer, parser, compiler and virtual machine
* a terminal based repl using [rustyline](https://github.com/kkawakam/rustyline)
* a Web Aseembly based repl using [xterm-js-rs](https://github.com/segeljakt/xterm-js-rs) auto deployed to [github pages](https://strtok.github.io/lisp/)

# Virtual Machine

## Registers

Register | Description
---------|------------
ACC      | Accumulator
IP       | Instruction Pointer

## OpCodes

Opcode | Description
-------|------------
ADD    | Perform addition on the values in ACC and the the top of the stack and store the result in ACC
CAR    | Move the car of the pair in ACC into ACC. Errors if ACC Is not a pair.
CDR    | Move the cdr of the pair in ACC into ACC. Errors if ACC is not a pair.
CONS   | Box a pair, storing a reference in ACC. The car of the pair is taken from ACC, and the cdr from the top of the stack.
ENVGET &lt;SLOT&gt; | Store the value in environment slot SLOT in ACC
ENVSET &lt;SLOT&gt; | Set the value in environment slot SLOT to the value in ACC
EQ     | Sets ACC to true if ACC == arg[0], otherwise false. This instruction mirrors the eq? procedure in scheme.
HALT   | Halt program, returning the result contained within ACC
MUL    | Perform multiplication on the values in ACC and the the top of the stack and store the result in ACC
PUSH   | Push the value in ACC on to the stack
QUOTE &lt;VAL&gt; | Move VAL into ACC
SUB    | Perform subtraction on the values in ACC and the the top of the stack and store the result in ACC

# License
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a>.
