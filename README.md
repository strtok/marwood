# _λ_ Marwood &emsp; [![License Badge]][License] [![Test Badge]][Test] [![Deploy Badge]][Deploy]

[License Badge]: https://img.shields.io/badge/license-MIT%2FApache--2.0-blue?style=flat&logo=appveyor
[License]: LICENSE-MIT
[Test Badge]: https://github.com/strtok/marwood/actions/workflows/test.yml/badge.svg
[Test]: https://github.com/strtok/marwood/actions/workflows/test.yml
[Deploy Badge]: https://github.com/strtok/marwood/actions/workflows/deploy.yml/badge.svg
[Deploy]: https://github.com/strtok/marwood/actions/workflows/deploy.yml
Marwood is a [Scheme R5RS](https://schemers.org/Documents/Standards/R5RS/) implementation for rust, featuring:

* a scheme r5rs compiler & 64 bit virtual machine composed of 3x64bit cells
* mark & sweep garbage collection
* a terminal based repl using [rustyline](https://github.com/kkawakam/rustyline)
* a WASM target repl using [xterm-js-rs](https://github.com/segeljakt/xterm-js-rs) auto deployed to [github pages](https://strtok.github.io/marwood/)

# Design

## VCells

Marwood's virtual machine is composed of 24 byte (3x64 bit) [VCells](marwood/src/vm/vcell.rs) 
represented by rust enums. The VCell type is universal and used to represent values
in the various Marwood regions (stack, heap, bytecode, global and local environment).

These cells are used to represent:

* Primitive scheme types
    - Fixednums
    - Bools
    - Pairs
    - Strings
    - Symbols
* Instruction op codes
* Instruction operands
* Heap and environment pointers

## Heap

Marwood's [heap](marwood/src/vm/heap.rs) is composed of a growbable vector of VCell
slots. [Garbage collection](marwood/src/vm/gc.rs) is performed via a root based 
mark & sweep.

## Global Environment

Marwood maintains a global [environment](marwood/src/vm/environment.rs), which may 
be manipulated by the scheme `define` procedure. This environment represents a global 
mapping of symbol => heap reference for the compiler, which in turn emits O(1) shallow 
bindings (environment slot references) for the runtime.

# License
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a>.
