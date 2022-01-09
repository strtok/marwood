# _λ_ Marwood Design &emsp; [![License Badge]][License] [![Test Badge]][Test] [![Deploy Badge]][Deploy]

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

### Procedure Application

Consider the following definition and application of add1:
```scheme
  (define add (lambda (x y) (+ x y)))
  (add (+ 1 1) (* 2 21))
```
The compiler will evaluate each operand and push its result on
the stack left to right, push %ep and then %ip, resulting in the
following stack:

```
    +----------------------------+
    |             %ip            |
    +----------------------------+
    |             %ep            |
    +----------------------------+
    |             n=2            |
    +----------------------------+
    |             42             |
    +----------------------------+
    |             2              |
    +----------------------------+
```

## Registers

| Register | Description         |
|----------|---------------------|
| ACC      | Accumulator         |
| IP       | Instruction pointer |
| BP       | Frame base pointer  |
| SP       | Stack Pointer       |

## OpCodes

| Opcode                       | Description                                                                                                           |
|------------------------------|-----------------------------------------------------------------------------------------------------------------------|
| ADD                          | Perform addition on the values in ACC and the the top of the stack and store the result in ACC                        |
| CALL %acc                    | Given the arguments for the procedure in %acc have been pushed on the stack, jump to the procedure in %acc.           |
| CAR                          | Move the car of the pair in ACC into ACC. Errors if ACC Is not a pair.                                                |
| CDR                          | Move the cdr of the pair in ACC into ACC. Errors if ACC is not a pair.                                                |
| CONS                         | Box a pair, storing a reference in ACC. The car of the pair is taken from ACC, and the cdr from the top of the stack. |
| ENVGET &lt;SLOT&gt;          | Store the value in environment slot SLOT in ACC                                                                       |
| ENVSET &lt;SLOT&gt;          | Set the value in environment slot SLOT to the value in ACC                                                            |
| ENTER                        | Setup the currently executing procedure's stack frame                                                                 |
| EQ                           | Sets ACC to true if ACC == arg[0], otherwise false. This instruction mirrors the eq? procedure in scheme.             |
| HALT                         | Halt program, returning the result contained within ACC                                                               |
| MOV &lt;SRC&gt; &lt;DEST&gt; | Move the value from SRC into DEST                                                                                     |
| MUL                          | Perform multiplication on the values in ACC and the the top of the stack and store the result in ACC                  |
| PUSH                         | Push the value in ACC on to the stack                                                                                 |
| RET                          | Return from a procedure entered via CALL                                                                              |
| SUB                          | Perform subtraction on the values in ACC and the the top of the stack and store the result in ACC                     |

# License
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a>.
