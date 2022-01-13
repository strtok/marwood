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

## Lexical Environment

Marwood maintains a per-procedure lexical environment represented by a vector of 
shallow bindings. At compile time, Marwood computes the exact mapping of bindings inherited 
by a procedures immediate-outer-function (IOF), building an EnvironmentMap that instructs 
the runtime of what bindings to inherit from the IOF's Environment and Stack.

## Procedure Application

Marwood uses the stack to pass each operand of a procedure being applied to
the procedure, and the `%acc` register to store the result of a procedure
evaluation.

Consider the following definition and application of an add procedure:

```scheme
  (define add (lambda (x y) (+ x y)))
  (add 10 20)
```

In compiling the expression `(add 10 20)`, marwood would emit instructions
to perform the following:

* Evaluate the operands `(10 20)` from left-to-right and push the results of 
each evaluation on the stack
* Push the number of operands (in this case 2) on the stack. The procedure being 
  applied will use this value for error checking and frame restoring.
* Evaluate the procedure expression `add`. At this point in time %acc 
  contains a reference to the procedure being applied.
* Emit a `CALL %acc` instruction

### CALL

The CALL instruction expects the following conditions:

* The operands for the procedure being applied have been pushed on the stack
* The top of the stack is the number of operands that were pushed.
* %acc contains a reference to a procedure

CALL will perform the following:

* Error if %acc is not a procedure
* PUSH %ep
* PUSH %ip
* MOV %acc %ip

Here's an example of an immediate-outer-function's (IOF) application of the `add` procedure:

```
    MOV      $07          %acc         //10
    PUSH     %acc
    MOV      $08          %acc         //20
    PUSH     %acc
    PUSH     2
    MOV      [genv[$00]]  %acc         //#<procedure>
    CALL     %acc
    HALT
```
After the CALL instruction, the stack should appear as follows:
```
    +----------------------------+
    |             %ip            | <= %sp
    +----------------------------+
    |             %ep            |
    +----------------------------+
    |             n=2            |
    +----------------------------+
    |             20             |
    +----------------------------+
    |             10             |
    +----------------------------+
```

### ENTER

The procedure's byte code will immediately execute the ENTER instruction to
finish setting up the stack. 

The ENTER instruction performs the following in order:
* 
* Error if SP[-2] does not match the lambda's expected operand count
* PUSH %bp
* MOV %sp[-4] %bp

Offsets to %bp will be used by the procedure to access arguments on the stack
as seen below.

Here's the instructions for the example `add` procedure:

```
ENTER
MOV      [%bp[-1]]    %acc
PUSH     %acc
MOV      [%bp[+0]]    %acc
ADD
RET
```

And the stack after ENTER has finished executing:

```
    +----------------------------+
    |             %bp            | <= %sp
    +----------------------------+
    |             %ip            |
    +----------------------------+
    |             %ep            |
    +----------------------------+
    |             n=2            |
    +----------------------------+
    |             20             | <= %bp
    +----------------------------+
    |             10             |
    +----------------------------+
```

### RET

The RET instruction performs the following in order:

* Restore caller's %sp
* Restore caller's %ip
* Restore caller's %bp

The result of a procedure evaluation in Marwood is always stored in %acc. After a RET 
instruction, execution will proceed beginning with the caller's %ip+1. In the example 
above, the HALT instruction of the caller will execute immediately after the RET instruction 
of the `add` procedure.

## Registers

| Register | Description         |
|----------|---------------------|
| %acc     | Accumulator         |
| %ip      | Instruction Pointer |
| %bp      | Frame Base Pointer  |
| %sp      | Stack Pointer       |
| %ep      | Environment Pointer |

## OpCodes

| Opcode                       | Description                                                                                                           |
|------------------------------|-----------------------------------------------------------------------------------------------------------------------|
| ADD                          | Perform addition on the values in ACC and the the top of the stack and store the result in ACC                        |
| CALL %acc                    | Given the arguments for the procedure in %acc have been pushed on the stack, jump to the procedure in %acc.           |
| CAR                          | Move the car of the pair in ACC into ACC. Errors if ACC Is not a pair.                                                |
| CDR                          | Move the cdr of the pair in ACC into ACC. Errors if ACC is not a pair.                                                |
| CONS                         | Box a pair, storing a reference in ACC. The car of the pair is taken from ACC, and the cdr from the top of the stack. |
| CLOSURE %acc                 | Create a lexical environment as a result of evaluating a (lambda ...) expression.                                     |
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