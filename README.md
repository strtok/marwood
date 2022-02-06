# _λ_ Marwood &emsp; [![License Badge]][License] [![Test Badge]][Test] [![Deploy Badge]][Deploy]

[License Badge]: https://img.shields.io/badge/license-MIT%2FApache--2.0-blue?style=flat&logo=appveyor
[License]: LICENSE-MIT
[Test Badge]: https://github.com/strtok/marwood/actions/workflows/test.yml/badge.svg
[Test]: https://github.com/strtok/marwood/actions/workflows/test.yml
[Deploy Badge]: https://github.com/strtok/marwood/actions/workflows/deploy.yml/badge.svg
[Deploy]: https://github.com/strtok/marwood/actions/workflows/deploy.yml
Marwood is an embeddable [Scheme R5RS](https://schemers.org/Documents/Standards/R5RS/) implementation, featuring:

* a scheme r5rs compiler & 64 bit runtime composed of 3x64bit cells
* a terminal based repl using [rustyline](https://github.com/kkawakam/rustyline)
* a WebAssembly repl at [repl.marwood.io](https://repl.marwood.io).

# Example

The adder example creates a procedure called `make-adder`, which given an argument 
x returns a procedure that adds its own argument y to x.

This example may be executed with `cargo run --example adder`.

Here's the scheme that will execute in the VM given the rust example:
```scheme
(define make-adder 
    (lambda (x) 
        (lambda (y) (+ x y))))
(define add-1000 
    (make-adder 1000))
    
(add-1000 0)
(add-1000 1)
(add-1000 2)
(add-1000 3)
(add-1000 4)
```
And the rust code to execute the scheme above in marwood:
```rust
let mut vm = Vm::new();
 
vm.eval(&parse!("(define make-adder (lambda (x) (lambda (y) (+ x y))))")).unwrap();
vm.eval(&parse!("(define add-1000 (make-adder 1000))")).unwrap();

for it in 0..5 {
    match vm.eval(&list!["add-1000", it]) {
        Ok(result) => assert_eq!(result, cell![1000 + it]),
        Err(e) => error!("error: {}", e),
    }
}
```

# Design

## Cells

Marwood's parser outputs a data structure called a [Cell](marwood/src/cell.rs), which is capable of recursively
representing any scheme expression. This data structure is interpreted by the compiler
and converted to Marwood's runtime `VCell` type.

```rust
enum Cell {
  Boolean(bool),
  Symbol(String),
  Pair(Box<Cell>, Box<Cell>),
  ...
}
```

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

```rust
enum VCell {
  OpCode(OpCode)
  Ptr(usize),
  String(<Rc<String>>),
  ...
}
```

## Heap & Garbage Collection

Marwood's [heap](marwood/src/vm/heap.rs) is composed of a growbable vector of VCell
slots. [Garbage collection](marwood/src/vm/gc.rs) is performed via a root based
mark & sweep on Marwood's roots:

* Global Environment Slots
* Registers
* Currently running stack
* Currently running procedure bytecode and lexical environment

## Global Environment

Marwood maintains a global [environment](marwood/src/vm/environment.rs), which may
be manipulated by a top level `define` call. This environment represents a global
mapping of symbol => heap reference for the compiler, which in turn emits O(1) shallow
bindings (environment slot references) for the runtime.

## Lexical Environment

Marwood maintains a per-procedure lexical environment represented by a vector of 
shallow bindings. At compile time, Marwood computes the exact mapping of bindings inherited 
by a procedures immediate-outer-function (IOF), building an EnvironmentMap that instructs 
the runtime of what bindings to inherit from the IOF's Environment and Stack.

## Macros

Marwood's macro support is immature, with enough support for the derived procedures
from R5RS, such as: let, let*, letrec, and, or, begin, when, unless, etc.

### Built-in Procedures

There are three types of built-in procedures:

* Syntactic keywords such as `if`, `define` and `lambda`. These procedures are not
  first class and they are handled entirely by the compiler. The implementation for
  these syntactic keywords may be found in [compile.rs](marwood/src/vm/compile.rs).

* Certain primitives or library procedures such as type primitives (e.g. `null?` and
  `procedure?`), `eq?`, `car`, `cdr`, `-`, `+`, etc are implemented as rust built-ins
  and unlike syntactic keywords may be shadowed in the environment. These can
  be found in [builtin.rs](marwood/src/vm/builtin.rs).

* Derived library procedures that are implemented directly in
  [prelude.scm](marwood/prelude.scm).

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

### CALL Instruction

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

### TCALL Instruction

TCALL is the tail call optimized version of CALL. It is emitted by the compiler if
a procedure application is in the tail position. TCALL performs a tailcall by 
copying the just pushed procedure arguments to the argument slots used in the 
base of the current frame.

If the argument count is exactly the same, then TCALL will copy the just 
pushed arguments into their corresponding positions in the base of the 
current call frame.

If the argument count differs, TCALL rewrites the current stack frame with 
the just pushed arguments.

### ENTER Instruction

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

### VARARG Instruction

The VARARG instruction precedes the ENTER instruction for procedures with variable number of arguments, 
which are in the form:

```scheme
(lambda rest body)
(lambda (<required arguments> . rest) body)
(define (variable <required arguments> . rest) body)
```

VARARG rebuilds the top of the caller's frame in this order:

* the caller's %ip, %ep and argc are popped off the stack and saved
* every optional argument is popped off the stack and added to a newly formed list
* the list is pushed in the argument position that corresponds to it
* a new correct arg is pushed on the stack followed by the caller's saved
  %ip and %ep

### RET Instruction

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
| CALL %acc                    | Given the arguments for the procedure in %acc have been pushed on the stack, jump to the procedure in %acc.           |
| CLOSURE %acc                 | Create a lexical environment as a result of evaluating a (lambda ...) expression.                                     |
| ENVGET &lt;SLOT&gt;          | Store the value in environment slot SLOT in ACC                                                                       |
| ENVSET &lt;SLOT&gt;          | Set the value in environment slot SLOT to the value in ACC                                                            |
| ENTER                        | Setup the currently executing procedure's stack frame                                                                 |
| HALT                         | Halt program, returning the result contained within ACC                                                               |
| JNT &lt;OFFSET&gt;           | Set %ip to OFFSET if %acc is #f                                                                                       |
| JMP &lt;OFFSET&gt;           | Set %ip to OFFSET                                                                                                     |
| MOV &lt;SRC&gt; &lt;DEST&gt; | Move the value from SRC into DEST                                                                                     |
| PUSH                         | Push the value in ACC on to the stack                                                                                 |
| RET                          | Return from a procedure entered via CALL                                                                              |
| TCALL %acc                   | Identical to a CALL instruction, except that a tail optimizing CALL is performed.                                     |

# License
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a>.
