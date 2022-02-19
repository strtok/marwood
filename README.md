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

And these scheme language features:

* most R5RS language and library features
* R7Small unicode support
* Tail call optimization
* First class continuations (call/cc)
* Support for scheme's numerical tower, including rationals
  * No complex number support
* Partial support for syntax-rules

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
# License
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a>.
