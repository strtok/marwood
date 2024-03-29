# _λ_ Marwood &emsp; [![License Badge]][License] [![Test Badge]][Test] [![Deploy Badge]][Deploy]

[License Badge]: https://img.shields.io/badge/license-MIT%2FApache--2.0-blue?style=flat&logo=appveyor
[License]: LICENSE-MIT
[Test Badge]: https://github.com/strtok/marwood/actions/workflows/test.yml/badge.svg
[Test]: https://github.com/strtok/marwood/actions/workflows/test.yml
[Deploy Badge]: https://github.com/strtok/marwood/actions/workflows/deploy.yml/badge.svg
[Deploy]: https://github.com/strtok/marwood/actions/workflows/deploy.yml
Marwood is an embeddable [Scheme R7RS](https://small.r7rs.org) implementation, featuring:

* a scheme R7RS compiler & 64 bit runtime
* a terminal based repl using [rustyline](https://github.com/kkawakam/rustyline)
* a WebAssembly repl at [repl.marwood.io](https://repl.marwood.io).

And these scheme language features:

* most R7RS language and library features
* Tail call optimization
* First class continuations (call/cc)
* Support for scheme's numerical tower, including rationals
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

vm.eval_text("(define make-adder (lambda (x) (lambda (y) (+ x y))))")
    .unwrap();
vm.eval_text("(define add-1000 (make-adder 1000))")
    .unwrap();

for it in 0..5 {
    match vm.eval(&list!["add-1000", it]) {
        Ok(result) => {
            println!("{} => {}", it, result);
            assert_eq!(result, cell![1000 + it])
        }
        Err(e) => error!("error: {}", e),
    }
}
```
# License
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a>.
