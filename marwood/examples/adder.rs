use log::error;
use marwood::cell::Cell;
use marwood::vm::Vm;
use marwood::{cell, lex, list, parse};

extern crate marwood;

fn main() {
    let mut vm = Vm::new();

    vm.eval(&parse!(
        "(define make-adder (lambda (x) (lambda (y) (+ x y))))"
    ))
    .unwrap();

    vm.eval(&parse!("(define add-1000 (make-adder 1000))"))
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
}
