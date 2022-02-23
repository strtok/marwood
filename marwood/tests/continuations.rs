#[macro_use]
mod common;
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;

#[test]
fn continuations() {
    evals![
        "(call/cc (lambda (cc) (cc 10)))" => "10"
    ];

    evals![
        "(define animal (call/cc (lambda (cc) cc)))" => "#<void>",
        "(animal 'elephant)" => "#<void>",
        "animal" => "elephant"
    ];

    evals![
        "(call-with-current-continuation (lambda (cc) (+ 2 5)))" => "7",
        "(call-with-current-continuation (lambda (cc) (+ 2 5 (cc 3))))" => "3"
    ];

    evals![
        "(define add100 0)" => "#<void>",
        "(+ 100 (call/cc (lambda (cc) (set! add100 cc) 0)))" => "100",
        "(add100 16)" => "116",
        "(* 2 (add100 16))" => "116"
    ];
}
