#[macro_use]
mod common;
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;

#[test]
fn continuations_are_procedures() {
    evals![
      "(call/cc procedure?)" => "#t"
    ];
}

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

#[test]
fn hello_world() {
    evals![
        "(let ([x (call/cc (λ (k) k))])
         (x (λ (ignore) \"hello world\")))"

        => "\"hello world\""
    ];
    evals![
        "(((call/cc (λ (k) k))
            (λ (x) x)) \"hello world\")"

        => "\"hello world\""
    ];
}

#[test]
fn factorial_cps() {
    evals!["(define (factorial n)
            (define (factorial n k)
                (cond
                   [(zero? n) (k 1)]
                   [else 
                      (factorial (- n 1) 
                                 (λ (v) (k (* v n))))]))
            (factorial n (λ (v) v)))"
            => "#<void>",
        
            "(factorial 10)" 
            => "3628800"];
}
