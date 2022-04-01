#[macro_use]
mod common;
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;

use marwood::vm::Error::{ExpectedPairButFound, InvalidSyntax, InvalidVectorIndex};

#[test]
fn vector_and_make_vector() {
    evals![
        "(vector)" => "#()",
        "(vector (+ 10 10) #t)" => "#(20 #t)"
    ];
    evals![
        "(make-vector 0)" => "#()",
        "(make-vector 3)" => "#(0 0 0)",
        "(make-vector 3 '(1 2))" => "#((1 2) (1 2) (1 2))"
    ];
}

#[test]
fn vector_length() {
    evals!["(vector-length #())" => "0",
           "(vector-length #(1 2 3))" => "3"
    ];
    fails!["(vector-length '(1 2 3)))" => 
            InvalidSyntax("(1 2 3) is not a vector".into())];
}

#[test]
fn vector_ref() {
    evals!["(vector-ref #(1 2 3) 0)" => "1",
           "(vector-ref #(1 2 3) 1)" => "2",
           "(vector-ref #(1 2 3) 2)" => "3"
    ];
    fails!["(vector-ref '(1 2 3) 0))" => 
            InvalidSyntax("(1 2 3) is not a vector".into())];
    fails!["(vector-ref #(1 2 3) 10))" => 
            InvalidVectorIndex(10, 3)];
}

#[test]
fn vector_set() {
    evals!["(define v #(1 2 3))" => "#<void>",
           "(vector-set! v 0 42)" => "#<void>",
           "v" => "#(42 2 3)"
    ];
    evals!["(define v #(1 2 3))" => "#<void>",
           "(vector-fill! v 42)" => "#<void>",
           "v" => "#(42 42 42)"
    ];
    fails!["(vector-set! '(1 2 3) 0 0))" =>
            InvalidSyntax("(1 2 3) is not a vector".into())];
    fails!["(vector-fill! '(1 2 3) 0))" =>
            InvalidSyntax("(1 2 3) is not a vector".into())];
    fails!["(vector-set! #(1 2 3) 10 0))" =>
            InvalidVectorIndex(10, 3)];
}

#[test]
fn list_vector_conversions() {
    evals!["(vector->list #(1 2 3))" => "(1 2 3)",
           "(vector->list #())" => "()"
    ];
    evals!["(list->vector '(1 2 3))" => "#(1 2 3)",
           "(list->vector '())" => "#()"
    ];
    fails!["(vector->list '(1 2 3))" =>
            InvalidSyntax("(1 2 3) is not a vector".into())];
    fails!["(list->vector #(1 2 3))" =>
            ExpectedPairButFound("#(1 2 3)".into())];
}

#[test]
fn vector_copy() {
    evals![
        "(vector-copy #(1 2 3))" => "#(1 2 3)",
        "(vector-copy #(1 2 3) 1)" => "#(2 3)",
        "(vector-copy #(1 2 3) 1 1)" => "#(2)"
    ];
}

#[test]
fn vector_copy_mut() {
    evals![
        "(define a (vector 1 2 3 4 5))" => "#<void>",
        "(define b (vector 10 20 30 40 50))" => "#<void>",
        "(vector-copy! b 1 a 0 2)" => "#<void>",
        "b" => "#(10 1 2 40 50)"
    ];
}
