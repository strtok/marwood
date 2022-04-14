#[macro_use]
mod common;
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;

use marwood::error::Error::{ExpectedPairButFound, InvalidNumArgs, InvalidSyntax};

#[test]
fn car_and_cdr() {
    evals![
        "(car '(1 2 3))" => "1",
        "(cdr '(1 2 3))" => "(2 3)"
    ];
    fails!["(car 1)" => ExpectedPairButFound("1".into()),
           "(cdr 1)" => ExpectedPairButFound("1".into())
    ];
}

#[test]
fn cons() {
    evals![
        "(cons 1 2)" => "(1 . 2)",
        "(cons '(1 2) '(3 4))" => "((1 2) . (3 4))",
        "(cons 1 (cons 2 (cons 3 '())))" => "(1 2 3)"
    ];
    fails!["(cons 1)" => InvalidNumArgs("cons".into()),
           "(cons 1 2 3)" => InvalidNumArgs("cons".into())
    ];
}

#[test]
fn append() {
    evals!["(append '())" => "()",
           "(append '(1 2 3))" => "(1 2 3)",
           "(append '(1 2 3) '(4 5 6))" => "(1 2 3 4 5 6)",
           "(append '(1 2 3) '() '(4 5 6))" => "(1 2 3 4 5 6)",
           "(append '(1 2 3) '() '(4 5 6 . 7))" => "(1 2 3 4 5 6 . 7)",
           "(append '(1) '(2) '(3))" => "(1 2 3)",
           "(append '(1) '(2) 3)" => "(1 2 . 3)"
    ];
    fails!["(append '(1 2 . 3) '())" => InvalidSyntax("(1 2 . 3) is an improper list".into())]
}

#[test]
fn reverse() {
    evals!["(reverse '())" => "()",
           "(reverse '(1 2 3))" => "(3 2 1)"
    ];
    fails!["(reverse '(1 2 . 3))" => InvalidSyntax("(1 2 . 3) is an improper list".into())]
}

#[test]
fn list_tail() {
    evals!["(list-tail '() 0)" => "()",
            "(list-tail '(1 2 3) 0)" => "(1 2 3)",
            "(list-tail '(1 2 3) 1)" => "(2 3)",
            "(list-tail '(1 2 3) 2)" => "(3)",
            "(list-tail '(1 2 3) 3)" => "()",
            "(list-tail '(1 2 . 3) 2)" => "3"
    ];
    evals![
        "(define l '(1 2 3))" => "#<void>",
        "(set-cdr! (list-tail l 1) '(4))" => "#<void>",
        "l" => "(1 2 4)"
    ];
    fails![
        "(list-tail '(1 2 3) 4) " => InvalidSyntax("4 is out of range for (1 2 3)".into())
    ];
}

#[test]
fn list_ref() {
    evals!["(list-ref '(1 2 3) 0)" => "1",
            "(list-ref '(1 2 3) 1)" => "2",
            "(list-ref '(1 2 3) 2)" => "3"
    ];
    fails![
        "(list-ref '() 0)" => InvalidSyntax("0 is out of range for ()".into()),
        "(list-ref '(1 2 3) 3)" => InvalidSyntax("3 is out of range for (1 2 3)".into()),
        "(list-ref '(1 2 3) 4)" => InvalidSyntax("4 is out of range for (1 2 3)".into())
    ];
}

#[test]
fn assoc() {
    evals!["(assoc 0 '((0 foo) (1 bar) (2 baz)))" => "(0 foo)",
           "(assoc 2 '((0 foo) (1 bar) (2 baz)))" => "(2 baz)",
           "(assoc 3 '((0 foo) (1 bar) (2 baz)))" => "#f",
           "(assoc '(1 2) '((0 foo) ((1 2) bar) (2 baz)))" => "((1 2) bar)"
    ];
    evals!["(assq 0 '((0 foo) (1 bar) (2 baz)))" => "(0 foo)",
           "(assq 2 '((0 foo) (1 bar) (2 baz)))" => "(2 baz)",
           "(assq 3 '((0 foo) (1 bar) (2 baz)))" => "#f",
           "(assq '(1 2) '((0 foo) ((1 2) bar) (2 baz)))" => "#f"

    ];
    evals!["(assv 0 '((0 foo) (1 bar) (2 baz)))" => "(0 foo)",
           "(assv 2 '((0 foo) (1 bar) (2 baz)))" => "(2 baz)",
           "(assv 3 '((0 foo) (1 bar) (2 baz)))" => "#f",
           "(assv '(1 2) '((0 foo) ((1 2) bar) (2 baz)))" => "#f"
    ];
}
