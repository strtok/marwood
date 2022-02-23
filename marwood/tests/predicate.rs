#[macro_use]
mod common;
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;

#[test]
fn eqv() {
    evals![
        "(define foo '(1 2 3))" => "#<void>",
        "(define bar '(1 2 3))" => "#<void>",
        "(define baz foo)" => "#<void>",
        "(eq? foo bar)" => "#f",
        "(eq? bar baz)" => "#f",
        "(eq? foo baz)" => "#t",
        "(eq? (cdr foo) (cdr baz))" => "#t",
        "(eq? (cons foo bar) (cons foo bar))" => "#t"
    ];

    evals![
        "(eq? 0 0)" => "#t",
        "(eq? #\\a #\\a)" => "#t",
        "(eq? '() '())" => "#t",
        "(eq? #f #f)" => "#t",
        "(eq? #t #t)" => "#t",
        "(eq? 'foo 'foo)" => "#t",
        "(eq? '(1 2 3) '(1 2 3))" => "#f",
        "(eq? #(1 2 3) #(1 2 3))" => "#f"
    ];
}

#[test]
fn equal() {
    evals![
        "(define foo '(1 2 3))" => "#<void>",
        "(define bar '(1 2 3))" => "#<void>",
        "(define baz foo)" => "#<void>",
        "(equal? foo bar)" => "#t",
        "(equal? bar baz)" => "#t",
        "(equal? foo baz)" => "#t",
        "(equal? (cdr foo) (cdr baz))" => "#t",
        "(equal? (cons foo bar) (cons foo bar))" => "#t",
        "(equal? 0 0)" => "#t",
        "(equal? '() '())" => "#t",
        "(equal? #f #f)" => "#t",
        "(equal? #t #t)" => "#t",
        "(equal? 'foo 'foo)" => "#t",
        "(equal? #(1 2 3) #(1 2 3))" => "#t",
        "(equal? \"foo\" \"foo\")" => "#t",
        "(equal? #\\a #\\a)" => "#t"
    ];
}

#[test]
fn not() {
    evals![
      "(not #t)" => "#f",
      "(not #f)" => "#t",
      "(not 'apples)" => "#f"
    ];
}

#[test]
fn unary_predicate() {
    evals![
        "(number? 10)" => "#t",
        "(number? '10)" => "#t",
        "(number? 'apples)" => "#f",
        "(integer? 10)" => "#t",
        "(integer? 10/5)" => "#t",
        "(integer? 10/3)" => "#f",
        "(integer? 10.3)" => "#f",

        "(integer? 10)" => "#t",
        "(integer? 10/5)" => "#t",
        "(integer? 10/3)" => "#f",
        "(integer? 10.3)" => "#f",

        "(real? 10)" => "#t",
        "(real? 10/5)" => "#t",
        "(real? 10/3)" => "#t",
        "(real? 10.3)" => "#t",

        "(complex? 10)" => "#t",
        "(complex? 10/5)" => "#t",
        "(complex? 10/3)" => "#t",
        "(complex? 10.3)" => "#t",

        "(rational? 10)" => "#t",
        "(rational? 10/5)" => "#t",
        "(rational? 10/3)" => "#t",
        "(rational? 10.3)" => "#f"
    ];

    evals![
        "(boolean? #t)" => "#t",
        "(boolean? #f)" => "#t",
        "(boolean? '#t)" => "#t",
        "(boolean? '#f)" => "#t",
        "(boolean? 10)" => "#f"
    ];

    evals![
        "(symbol? 'apples)" => "#t",
        "(symbol? 10)" => "#f"
    ];

    evals![
        "(string? \"foo\")" => "#t",
        "(symbol? 10)" => "#f"
    ];

    evals![
        "(char? #\\a)" => "#t",
        "(char? 10)" => "#f"
    ];

    evals![
        "(symbol? 'apples)" => "#t",
        "(symbol? 10)" => "#f"
    ];
    evals![
        "(null? '())" => "#t",
        "(null? (cdr '(apples)))" => "#t",
        "(null? #f)" => "#f"
    ];

    evals![
        "(procedure? (lambda (x) x))" => "#t",
        "(define identity (lambda (x) x))" => "#<void>",
        "(procedure? identity)" => "#t"
    ];

    evals![
        "(pair? '(1 2 3))" => "#t",
        "(pair? '(1. 2))" => "#t",
        "(pair? (cons 1 2))" => "#t",
        "(pair? 'apples)" => "#f"
    ];

    evals![
        "(list? '())" => "#t",
        "(list? '(1 2))" => "#t",
        "(list? '(1 3 4))" => "#t",
        "(list? '(1 2 . 3))" => "#f"
    ];

    evals![
        "(vector? #(1 2 3))" => "#t"
    ];
}
