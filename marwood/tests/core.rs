#[macro_use]
mod common;

use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Error::{
    InvalidDefineSyntax, InvalidProcedure, InvalidSyntax, InvalidUsePrimitive, UnquotedNil,
    VariableNotBound,
};
use marwood::vm::Vm;

#[test]
fn comments() {
    evals![
       "1 ;number one" => "1",
       "(+ 10 ;adding 10\n 5;to the number 5\n)" => "15"
    ];
}

#[test]
fn eval_literal() {
    evals![
       "1" => "1",
       "-10" => "-10",
       "#t" => "#t",
       "#f" => "#f",
       "'()" => "()"
    ];

    fails!["foo" => VariableNotBound("foo".into()),
               "()" => UnquotedNil];
}

#[test]
fn eval_quote() {
    evals![
        "'1" => "1",
        "'#t" => "#t",
        "'#f" => "#f",
        "'()" => "()",
        "'(1 2 3)" => "(1 2 3)"
    ];
}

#[test]
fn procedure_display() {
    prints![
        "+" => "#<procedure:+>",
        "(lambda (x y) (+ x y))" => "#<procedure:(λ (x y))>"
    ];
}

#[test]
fn if_expressions() {
    evals![
        "(if (list? '(1 2 3)) 'yep 'nope)" => "yep",
        "(if (list? #t) 'yep 'nope)" => "nope",
        "(if (list? '(1 2 3)) 'yep)" => "yep",
        "(if (list? #t) 'yep)" => "#<void>"
    ];
}

#[test]
fn gc_cleans_intern_map() {
    evals![
        "'foo" => "foo",
        "'foo" => "foo"
    ]
}

#[test]
fn invalid_procedure_calls() {
    fails![
        "(1 2 3)" => InvalidProcedure("1".to_string()),
        "((+ 1 1) 2 3)" => InvalidProcedure("2".to_string())
    ];
}

#[test]
fn lambdas() {
    evals![
        "((lambda () 10))" => "10",
        "(+ ((lambda () 50)) ((lambda () 100)))" => "150"
    ];
}

#[test]
fn tge_capturing_lambda() {
    evals![
        "(define x 100)" => "#<void>",
        "(define y 50)" => "#<void>",
        "((lambda () x))" => "100",
        "(+ ((lambda () x)) ((lambda () y)))" => "150",
        "(define x 1000)" => "#<void>",
        "(define y 500)" => "#<void>",
        "(+ ((lambda () x)) ((lambda () y)))" => "1500"
    ];
}

#[test]
fn lambda_with_args() {
    // Identity
    evals![
        "(define identity (lambda (x) x))" => "#<void>",
        "(identity '(1 2 3))" => "(1 2 3)"
    ];

    // Two arg
    evals![
        "(define make-pair (lambda (x y) (cons x y)))" => "#<void>",
        "(make-pair 'apples 'bananas)" => "(apples . bananas)"
    ];

    // Mixed arg and global
    evals![
        "(define x 100)" => "#<void>",
        "(define add-to-x (lambda (y) (+ x y)))" => "#<void>",
        "(add-to-x 50)" => "150"
    ];
}

#[test]
fn lambda_operator_is_expression() {
    evals![
        "(define proc (lambda () add))" => "#<void>",
        "(define add (lambda (x y) (+ x y)))" => "#<void>",
        "((proc) 1 2)" => "3"
    ];
}

#[test]
fn iof_argument_capture() {
    evals![
        "(define make-adder (lambda (x) (lambda (y) (+ x y))))" => "#<void>",
        "(define add-10 (make-adder 10))" => "#<void>",
        "(add-10 20)" => "30"
    ];
}

#[test]
fn iof_environment_capture() {
    evals![
        "(define make-make-adder (lambda (x) (lambda (y) (lambda (z) (+ x y z)))))" => "#<void>",
        "(define make-adder (make-make-adder 1000))" => "#<void>",
        "(define add-1000-100 (make-adder 100))" => "#<void>",
        "(add-1000-100 10)" => "1110"
    ];

    evals![
        "(define (make-make-adder x) (lambda (y) (lambda (z) (+ x y z))))" => "#<void>",
        "(define make-adder (make-make-adder 1000))" => "#<void>",
        "(define add-1000-100 (make-adder 100))" => "#<void>",
        "(add-1000-100 10)" => "1110"
    ];

    evals![
        "(define f 0)" => "#<void>",
        "(define (make-make-adder x) (lambda (y) (set! f (lambda (x) x)) (lambda (z) (+ x y z))))" => "#<void>",
        "(define make-adder (make-make-adder 1000))" => "#<void>",
        "(define add-1000-100 (make-adder 100))" => "#<void>",
        "(add-1000-100 10)" => "1110"
    ];
}

#[test]
fn iof_environment_capture_with_first_class_procedure() {
    evals![
        "(define make-adder-adder (lambda (adder num) (lambda (n) (+ ((adder num) n)))))" => "#<void>",
        "((make-adder-adder (lambda (i) (lambda (j) (+ i j))) 100) 1000)" => "1100"
    ];
}

#[test]
fn define_special_forms() {
    evals![
        "(define (make-adder x) (lambda (y) (+ x y)))" => "#<void>",
        "(define add-10 (make-adder 10))" => "#<void>",
        "(add-10 20)" => "30"
    ];
}

#[test]
fn vararg() {
    evals![
        "((lambda l l))" => "()",
        "((lambda l l) 10)" => "(10)",
        "((lambda l l) 10 20)" => "(10 20)"
    ];

    evals![
        "(define (list . a) a)" => "#<void>",
        "(list)" => "()",
        "(list 10)" => "(10)",
        "(list 10 20)" => "(10 20)"
    ];

    evals![
        "((lambda (x . y) (cons x y)) 10)" => "(10 . ())",
        "((lambda (x . y) (cons x y)) 10 20)" => "(10 . (20))",
        "((lambda (x y . z) (cons (+ x y) z)) 10 20)" => "(30 . ())",
        "((lambda (x y . z) (cons (+ x y) z)) 10 20 30)" => "(30 . (30))",
        "((lambda (x y . z) (cons (+ x y) z)) 10 20 30 40)" => "(30 . (30 40))"
    ];
}

#[test]
fn tail_recursive() {
    evals![r#"(define (nth l n)
                    (if (null? l) '()
                        (if (eq? n 0) (car l)
                            (nth (cdr l) (- n 1)))))"# => "#<void>",
           "(nth '(1 2 3 4 5) 4)" => "5",
           "(nth '(1 2 3 4 5) 5)" => "()"
    ];
}

#[test]
fn disallow_aliasing_syntactic_symbol() {
    fails!["(define if 42)" => InvalidUsePrimitive("if".into())];
    fails!["(define my-if if)" => InvalidUsePrimitive("if".into())];
    fails!["(lambda (if) 42)" => InvalidUsePrimitive("if".into())];
}

#[test]
fn or() {
    evals!["(or 5)" => "5"];
    evals!["(or #f 5)" => "5"];
    evals!["(or (eq? 1 2) 'apples)" => "apples"];
}

#[test]
fn and() {
    evals!["(and 5)" => "5"];
    evals!["(and #t 5)" => "5"];
    evals!["(and #f 5)" => "#f"];
    evals!["(and #t #t 5)" => "5"];
}

#[test]
fn begin() {
    evals!["(begin (+ 10 10) (+ 20 20) (+ 5 5))" => "10"];
}

#[test]
fn unless() {
    evals!["(unless #t 10)" => "#<void>"];
    evals!["(unless #f 10)" => "10"];
}

#[test]
fn let_lambda() {
    evals!["(let () (+ 10 20))" => "30"];
    evals!["(let ([x 10] [y 20]) (+ x y))" => "30"];
}

#[test]
fn let_star() {
    evals!["(let* ([x 10] [y (* x x)]) (+ x y))" => "110"]
}

#[test]
fn set() {
    evals!["(define (generator) (let ([x 0]) (lambda () (set! x (+ x 1)) x)))" => "#<void>",
           "(define counter (generator))" => "#<void>",
           "(counter)" => "1",
           "(counter)" => "2",
           "(counter)" => "3"
    ];
}

#[test]
fn set_pair() {
    evals!["(define l '(1 2 3))" => "#<void>",
           "(set-car! (cdr l) 100))" => "#<void>",
           "l" => "(1 100 3)"
    ];
    evals!["(define l '(1 2 3))" => "#<void>",
           "(set-cdr! (cdr (cdr l)) '(4 5 6)))" => "#<void>",
           "l" => "(1 2 3 4 5 6)"
    ];
}

#[test]
fn internal_define() {
    evals!["((lambda (x) (define y 10) (+ x y)) 20)" => "30"];
    fails!["(lambda (x) (define y 10) (+ x y) (define z 10))" => 
            InvalidDefineSyntax("out of context: (define z 10)".into())];
}

#[test]
fn internal_define_is_lexical() {
    evals![
        "(define y 100)" => "#<void>",
        "((lambda (x) (define y 10) (+ x y)) 20)" => "30",
        "y" => "100"
    ];
    evals![
        "(define (y) 100)" => "#<void>",
        "((lambda (x) (define (y) 10) (+ x (y))) 20)" => "30",
        "(y)" => "100"
    ];
}

#[test]
fn find_primes() {
    evals![
        r#"
            (define (find-primes n)
                (define (make-sieve n)
                    (define (init-sieve v n)
                        (cond
                            ((zero? n) v)
                            (else (vector-set! v (- n 1) (- n 1)) (init-sieve v (- n 1)))))
                    (init-sieve (make-vector n) n))
                (define (mark-multiples-of v m i)
                    (cond
                        ((>= (* m i) (vector-length v)) v)
                        (else (vector-set! v (* m i) #f) (mark-multiples-of v m (+ i 1)))))
                (define (sieve v i)
                    (cond
                        ((>= i (vector-length v)) v)
                        ((eq? (vector-ref v i) #f) (sieve v (+ i 1)))
                        (else (sieve (mark-multiples-of v i i) (+ i 1)))))
                (define (sieve->list v)
                    (define (sieve->list v i)
                        (cond
                            ((= i (vector-length v)) '())
                            ((eq? (vector-ref v i) #f) (sieve->list v (+ i 1)))
                            (else (cons i (sieve->list v (+ i 1))))))
                    (sieve->list v 0))
                (sieve->list (sieve (make-sieve n) 2)))
            "# => "#<void>",
        "(find-primes 100)" => "(0 1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)"
    ];
}

#[test]
fn copy_closure() {
    evals![r#"
          (define (copy a b)
             ((lambda (partial) (cons a partial))
             (if (= b 0)
                'eol
                (copy (+ a 1) (- b 1)))))"# => "#<void>",
       "(copy 100 10)" => "(100 101 102 103 104 105 106 107 108 109 110 . eol)"
    ];
}

#[test]
fn apply_makes_environment() {
    evals![
        "(define x 0)" => "#<void>",
        "(let ~ ((i (* 9)))
              (if (< 1 i) (~ (- i 1)))
              (set! x (+ x i)))" => "#<void>",
        "x" => "45"
    ];
}

#[test]
fn eval() {
    evals![
        "(eval 10)" => "10",
        "(define x 42)" => "#<void>",
        "(eval 'x)" => "42"
    ];

    evals![
        "(eval '((lambda (x y) (+ x y)) 10 20))" => "30"
    ];

    fails![
        "(eval +)" => InvalidSyntax("#<procedure:+>".into())
    ];
}

#[test]
fn apply() {
    evals![
        "(apply + '())" => "0",
        "(apply + 10 '())" => "10",
        "(apply + '(10))" => "10",
        "(apply + 10 '(20))" => "30",
        "(apply + 10 20 '(30 40))" => "100"
    ];

    fails![
        "(apply + 10 20)" => InvalidSyntax("the last argument to apply must be a proper list".into()),
        "(apply + 10 '(10 . 20))" => InvalidSyntax("the last argument to apply must be a proper list".into())
    ];
}

#[test]
fn case() {
    evals!["(case (+ 3 5)
                [(1 2 3 4 5) 'small]
                [(6 7 8 9 10) 'big])" => "big"];
}
