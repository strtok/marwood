#[macro_use]
mod common;

use marwood::cell;
use marwood::cell::Cell;
use marwood::error::Error::{
    InvalidProcedure, InvalidSyntax, InvalidUsePrimitive, UnquotedNil, VariableNotBound,
};
use marwood::lex;
use marwood::parse;
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
fn quasiquote() {
    evals![
        "(define x 42)" => "#<void>",
        "`,1001" => "1001",
        "`(10 20 ,(+ 10 10 10))" => "(10 20 30)",
        "`#(10 20 ,(+ 10 10 10))" => "#(10 20 30)",
        "``(x ,x ,,x)" => "(quasiquote (x (unquote x) (unquote 42)))",
        "``#(x ,x ,,x)" => "(quasiquote #(x (unquote x) (unquote 42)))"
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
        "(1 2 3)" => InvalidProcedure(cell![1]),
        "((+ 1 1) 2 3)" => InvalidProcedure(cell![2])
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
fn letrec_and_letrec_star() {
    evals!["(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))" => "120"];
    evals!["(letrec* ((a 10) (b (+ a 1))) (+ a b))" => "21"];
    evals![r#"(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                       (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
                (even? 10))"# => "#t"];
}

/// Hygiene: the `or` prelude macro introduces a binding named `var1`
/// in its template. If the user has their own `var1` in scope, a
/// non-hygienic expander will shadow it.
#[test]
fn or_hygiene_does_not_capture_var1() {
    evals![
        "(define var1 42)" => "#<void>",
        "(or #f var1)" => "42"
    ];
}

/// Hygiene: `cond` with `=>` introduces a `temp` binding around the
/// user-supplied result expression. A result expression that closes
/// over a user-level `temp` must still resolve to the user's binding.
#[test]
fn cond_arrow_hygiene_does_not_capture_temp() {
    evals![
        "(define temp 10)" => "#<void>",
        "(cond (100 => (lambda (x) (+ x temp))))" => "110"
    ];
}

/// Hygiene: `case` introduces an `atom-key` binding around the
/// user-supplied clause bodies. User references to their own
/// `atom-key` must not resolve to the macro-introduced binding.
#[test]
fn case_hygiene_does_not_capture_atom_key() {
    evals![
        "(define atom-key 'a)" => "#<void>",
        "(case (+ 0 1) ((1 2 3) atom-key))" => "a"
    ];
}

/// Symbols inside a `(quote ...)` form in a macro template are
/// data, not identifier references, so they must intern to the same
/// heap slot as a user-written literal of the same name. `expand`
/// switches to a non-stamping mode under quote.
#[test]
fn quoted_symbol_in_template_is_eq_to_user_symbol() {
    evals![
        "(define-syntax give-foo (syntax-rules () ((_) 'foo)))" => "#<void>",
        "(eq? (give-foo) 'foo)" => "#t"
    ];
}

/// Free identifiers in a macro template refer to bindings visible
/// at `define-syntax` time (the macro's definition environment),
/// not at use time. The compiler captures resolvable globals into
/// the macro's definition scope at definition time, so a later user
/// redefinition of `+` does not re-target the template's `+`.
#[test]
fn template_free_identifier_refers_to_definition_env() {
    evals![
        "(define-syntax my-add (syntax-rules () ((_ a b) (+ a b))))" => "#<void>",
        "(define + (lambda (a b) (- a b)))" => "#<void>",
        "(my-add 5 3)" => "8"
    ];
}

/// End-to-end smoke test for the macros defined in `prelude.scm`.
/// Each form here is implemented via `define-syntax` rather than as
/// a primitive, so the prelude depends on the macro expander matching,
/// nested ellipsis, and hygiene all working together.
#[test]
fn prelude_macros_smoke() {
    evals![
        "(let ((x 1) (y 2)) (+ x y))" => "3",
        "(let* ((x 1) (y (+ x 1))) (+ x y))" => "3",
        "(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))" => "120",
        "(letrec* ((a 10) (b (+ a 1))) (+ a b))" => "21",
        "(and)" => "#t",
        "(and 1 2 3)" => "3",
        "(and 1 #f 3)" => "#f",
        "(or)" => "#f",
        "(or #f #f 7)" => "7",
        "(or #f 3 (error 'unreachable))" => "3",
        "(when #t 1 2 3)" => "3",
        "(unless #f 1 2 3)" => "3",
        "(begin 1 2 3)" => "3",
        "(cond ((= 1 2) 'no) ((= 1 1) 'yes) (else 'else))" => "yes",
        "(cond (#f 'no) (else 'else))" => "else",
        "(cond ((+ 1 2) => (lambda (x) (* x 10))))" => "30",
        "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))" => "composite",
        "(case 'unknown ((a b) 'ab) ((c d) 'cd) (else 'other))" => "other"
    ];
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
            InvalidSyntax("out of context define: (define z 10)".into())];
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

#[test]
fn void_is_value() {
    evals!["(define void (define void 0))" => "#<void>",
           "void" => "#<void>"
    ];
}

#[test]
fn map() {
    evals!["(map + '(1 2 3))" => "(1 2 3)"];
    evals!["(map + '(1 2 3) '(4 5 6))" => "(5 7 9)"];
}

#[test]
fn for_each() {
    evals!["(map + '(1 2 3))" => "(1 2 3)"];
}
