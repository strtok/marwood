use crate::cell::Cell;
use crate::vm::environment::GlobalEnvironment;
use crate::vm::heap::Heap;
use crate::vm::stack::Stack;
use crate::vm::vcell::VCell;
use log::trace;

pub mod compile;
pub mod environment;
pub mod gc;
pub mod heap;
pub mod lambda;
pub mod opcode;
pub mod run;
pub mod stack;
pub mod vcell;

const HEAP_SIZE: usize = 1024;

#[derive(Debug)]
pub struct Vm {
    /// The heap and global environment
    heap: Heap,
    globenv: GlobalEnvironment,

    /// The current program stack
    stack: Stack,

    /// Registers
    acc: VCell,
    ep: usize,
    ip: (usize, usize),
    bp: usize,
}

impl Vm {
    /// New
    ///
    /// Return a new Vm
    pub fn new() -> Vm {
        Vm {
            heap: Heap::new(HEAP_SIZE),
            ip: (usize::MAX, 0),
            stack: Stack::new(),
            globenv: GlobalEnvironment::new(),
            ep: usize::MAX,
            acc: VCell::undefined(),
            bp: 0,
        }
    }

    /// Eval
    ///
    /// Compile the expression contained within cell, eval, and return
    /// the result.
    ///
    /// # Arguments
    /// `cell` - An expression to evaluate
    pub fn eval(&mut self, cell: &Cell) -> Result<Cell, Error> {
        let lambda = self.compile(cell)?;
        trace!("emit: \n{}", self.decompile_text(&lambda));
        let lambda = self.heap.put(lambda);
        self.ip.0 = lambda.as_ptr().unwrap();
        self.ip.1 = 0;
        self.run()
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error("expected {0}")]
    ExpectedType(&'static str, &'static str),

    #[error("expected pair, but found {0}")]
    ExpectedPairButFound(String),

    #[error("expected stack value")]
    ExpectedStackValue,

    #[error("invalid argument for {0}: expected {1}, but got {2}")]
    InvalidArgs(String, String, String),

    #[error("invalid number of arguments for {0}")]
    InvalidNumArgs(String),

    #[error("invalid bytecode")]
    InvalidBytecode,

    #[error("call of non-procedure: {0}")]
    InvalidProcedure(String),

    #[error("invalid stack index: {0}")]
    InvalidStackIndex(usize),

    #[error("lambda require at least one expression")]
    LambdaMissingExpression,

    #[error("unknown procedure {0}")]
    UnknownProcedure(String),

    #[error("variable {0} not bound")]
    VariableNotBound(String),

    #[error("invalid syntax: () must be quoted")]
    UnquotedNil,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex;
    use crate::parse;
    use crate::vm::Error::{
        ExpectedPairButFound, InvalidArgs, InvalidNumArgs, InvalidProcedure, UnquotedNil,
        VariableNotBound,
    };

    macro_rules! evals {
        ($($lhs:expr => $rhs:expr),+) => {{
            let mut vm = Vm::new();
             $(
                assert_eq!(vm.eval(&parse!($lhs)), Ok(match $rhs {
                    "#<void>" => Cell::Void,
                    _ => parse!($rhs)
                }));
             )+
        }};
    }

    macro_rules! fails {
        ($($lhs:expr => $rhs:expr),+) => {{
            let mut vm = Vm::new();
             $(
                assert_eq!(vm.eval(
                    &parse!($lhs)
                ), Err($rhs));
             )+
        }};
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
        fails!["(cons 1)" => ExpectedPairButFound("()".into()),
               "(cons 1 2 3)" => InvalidNumArgs("cons".into())
        ];
    }

    #[test]
    fn arithmetic() {
        evals![
            "(+)" => "0",
            "(+ 10)" => "10",
            "(+ 10 20)" => "30",
            "(+ 10 20 30)" => "60",
            "(+ (+ 100 100) (+ 25 25) 50)" => "300",
            "(- 10)" => "-10",
            "(- 100 10)" => "90",
            "(- 1000 100 10)" => "890",
            "(*)" => "1",
            "(* 10)" => "10",
            "(* 10 10)" => "100",
            "(* 10 10 10)" => "1000"
        ];
        fails!["(+ '(10))" => InvalidArgs("+".into(), "number".into(), "(10)".into()),
               "(+ 10 '(10))" => InvalidArgs("+".into(), "number".into(), "(10)".into()),
               "(-)" => InvalidNumArgs("-".into())
        ];
    }

    #[test]
    fn eq() {
        evals![
            "(define foo '(1 2 3))" => "#<void>",
            "(define bar '(1 2 3))" => "#<void>",
            "(define baz foo)" => "#<void>",
            "(eq? foo bar)" => "#f",
            "(eq? bar baz)" => "#f",
            "(eq? foo baz)" => "#t",
            "(eq? (cdr foo) (cdr baz))" => "#t"
        ];

        evals![
            "(eq? 0 0)" => "#f",
            "(eq? '() '())" => "#f",
            "(eq? #f #f)" => "#f",
            "(eq? #t #t)" => "#f",
            "(eq? 'foo 'foo)" => "#t"
        ];
    }

    #[test]
    fn unary_predicate() {
        evals![
            "(number? 10)" => "#t",
            "(number? '10)" => "#t",
            "(number? 'apples)" => "#f"
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
        ]
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
    }

    #[test]
    fn iof_environment_capture_with_first_class_procedure() {
        evals![
            "(define make-adder-adder (lambda (adder num) (lambda (n) (+ ((adder num) n)))))" => "#<void>",
            "((make-adder-adder (lambda (i) (lambda (j) (+ i j))) 100) 1000)" => "1100"
        ];
    }
}
