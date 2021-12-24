use crate::cell::Cell;
use crate::vm::heap::Heap;
use crate::vm::node::Node;
use crate::vm::run::Environment;
use log::trace;

pub mod compile;
pub mod gc;
pub mod heap;
pub mod node;
pub mod opcode;
pub mod run;

const HEAP_SIZE: usize = 1024;

#[derive(Debug)]
pub struct Vm {
    heap: Heap,
    bc: Vec<Node>,
    stack: Vec<Node>,
    globenv: Environment,
    acc: Node,
    ip: usize,
}

impl Vm {
    /// New
    ///
    /// Return a new Vm
    pub fn new() -> Vm {
        Vm {
            heap: Heap::new(HEAP_SIZE),
            bc: vec![],
            stack: vec![],
            globenv: Environment::new(),
            acc: Node::undefined(),
            ip: 0,
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
        self.bc = self.compile(cell)?;
        trace!("emit: \n{}", self.decompile_text(&self.bc));
        self.ip = 0;
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
    #[error("expected pair, but found {0}")]
    ExpectedPair(String),

    #[error("invalid argument for {0}: expected {1}, but got {2}")]
    InvalidArgs(String, String, String),

    #[error("invalid number of arguments for {0}")]
    InvalidNumArgs(String),

    #[error("invalid bytecode")]
    InvalidBytecode,

    #[error("expected stack value")]
    ExpectedStackValue,

    #[error("unknown procedure {0}")]
    UnknownProcedure(String),

    #[error("variable {0} not bound")]
    VariableNotBound(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex;
    use crate::parse;
    use crate::vm::Error::{ExpectedPair, InvalidArgs, InvalidNumArgs, VariableNotBound};

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
           "'()" => "()",
           "()" => "()"
        ];

        fails!["foo" => VariableNotBound("foo".into())];
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
        fails!["(car 1)" => ExpectedPair("1".into()),
               "(cdr 1)" => ExpectedPair("1".into())
        ];
    }

    #[test]
    fn cons() {
        evals![
            "(cons 1 2)" => "(1 . 2)",
            "(cons '(1 2) '(3 4))" => "((1 2) . (3 4))",
            "(cons 1 (cons 2 (cons 3 '())))" => "(1 2 3)"
        ];
        fails!["(cons 1)" => ExpectedPair("()".into()),
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
    fn gc_cleans_intern_map() {
        evals![
            "'foo" => "foo",
            "'foo" => "foo"
        ]
    }
}
