use crate::cell::Cell;
use crate::vm::heap::Heap;
use crate::vm::node::Node;
use log::trace;

pub mod compile;
pub mod heap;
pub mod node;
pub mod opcode;
pub mod run;

const HEAP_SIZE: usize = 1024;

pub struct Vm {
    heap: Heap,
    bc: Vec<Node>,
    stack: Vec<Node>,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex;
    use crate::parse;

    macro_rules! evals {
        ($($lhs:expr => $rhs:expr),+) => {{
            let mut vm = Vm::new();
             $(
                assert_eq!(vm.eval(&parse!($lhs)), Ok(parse!($rhs)));
             )+
        }};
    }

    macro_rules! fails {
        ($($lhs:expr),+) => {{
            let mut vm = Vm::new();
             $(
                assert!(matches!(vm.eval(
                    &parse!($lhs)
                ), Err(_)));
             )+
        }};
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
        fails!["(car 1)", "(cdr 1)"];
    }

    #[test]
    fn cons() {
        evals![
            "(cons 1 2)" => "(1 . 2)",
            "(cons '(1 2) '(3 4))" => "((1 2) . (3 4))",
            "(cons 1 (cons 2 (cons 3 '())))" => "(1 2 3)"
        ];
        fails!["(cons 1)", "(cons 1 2 3)"];
    }

    #[test]
    fn arithmetic() {
        evals![
            "(+)" => "0",
            "(+ 10)" => "10",
            "(+ 10 20)" => "30",
            "(+ 10 20 30)" => "60",
            "(+ (+ 100 100) (+ 25 25) 50)" => "300"
        ];
        fails!["(+ '(10))", "(+ 10 '(10))"];
    }
}
