use crate::cell::Cell;
use crate::lex;
use crate::parse::parse;
use crate::vm::environment::GlobalEnvironment;
use crate::vm::heap::Heap;
use crate::vm::stack::Stack;
use crate::vm::vcell::VCell;
use log::trace;
use std::fmt::{Debug, Formatter};

pub mod builtin;
pub mod compare;
pub mod compile;
pub mod environment;
pub mod gc;
pub mod heap;
pub mod lambda;
pub mod opcode;
pub mod run;
pub mod stack;
pub mod transform;
pub mod vcell;
pub mod vector;

const HEAP_SIZE: usize = 4096;
struct Display(Box<dyn Fn(&Cell)>);

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

    /// Display Callback
    display: Display,
}

impl Vm {
    /// New
    ///
    /// Return a new Vm
    pub fn new() -> Vm {
        let mut vm = Vm {
            heap: Heap::new(HEAP_SIZE),
            ip: (usize::MAX, 0),
            stack: Stack::new(),
            globenv: GlobalEnvironment::new(),
            ep: usize::MAX,
            acc: VCell::undefined(),
            bp: 0,
            display: Display(Box::new(|cell| print!("{}", cell))),
        };
        vm.load_builtins();
        vm.load_prelude();
        vm
    }

    /// Load Prelude
    ///
    /// Read and compile prelude.scm
    pub fn load_prelude(&mut self) {
        let prelude_text = include_str!("../../prelude.scm");
        let prelude_tokens = lex::scan(prelude_text).expect("invalid prelude");
        let mut it = prelude_tokens.iter().peekable();
        while it.peek().is_some() {
            let ast = parse(prelude_text, &mut it).expect("invalid prelude");
            self.eval(&ast).expect("invalid prelude");
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
        trace!("entry: \n{}", self.decompile_text(&lambda));
        let lambda = self.heap.put(lambda);
        self.ip.0 = lambda.as_ptr().unwrap();
        self.ip.1 = 0;
        self.run()
    }

    pub fn set_display_fn(&mut self, func: Box<dyn Fn(&Cell)>) {
        self.display = Display(func);
    }

    pub fn display(&self, cell: &Cell) {
        self.display.display(cell)
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Display {
    fn display(&self, cell: &Cell) {
        self.0(cell)
    }
}

impl Debug for Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Display{{}}")
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

    #[error("invalid define syntax: {0}")]
    InvalidDefineSyntax(String),

    #[error("call of non-procedure: {0}")]
    InvalidProcedure(String),

    #[error("invalid stack index: {0}")]
    InvalidStackIndex(usize),

    #[error("invalid use of primitive {0}")]
    InvalidUsePrimitive(String),

    #[error("invalid syntax: {0}")]
    InvalidSyntax(String),

    #[error("lambda require at least one expression")]
    LambdaMissingExpression,

    #[error("misplaced macro keyword {0}")]
    MisplacedMacroKeyword(String),

    #[error("unknown procedure {0}")]
    UnknownProcedure(String),

    #[error("{0} is not bound")]
    VariableNotBound(String),

    #[error("invalid syntax: () must be quoted")]
    UnquotedNil,

    #[error("vector index {0} out of range of 0..{0}")]
    InvalidVectorIndex(usize, usize),
}
