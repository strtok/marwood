use crate::cell::Cell;
use crate::error::Error;
use crate::lex;
use crate::parse;
use crate::vm::environment::GlobalEnvironment;
use crate::vm::heap::Heap;
use crate::vm::stack::Stack;
use crate::vm::vcell::VCell;
use log::trace;
use std::fmt::Debug;

pub mod builtin;
pub mod compare;
pub mod compile;
pub mod continuation;
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

const HEAP_CHUNK_SIZE: usize = 8192;

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

    /// System Interface (display, write, etc).
    sys: Box<dyn SystemInterface>,
}

impl Vm {
    /// New
    ///
    /// Return a new Vm
    pub fn new() -> Vm {
        let mut vm = Vm {
            heap: Heap::new(HEAP_CHUNK_SIZE),
            ip: (usize::MAX, 0),
            stack: Stack::new(),
            globenv: GlobalEnvironment::new(),
            ep: usize::MAX,
            acc: VCell::undefined(),
            bp: 0,
            sys: Box::new(StubInterface {}),
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
            let ast = parse::parse(prelude_text, &mut it).expect("invalid prelude");
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
        self.prepare_eval(cell)?;
        self.run()
    }

    pub fn prepare_eval(&mut self, cell: &Cell) -> Result<(), Error> {
        let lambda = self.compile_runnable(cell)?;
        trace!("entry: \n{}", self.decompile_text(&lambda));
        let lambda = self.heap.put(lambda);
        self.ip.0 = lambda.as_ptr().unwrap();
        self.ip.1 = 0;
        Ok(())
    }

    /// Eval Text
    ///
    /// Parse and eval one expression, returning the result of
    /// evaluation and remaining text if any.
    ///
    /// # Arguments
    /// `text` - The text to eval
    pub fn eval_text<'a>(&mut self, text: &'a str) -> Result<(Cell, Option<&'a str>), Error> {
        let (cell, remaining_text) = parse::parse_text(text)?;
        self.prepare_eval(&cell)?;
        Ok((self.run()?, remaining_text))
    }

    pub fn set_system_interface(&mut self, sys: Box<dyn SystemInterface>) {
        self.sys = sys;
    }

    pub fn display(&self, cell: &Cell) {
        self.sys.display(cell)
    }

    pub fn write(&self, cell: &Cell) {
        self.sys.write(cell)
    }

    pub fn term_rows(&self) -> usize {
        self.sys.terminal_dimensions().1
    }

    pub fn term_cols(&self) -> usize {
        self.sys.terminal_dimensions().0
    }

    pub fn time_utc(&self) -> u64 {
        self.sys.time_utc()
    }

    pub fn global_symbols(&self) -> Vec<&str> {
        self.globenv
            .iter_bindings()
            .map(|sym| self.heap.get_at_index(*sym).as_symbol().unwrap())
            .collect()
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

/// SystemInterface is the interface between marwood and the operating
/// environment (e.g. display, write, etc).
pub trait SystemInterface: Debug {
    fn display(&self, cell: &Cell);
    fn write(&self, cell: &Cell);
    fn terminal_dimensions(&self) -> (usize, usize);
    fn time_utc(&self) -> u64;
}

#[derive(Debug)]
struct StubInterface {}
impl SystemInterface for StubInterface {
    fn display(&self, _: &Cell) {}
    fn write(&self, _: &Cell) {}
    fn terminal_dimensions(&self) -> (usize, usize) {
        (0, 0)
    }
    fn time_utc(&self) -> u64 {
        0
    }
}
