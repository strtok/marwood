use crate::cell::Cell;
use crate::vm::node::Node;
use crate::vm::opcode::OpCode;
use crate::vm::{Error, Vm};

impl Vm {
    /// Run
    ///
    /// Run the virtual machine until it encounters a HALT instruction,
    /// and return the value contained within the ACC register as a Cell
    pub fn run(&mut self) -> Result<Cell, Error> {
        loop {
            match self.read_op()? {
                OpCode::Car => {
                    let arg = self.heap.get(&self.acc);
                    self.acc = car(arg)?;
                }
                OpCode::Cdr => {
                    let arg = self.heap.get(&self.acc);
                    self.acc = cdr(arg)?;
                }
                OpCode::Quote => {
                    self.acc = self.read_arg()?;
                }
                OpCode::Halt => break,
            }
        }
        Ok(self.heap.get_as_cell(&self.acc))
    }

    /// Read Arg
    ///
    /// Read an argument node from program[ip], increment ip and
    /// return the value.
    fn read_arg(&mut self) -> Result<Node, Error> {
        let arg = self
            .program
            .get(self.ip)
            .cloned()
            .filter(|it| !it.is_opcode())
            .ok_or(Error::InvalidBytecode);
        self.ip += 1;
        arg
    }

    /// Read Op
    ///
    /// Read an op code from program[ip], increment ip and
    /// return the opcode.
    fn read_op(&mut self) -> Result<OpCode, Error> {
        let op = self
            .program
            .get(self.ip)
            .cloned()
            .filter(|it| it.is_opcode())
            .map(|it| it.as_opcode().unwrap())
            .ok_or(Error::InvalidBytecode);
        self.ip += 1;
        op
    }
}

fn car<T: AsRef<Node>>(node: T) -> Result<Node, Error> {
    node.as_ref()
        .car()
        .ok_or_else(|| Error::ExpectedPair(node.as_ref().to_string()))
}

fn cdr<T: AsRef<Node>>(node: T) -> Result<Node, Error> {
    node.as_ref()
        .cdr()
        .ok_or_else(|| Error::ExpectedPair(node.as_ref().to_string()))
}
