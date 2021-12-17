use crate::cell::Cell;
use crate::vm::node::{Node, Value};
use crate::vm::opcode::OpCode;
use crate::vm::run::RuntimeError::{
    ExpectedPair, ExpectedStackValue, InvalidArgs, InvalidBytecode, InvalidNumArgs,
    UnknownProcedure,
};
use crate::vm::{Error, Vm};

#[derive(Debug, Eq, PartialEq)]
pub enum RuntimeError {
    ExpectedPair(Node),
    InvalidArgs(String, String, Node),
    InvalidNumArgs(String),
    InvalidBytecode,
    ExpectedStackValue,
    UnknownProcedure(String),
}

impl Vm {
    /// Run
    ///
    /// Run the virtual machine until it encounters a HALT instruction,
    /// and return the value contained within the ACC register as a Cell
    pub fn run(&mut self) -> Result<Cell, Error> {
        loop {
            match self.run_one() {
                Ok(true) => break,
                Ok(false) => continue,
                Err(e) => return Err(self.map_runtime_error(e)),
            }
        }
        Ok(self.heap.get_as_cell(&self.acc))
    }

    /// Execute one instruction, returning either a bool or runtime error.
    /// If the bool is set true, it indicates a HALT was encountered and
    /// execution should cease.
    fn run_one(&mut self) -> Result<bool, RuntimeError> {
        let op_code = self.read_op()?;
        match op_code {
            OpCode::Add | OpCode::Sub | OpCode::Mul => {
                let proc_name = match op_code {
                    OpCode::Add => "+",
                    OpCode::Sub => "-",
                    OpCode::Mul => "*",
                    _ => return Err(InvalidBytecode),
                };
                let x = self.heap.get(&self.acc);
                let y = self.pop_stack()?;
                let y = self.heap.get(&y);
                let x = x.as_fixed_num().ok_or_else(|| {
                    InvalidArgs(proc_name.to_string(), "number".to_string(), x.clone())
                })?;
                let y = y.as_fixed_num().ok_or_else(|| {
                    InvalidArgs(proc_name.to_string(), "number".to_string(), y.clone())
                })?;
                match op_code {
                    OpCode::Add => self.acc = self.heap.put(Value::fixed_num(x + y)),
                    OpCode::Sub => self.acc = self.heap.put(Value::fixed_num(y - x)),
                    OpCode::Mul => self.acc = self.heap.put(Value::fixed_num(x * y)),
                    _ => return Err(InvalidBytecode),
                };
            }
            OpCode::Car => {
                let arg = self.heap.get(&self.acc);
                self.acc = car(arg)?;
            }
            OpCode::Cdr => {
                let arg = self.heap.get(&self.acc);
                self.acc = cdr(arg)?;
            }
            OpCode::Cons => {
                let car = self.heap.put(self.acc.clone());
                let cdr = self.pop_stack()?;
                let cdr = self.heap.put(cdr);
                self.acc = self.heap.put(Value::Pair(
                    car.as_reference().unwrap(),
                    cdr.as_reference().unwrap(),
                ));
            }
            OpCode::Push => {
                self.stack.push(self.acc.clone());
            }
            OpCode::Quote => {
                self.acc = self.read_arg()?;
            }
            OpCode::Halt => return Ok(true),
        }

        Ok(false)
    }

    /// Pop Stack
    ///
    /// Pop a value off the stack. Error if the stack is empty.
    fn pop_stack(&mut self) -> Result<Node, RuntimeError> {
        self.stack.pop().ok_or(ExpectedStackValue)
    }

    /// Read Arg
    ///
    /// Read an argument node from program[ip], increment ip and
    /// return the value.
    fn read_arg(&mut self) -> Result<Node, RuntimeError> {
        let arg = self
            .bc
            .get(self.ip)
            .cloned()
            .filter(|it| !it.is_opcode())
            .ok_or(InvalidBytecode);
        self.ip += 1;
        arg
    }

    /// Read Op
    ///
    /// Read an op code from program[ip], increment ip and
    /// return the opcode.
    fn read_op(&mut self) -> Result<OpCode, RuntimeError> {
        let op = self
            .bc
            .get(self.ip)
            .cloned()
            .filter(|it| it.is_opcode())
            .map(|it| it.as_opcode().unwrap())
            .ok_or(InvalidBytecode);
        self.ip += 1;
        op
    }

    /// Map Runtime Error
    ///
    /// Map a runtime error to a vm::Error, converting any runtime values
    /// (e.g. references to heap values) to printable forms.
    fn map_runtime_error(&self, runtime_error: RuntimeError) -> Error {
        match runtime_error {
            ExpectedPair(node) => Error::ExpectedPair(self.heap.get_as_cell(&node).to_string()),
            InvalidArgs(proc, expected, got) => {
                Error::InvalidArgs(proc, expected, self.heap.get_as_cell(&got).to_string())
            }
            InvalidNumArgs(proc) => Error::InvalidNumArgs(proc),
            InvalidBytecode => Error::InvalidBytecode,
            ExpectedStackValue => Error::ExpectedStackValue,
            UnknownProcedure(proc) => Error::UnknownProcedure(proc),
        }
    }
}

fn car<T: AsRef<Node>>(node: T) -> Result<Node, RuntimeError> {
    node.as_ref()
        .as_car()
        .ok_or_else(|| ExpectedPair(node.as_ref().clone()))
}

fn cdr<T: AsRef<Node>>(node: T) -> Result<Node, RuntimeError> {
    node.as_ref()
        .as_cdr()
        .ok_or_else(|| ExpectedPair(node.as_ref().clone()))
}
