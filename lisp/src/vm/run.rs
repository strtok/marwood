use crate::cell::Cell;
use crate::vm::node::{Node, Value};
use crate::vm::opcode::OpCode;
use crate::vm::Error::InvalidArgs;
use crate::vm::{Error, Vm};

impl Vm {
    /// Run
    ///
    /// Run the virtual machine until it encounters a HALT instruction,
    /// and return the value contained within the ACC register as a Cell
    pub fn run(&mut self) -> Result<Cell, Error> {
        loop {
            match self.read_op()? {
                OpCode::Add => {
                    let x = self.heap.get(&self.acc);
                    let y = self.pop_stack()?;
                    let y = self.heap.get(&y);
                    let x = x.as_fixed_num().ok_or_else(|| {
                        InvalidArgs(
                            "+".to_string(),
                            "number".to_string(),
                            self.heap.get_as_cell(&x).to_string(),
                        )
                    })?;
                    let y = y.as_fixed_num().ok_or_else(|| {
                        InvalidArgs(
                            "+".to_string(),
                            "number".to_string(),
                            self.heap.get_as_cell(&y).to_string(),
                        )
                    })?;

                    self.acc = self.heap.put(Value::fixed_num(x + y));
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
            .bc
            .get(self.ip)
            .cloned()
            .filter(|it| !it.is_opcode())
            .ok_or(Error::InvalidBytecode);
        self.ip += 1;
        arg
    }

    /// Read Arg
    ///
    /// Pop a value off the stack or error
    fn pop_stack(&mut self) -> Result<Node, Error> {
        self.stack.pop().ok_or(Error::ExpectedStackValue)
    }

    /// Read Op
    ///
    /// Read an op code from program[ip], increment ip and
    /// return the opcode.
    fn read_op(&mut self) -> Result<OpCode, Error> {
        let op = self
            .bc
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
        .as_car()
        .ok_or_else(|| Error::ExpectedPair(node.as_ref().to_string()))
}

fn cdr<T: AsRef<Node>>(node: T) -> Result<Node, Error> {
    node.as_ref()
        .as_cdr()
        .ok_or_else(|| Error::ExpectedPair(node.as_ref().to_string()))
}
