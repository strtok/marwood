use crate::cell::Cell;
use crate::vm::node::{Node, Value};
use crate::vm::opcode::OpCode;
use crate::vm::run::RuntimeError::{
    ExpectedPair, ExpectedStackValue, InvalidArgs, InvalidBytecode,
};
use crate::vm::{Error, Vm};
use std::collections::HashMap;

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
                Err(e) => return Err(e.into_error(self)),
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
}

/// Environment
///
/// Environment represents a binding of a symbol to a value in the heap.
/// The environment tracks both deep bindings (sym -> slot), and also a
/// vector of shallow bindings (slot -> Node).
#[derive(Debug)]
pub struct Environment {
    /// Deep bindings is a map of symbol reference -> slot, and
    /// is used by the compiler to aassociate a symbol at compilation
    /// time with the environment slot.
    bindings: HashMap<usize, usize>,

    /// Environment slots. The compiler produces shallow bindings as
    /// references into this vector at compile time.
    slots: Vec<Node>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            bindings: HashMap::new(),
            slots: vec![],
        }
    }

    /// Get binding
    ///
    /// Get binding provides a deep binding lookup of sym -> slot. If the
    /// binding does not already exist, get_binding() will crates a new
    /// binding and return the newly bound slot.
    ///
    /// # Arguments
    /// `sym` - The symbol to provide a binding for
    pub fn get_binding<T: Into<usize>>(&mut self, sym: T) -> usize {
        let sym: usize = sym.into();
        match self.bindings.get(&sym) {
            Some(slot) => *slot,
            None => {
                self.slots.push(Node::undefined());
                let slot = self.slots.len() - 1;
                self.bindings.insert(sym, slot);
                slot
            }
        }
    }

    /// Get slot
    ///
    /// Get slot provides a shallow binding interface. Given the environment
    /// slot, return the bound value.
    ///
    /// Any slot accessed by the runtime must have been valid at compilation
    /// time, so any reference to an unknown slot is considered a critical error
    /// and will cause a panic of the runtime.
    ///
    /// # Arguments
    /// `slot` - The slot to return a value for
    pub fn get_slot(&self, slot: usize) -> Node {
        self.slots
            .get(slot)
            .expect("invalid environment slot")
            .clone()
    }

    /// Put slot
    ///
    /// Get slot provides a shallow binding interface. Given the environment
    /// slot, return the bound value.
    ///
    /// Any slot accessed by the runtime must have been valid at compilation
    /// time, so any reference to an unknown slot is considered a critical error
    /// and will cause a panic of the runtime.
    ///
    /// Any value other than a reference or undefined is considered a runtime error
    /// and will result in a panic.
    ///
    /// # Arguments
    /// `slot` - The slot to return a value for
    pub fn put_slot(&mut self, slot: usize, node: Node) {
        assert!(matches!(node.val, Value::Reference(_) | Value::Undefined));
        *self.slots.get_mut(slot).expect("invalid environment slot") = node;
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum RuntimeError {
    ExpectedPair(Node),
    InvalidArgs(String, String, Node),
    InvalidBytecode,
    ExpectedStackValue,
}

impl RuntimeError {
    /// Into Error
    ///
    /// Convert a runtime error into a vm::Error, converting any runtime values
    /// (e.g. references to heap values) to printable forms -- This function requires
    /// read only access to the Vm in order to access the heap.
    fn into_error(self, vm: &Vm) -> Error {
        match self {
            ExpectedPair(node) => Error::ExpectedPair(vm.heap.get_as_cell(&node).to_string()),
            InvalidArgs(proc, expected, got) => {
                Error::InvalidArgs(proc, expected, vm.heap.get_as_cell(&got).to_string())
            }
            InvalidBytecode => Error::InvalidBytecode,
            ExpectedStackValue => Error::ExpectedStackValue,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::node::StringReference;

    #[test]
    fn same_binding_same_slot() {
        let mut env = Environment::new();
        assert_eq!(env.get_binding(StringReference(50)), 0);
        assert_eq!(env.get_binding(StringReference(100)), 1);
        assert_eq!(env.get_binding(StringReference(50)), 0);
        assert_eq!(env.get_slot(0), Node::undefined());
        env.put_slot(0, Node::reference(42));
        assert_eq!(env.get_slot(0), Node::reference(42));
        env.put_slot(0, Node::undefined());
    }

    #[test]
    #[should_panic]
    fn put_slot_panics_if_non_reference() {
        let mut env = Environment::new();
        assert_eq!(env.get_binding(StringReference(50)), 0);
        env.put_slot(0, Node::nil());
    }
}
