use crate::cell::Cell;
use crate::vm::node::TaggedReference::SymbolReference;
use crate::vm::node::{Node, Value};
use crate::vm::opcode::OpCode;
use crate::vm::run::RuntimeError::{
    ExpectedPair, ExpectedStackValue, InvalidArgs, InvalidBytecode, VariableNotBound,
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
            OpCode::EnvGet => {
                let env_slot = self.read_arg()?.as_env_slot().expect("expected env slot");
                self.acc = self.globenv.get_slot(env_slot.0);
                if matches!(self.acc.val, Value::Undefined) {
                    return Err(VariableNotBound(self.get_str_bound_to(env_slot)));
                }
            }
            OpCode::EnvSet => {
                let env_slot = self.read_arg()?.as_env_slot().expect("expected env slot");
                self.globenv.put_slot(env_slot.0, self.acc.clone());
                self.acc = self.heap.put(Node::void());
            }
            OpCode::Eq => {
                let arg = self.pop_stack()?;
                self.acc = self.heap.put(self.acc == arg);
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

    /// Get Symbol Bound To
    ///
    /// Given either an environment slot, or a symbol reference, return the
    /// original string bound to the reference.
    ///
    /// The runtime cost of this function is > O(1), but it's only needed
    /// during an error path or debugging.
    ///
    /// # Arguments
    /// `node` - The node containing an environment slot of sym reference
    /// to provide a reverse lookup for.
    ///
    /// # Returns
    /// Returns the bound symbol, or "#<undefined>" if symbol lookup failed
    /// for any reason.
    fn get_str_bound_to<T: Into<Node>>(&self, node: T) -> String {
        let node = node.into();
        match node.val {
            Value::EnvSlot(slot) => match self.globenv.get_symbol(slot) {
                Some(sym_ref) => self.get_str_bound_to(Node::symbol(sym_ref)),
                None => "#<undefined>".into(),
            },
            Value::Reference(ptr) => match ptr.get() {
                SymbolReference(ptr) => self
                    .heap
                    .string_heap
                    .get_symbol(ptr)
                    .unwrap_or_else(|| "#<undefined>".into()),
                _ => "#<undefined>".into(),
            },
            _ => "#<undefined>".into(),
        }
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

    /// Get Symbol
    ///
    /// Get the symbol bound to an environment slot. This is a reverse lookup
    /// of a binding created with get_binding().
    ///
    /// # Arguments
    /// `slot` - The slot to find the symbol for.
    pub fn get_symbol<T: Into<usize>>(&self, slot: T) -> Option<usize> {
        let slot = slot.into();
        self.bindings
            .iter()
            .find(|it| *(it.1) == slot)
            .map(|it| *it.0)
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

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum RuntimeError {
    ExpectedPair(Node),
    InvalidArgs(String, String, Node),
    InvalidBytecode,
    ExpectedStackValue,
    VariableNotBound(String),
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
            VariableNotBound(sym) => Error::VariableNotBound(sym),
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

    #[test]
    fn same_binding_same_slot() {
        let mut env = Environment::new();
        assert_eq!(env.get_binding(50_usize), 0);
        assert_eq!(env.get_binding(100_usize), 1);
        assert_eq!(env.get_binding(50_usize), 0);
        assert_eq!(env.get_symbol(0_usize), Some(50_usize));
        assert_eq!(env.get_symbol(1_usize), Some(100_usize));
        assert_eq!(env.get_slot(0), Node::undefined());
        env.put_slot(0, Node::reference(42));
        assert_eq!(env.get_slot(0), Node::reference(42));
        env.put_slot(0, Node::undefined());
    }

    #[test]
    #[should_panic]
    fn put_slot_panics_if_non_reference() {
        let mut env = Environment::new();
        assert_eq!(env.get_binding(50_usize), 0);
        env.put_slot(0, Node::nil());
    }
}
