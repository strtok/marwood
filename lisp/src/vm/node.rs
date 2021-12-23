use crate::vm::node::RefType::{NodePtr, SymbolPtr};
use crate::vm::opcode::OpCode;
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Node {
    pub flags: u32,
    pub val: Value,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Bool(bool),
    EnvSlot(EnvSlot),
    FixedNum(FixedNum),
    Nil,
    OpCode(OpCode),
    Pair(Ptr, Ptr),
    Ptr(Ptr),
    Undefined,
    Void,
}

impl Value {
    pub fn fixed_num(val: i64) -> Value {
        Value::FixedNum(FixedNum(val))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(packed)]
pub struct Ptr(u64);

#[derive(Debug, Eq, PartialEq)]
pub enum RefType {
    NodePtr(usize),
    SymbolPtr(usize),
}

impl Ptr {
    const fn mask() -> u64 {
        0xFFFFFFFF << 48
    }

    const fn sym_flag() -> u64 {
        0b1 << 48
    }

    pub fn new_node_ptr(ptr: usize) -> Ptr {
        Ptr(ptr as u64)
    }

    pub fn new_symbol_ptr(ptr: usize) -> Ptr {
        let ptr = (ptr as u64) | Self::sym_flag();
        Ptr(ptr)
    }

    pub fn get(&self) -> RefType {
        if (self.0 & Self::sym_flag()) > 0 {
            RefType::SymbolPtr((self.0 & !Self::mask()) as usize)
        } else {
            RefType::NodePtr(self.0 as usize)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(packed)]
pub struct FixedNum(pub i64);

impl From<i64> for FixedNum {
    fn from(val: i64) -> Self {
        FixedNum(val)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(packed)]
pub struct EnvSlot(pub usize);

impl From<usize> for EnvSlot {
    fn from(slot: usize) -> Self {
        EnvSlot(slot)
    }
}

impl From<EnvSlot> for usize {
    fn from(val: EnvSlot) -> usize {
        val.0
    }
}

impl Node {
    pub fn new(val: Value) -> Node {
        Node { val, flags: 0 }
    }

    pub fn undefined() -> Node {
        Node::new(Value::Undefined)
    }

    pub fn void() -> Node {
        Node::new(Value::Void)
    }

    pub fn fixed_num<T: Into<FixedNum>>(val: T) -> Node {
        Node::new(Value::FixedNum(val.into()))
    }

    pub fn ptr(val: usize) -> Node {
        Node::new(Value::Ptr(Ptr::new_node_ptr(val)))
    }

    pub fn env_slot<T: Into<EnvSlot>>(slot: T) -> Node {
        Node::new(Value::EnvSlot(slot.into()))
    }

    pub fn symbol<T: Into<usize>>(ptr: T) -> Node {
        Ptr::new_symbol_ptr(ptr.into()).into()
    }

    pub fn nil() -> Node {
        Node::new(Value::Nil)
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self.val, Value::Ptr(_))
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self.val, Value::Undefined)
    }

    pub fn is_nil(&self) -> bool {
        matches!(self.val, Value::Nil)
    }

    pub fn is_opcode(&self) -> bool {
        matches!(self.val, Value::OpCode(_))
    }

    pub fn as_opcode(&self) -> Option<OpCode> {
        match &self.val {
            Value::OpCode(op) => Some(op.clone()),
            _ => None,
        }
    }

    pub fn as_car(&self) -> Option<Node> {
        match self.val {
            Value::Pair(car, _) => Some(car.into()),
            _ => None,
        }
    }

    pub fn as_cdr(&self) -> Option<Node> {
        match self.val {
            Value::Pair(_, cdr) => Some(cdr.into()),
            _ => None,
        }
    }

    pub fn as_fixed_num(&self) -> Option<i64> {
        match self.val {
            Value::FixedNum(FixedNum(val)) => Some(val),
            _ => None,
        }
    }

    pub fn as_ptr(&self) -> Option<Ptr> {
        match self.val {
            Value::Ptr(val) => Some(val),
            _ => None,
        }
    }

    pub fn as_env_slot(&self) -> Option<EnvSlot> {
        match self.val {
            Value::EnvSlot(val) => Some(val),
            _ => None,
        }
    }

    pub fn as_symbol_ptr(&self) -> Option<usize> {
        match self.val {
            Value::Ptr(ptr) => match ptr.get() {
                SymbolPtr(ptr) => Some(ptr),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_node_ptr(&self) -> Option<usize> {
        match self.val {
            Value::Ptr(ptr) => match ptr.get() {
                NodePtr(ptr) => Some(ptr),
                _ => None,
            },
            _ => None,
        }
    }
}

impl<T> From<T> for Value
where
    T: AsRef<Node>,
{
    fn from(node: T) -> Self {
        node.as_ref().clone().val
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Value {
        Value::Bool(val)
    }
}

impl<T> From<T> for Node
where
    T: AsRef<Value>,
{
    fn from(val: T) -> Self {
        Node::new(val.as_ref().clone())
    }
}

impl From<OpCode> for Node {
    fn from(val: OpCode) -> Self {
        Node::new(Value::OpCode(val))
    }
}

impl From<bool> for Node {
    fn from(val: bool) -> Self {
        Node::new(Value::Bool(val))
    }
}

impl From<FixedNum> for Node {
    fn from(val: FixedNum) -> Self {
        Node::new(Value::FixedNum(val))
    }
}

impl From<Ptr> for Node {
    fn from(val: Ptr) -> Self {
        Node::new(Value::Ptr(val))
    }
}

impl From<EnvSlot> for Node {
    fn from(val: EnvSlot) -> Self {
        Node::new(Value::EnvSlot(val))
    }
}

impl fmt::Display for Node {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.val.clone() {
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Ptr(ptr) => match ptr.get() {
                SymbolPtr(ptr) => write!(f, "sym[{}]", ptr),
                NodePtr(ptr) => write!(f, "[{}]", ptr),
            },
            Value::EnvSlot(EnvSlot(val)) => write!(f, "env[{}]", val),
            Value::OpCode(val) => write!(f, "{:?}", val),
            Value::Pair(Ptr(car), Ptr(cdr)) => write!(f, "({}, {})", car, cdr),
            Value::Undefined => write!(f, "undefined"),
            Value::FixedNum(FixedNum(val)) => write!(f, "{}", val),
            Value::Nil => write!(f, "()"),
            Value::Void => write!(f, ""),
        }
    }
}

impl AsRef<Node> for Node {
    fn as_ref(&self) -> &Node {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cell_size() {
        assert_eq!(std::mem::size_of::<Node>(), 24);
    }

    #[test]
    fn new_cell_is_undefined() {
        assert!(matches!(
            Node::undefined(),
            Node {
                val: Value::Undefined,
                ..
            }
        ));
    }

    #[test]
    fn ptr_tagged_type() {
        let node_ref = Ptr::new_node_ptr(50);
        assert_eq!(node_ref.get(), RefType::NodePtr(50));

        let sym_ref = Ptr::new_symbol_ptr(50);
        assert_eq!(sym_ref.get(), RefType::SymbolPtr(50));
    }
}
