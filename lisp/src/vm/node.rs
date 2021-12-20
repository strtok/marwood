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
    Pair(Reference, Reference),
    Reference(Reference),
    Symbol(StringReference),
    Undefined,
    Void,
}

impl Value {
    pub fn fixed_num(val: i64) -> Value {
        Value::FixedNum(FixedNum(val))
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
pub struct Reference(pub usize);

impl From<usize> for Reference {
    fn from(idx: usize) -> Self {
        Reference(idx)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(packed)]
pub struct StringReference(pub usize);

impl From<usize> for StringReference {
    fn from(idx: usize) -> Self {
        StringReference(idx)
    }
}

impl Into<usize> for StringReference {
    fn into(self) -> usize {
        self.0
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(packed)]
pub struct EnvSlot(pub usize);

impl From<usize> for EnvSlot {
    fn from(idx: usize) -> Self {
        EnvSlot(idx)
    }
}

impl Into<usize> for EnvSlot {
    fn into(self) -> usize {
        self.0
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

    pub fn reference<T: Into<Reference>>(reference: T) -> Node {
        Node::new(Value::Reference(reference.into()))
    }

    pub fn env_slot<T: Into<EnvSlot>>(slot: T) -> Node {
        Node::new(Value::EnvSlot(slot.into()))
    }

    pub fn symbol<T: Into<StringReference>>(reference: T) -> Node {
        Node::new(Value::Symbol(reference.into()))
    }

    pub fn nil() -> Node {
        Node::new(Value::Nil)
    }

    pub fn is_reference(&self) -> bool {
        matches!(self.val, Value::Reference(_))
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
            Value::Pair(Reference(car), _) => Some(Node::reference(car)),
            _ => None,
        }
    }

    pub fn as_cdr(&self) -> Option<Node> {
        match self.val {
            Value::Pair(_, Reference(cdr)) => Some(Node::reference(cdr)),
            _ => None,
        }
    }

    pub fn as_fixed_num(&self) -> Option<i64> {
        match self.val {
            Value::FixedNum(FixedNum(val)) => Some(val),
            _ => None,
        }
    }

    pub fn as_reference(&self) -> Option<Reference> {
        match self.val {
            Value::Reference(val) => Some(val),
            _ => None,
        }
    }

    pub fn as_env_slot(&self) -> Option<EnvSlot> {
        match self.val {
            Value::EnvSlot(val) => Some(val),
            _ => None,
        }
    }

    pub fn as_symbol_reference(&self) -> Option<usize> {
        match self.val {
            Value::Symbol(val) => Some(val.0),
            _ => None,
        }
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

impl<T> From<T> for Value
where
    T: AsRef<Node>,
{
    fn from(node: T) -> Self {
        node.as_ref().clone().val
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

impl From<Reference> for Node {
    fn from(val: Reference) -> Self {
        Node::new(Value::Reference(val))
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
            Value::Reference(Reference(val)) => write!(f, "[{}]", val),
            Value::EnvSlot(EnvSlot(val)) => write!(f, "env[{}]", val),
            Value::Symbol(StringReference(val)) => write!(f, "str[{}]", val),
            Value::OpCode(val) => write!(f, "{:?}", val),
            Value::Pair(Reference(car), Reference(cdr)) => write!(f, "({}, {})", car, cdr),
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
}
