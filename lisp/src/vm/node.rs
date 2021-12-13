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
    FixedNum(FixedNum),
    Nil,
    OpCode(OpCode),
    Pair(Reference, Reference),
    Reference(Reference),
    Undefined,
}

impl Value {
    pub fn fixed_num(val: i64) -> Value {
        Value::FixedNum(FixedNum(val))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(packed)]
pub struct FixedNum(pub i64);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(packed)]
pub struct Reference(pub usize);

impl Node {
    pub fn new(val: Value) -> Node {
        Node { val, flags: 0 }
    }

    pub fn undefined() -> Node {
        Node::new(Value::Undefined)
    }

    pub fn reference(idx: usize) -> Node {
        Node::new(Value::Reference(Reference(idx)))
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

    pub fn car(&self) -> Option<Node> {
        match self.val {
            Value::Pair(Reference(car), _) => Some(Node::reference(car)),
            _ => None,
        }
    }

    pub fn cdr(&self) -> Option<Node> {
        match self.val {
            Value::Pair(_, Reference(cdr)) => Some(Node::reference(cdr)),
            _ => None,
        }
    }

    pub fn fixed_num(&self) -> Option<i64> {
        match &self.val {
            Value::FixedNum(FixedNum(val)) => Some(val.clone()),
            _ => None,
        }
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
impl fmt::Display for Node {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.val.clone() {
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Reference(Reference(val)) => write!(f, "[{}]", val),
            Value::OpCode(val) => write!(f, "{:?}", val),
            Value::Pair(Reference(car), Reference(cdr)) => write!(f, "({}, {})", car, cdr),
            Value::Undefined => write!(f, "undefined"),
            Value::FixedNum(FixedNum(val)) => write!(f, "{}", val),
            Value::Nil => write!(f, "()"),
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
