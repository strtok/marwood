use crate::vm::node::TaggedPtr::{NodePtr, SymbolPtr};
use crate::vm::opcode::OpCode;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Node {
    Bool(bool),
    EnvSlot(usize),
    FixedNum(i64),
    Nil,
    OpCode(OpCode),
    Pair(Ptr, Ptr),
    Ptr(Ptr),
    Symbol(Rc<String>),
    Undefined,
    Void,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Ptr(u64);

#[derive(Debug, Eq, PartialEq)]
pub enum TaggedPtr {
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

    pub fn get(&self) -> TaggedPtr {
        if (self.0 & Self::sym_flag()) > 0 {
            TaggedPtr::SymbolPtr((self.0 & !Self::mask()) as usize)
        } else {
            TaggedPtr::NodePtr(self.0 as usize)
        }
    }

    pub fn as_usize(&self) -> usize {
        match self.get() {
            TaggedPtr::SymbolPtr(ptr) | TaggedPtr::NodePtr(ptr) => ptr,
        }
    }
}

impl Node {
    pub fn undefined() -> Node {
        Node::Undefined
    }

    pub fn void() -> Node {
        Node::Void
    }

    pub fn fixed_num<T: Into<i64>>(val: T) -> Node {
        Node::FixedNum(val.into())
    }

    pub fn ptr(val: usize) -> Node {
        Node::Ptr(Ptr::new_node_ptr(val))
    }

    pub fn pair(car: Ptr, cdr: Ptr) -> Node {
        Node::Pair(car, cdr)
    }

    pub fn env_slot<T: Into<usize>>(slot: T) -> Node {
        Node::EnvSlot(slot.into())
    }

    pub fn symbol<T: Into<String>>(ptr: T) -> Node {
        Node::Symbol(Rc::new(ptr.into()))
    }

    pub fn symbol_ptr<T: Into<usize>>(ptr: T) -> Node {
        Ptr::new_symbol_ptr(ptr.into()).into()
    }

    pub fn nil() -> Node {
        Node::Nil
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, Node::Pair(_, _))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Node::Ptr(_))
    }

    pub fn is_undefined(&self) -> bool {
        *self == Node::Undefined
    }

    pub fn is_nil(&self) -> bool {
        *self == Node::Nil
    }

    pub fn is_opcode(&self) -> bool {
        matches!(self, Node::OpCode(_))
    }

    pub fn as_opcode(&self) -> Option<OpCode> {
        match self {
            Node::OpCode(op) => Some(op.clone()),
            _ => None,
        }
    }

    pub fn as_car(&self) -> Option<Node> {
        match self {
            Node::Pair(car, _) => Some(car.into()),
            _ => None,
        }
    }

    pub fn as_cdr(&self) -> Option<Node> {
        match self {
            Node::Pair(_, cdr) => Some(cdr.into()),
            _ => None,
        }
    }

    pub fn as_fixed_num(&self) -> Option<i64> {
        match self {
            Node::FixedNum(val) => Some(*val),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Node::Symbol(s) => Some(&*s),
            _ => None,
        }
    }

    pub fn as_ptr(&self) -> Option<Ptr> {
        match self {
            Node::Ptr(ptr) => Some(*ptr),
            _ => None,
        }
    }

    pub fn as_env_slot(&self) -> Option<usize> {
        match self {
            Node::EnvSlot(slot) => Some(*slot),
            _ => None,
        }
    }

    pub fn as_symbol_ptr(&self) -> Option<usize> {
        match self {
            Node::Ptr(ptr) => match ptr.get() {
                SymbolPtr(ptr) => Some(ptr),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_node_ptr(&self) -> Option<usize> {
        match self {
            Node::Ptr(ptr) => match ptr.get() {
                NodePtr(ptr) => Some(ptr),
                _ => None,
            },
            _ => None,
        }
    }
}

impl From<OpCode> for Node {
    fn from(op: OpCode) -> Self {
        Node::OpCode(op)
    }
}

impl From<bool> for Node {
    fn from(val: bool) -> Self {
        Node::Bool(val)
    }
}

impl From<i64> for Node {
    fn from(val: i64) -> Self {
        Node::FixedNum(val)
    }
}

impl From<Ptr> for Node {
    fn from(ptr: Ptr) -> Self {
        Node::Ptr(ptr)
    }
}

impl From<&Ptr> for Node {
    fn from(ptr: &Ptr) -> Self {
        Node::Ptr(*ptr)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Bool(true) => write!(f, "#t"),
            Node::Bool(false) => write!(f, "#f"),
            Node::Ptr(ptr) => match ptr.get() {
                SymbolPtr(ptr) => write!(f, "sym[{}]", ptr),
                NodePtr(ptr) => write!(f, "[{}]", ptr),
            },
            Node::EnvSlot(slot) => write!(f, "env[{}]", slot),
            Node::OpCode(val) => write!(f, "{:?}", val),
            Node::Pair(car, cdr) => write!(f, "([{}], [{}])", car.as_usize(), cdr.as_usize()),
            Node::Undefined => write!(f, "undefined"),
            Node::FixedNum(val) => write!(f, "{}", val),
            Node::Nil => write!(f, "()"),
            Node::Symbol(s) => write!(f, "sym\"{}\"", *s),
            Node::Void => write!(f, "#<void>"),
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
        assert!(matches!(Node::undefined(), Node::Undefined));
    }

    #[test]
    fn ptr_tagged_type() {
        let node_ref = Ptr::new_node_ptr(50);
        assert_eq!(node_ref.get(), TaggedPtr::NodePtr(50));

        let sym_ref = Ptr::new_symbol_ptr(50);
        assert_eq!(sym_ref.get(), TaggedPtr::SymbolPtr(50));
    }
}
