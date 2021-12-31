use crate::vm::opcode::OpCode;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VCell {
    Acc,
    Bool(bool),
    EnvSlot(usize),
    FixedNum(i64),
    Nil,
    OpCode(OpCode),
    Pair(usize, usize),
    Ptr(usize),
    Symbol(Rc<String>),
    Undefined,
    Void,
}

impl VCell {
    pub fn undefined() -> VCell {
        VCell::Undefined
    }

    pub fn void() -> VCell {
        VCell::Void
    }

    pub fn fixed_num<T: Into<i64>>(val: T) -> VCell {
        VCell::FixedNum(val.into())
    }

    pub fn ptr(val: usize) -> VCell {
        VCell::Ptr(val)
    }

    pub fn pair(car: usize, cdr: usize) -> VCell {
        VCell::Pair(car, cdr)
    }

    pub fn env_slot<T: Into<usize>>(slot: T) -> VCell {
        VCell::EnvSlot(slot.into())
    }

    pub fn symbol<T: Into<String>>(ptr: T) -> VCell {
        VCell::Symbol(Rc::new(ptr.into()))
    }

    pub fn nil() -> VCell {
        VCell::Nil
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, VCell::Pair(_, _))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, VCell::Ptr(_))
    }

    pub fn is_undefined(&self) -> bool {
        *self == VCell::Undefined
    }

    pub fn is_nil(&self) -> bool {
        *self == VCell::Nil
    }

    pub fn is_opcode(&self) -> bool {
        matches!(self, VCell::OpCode(_))
    }

    pub fn as_opcode(&self) -> Option<OpCode> {
        match self {
            VCell::OpCode(op) => Some(op.clone()),
            _ => None,
        }
    }

    pub fn as_car(&self) -> Option<VCell> {
        match self {
            VCell::Pair(car, _) => Some(VCell::Ptr(*car)),
            _ => None,
        }
    }

    pub fn as_cdr(&self) -> Option<VCell> {
        match self {
            VCell::Pair(_, cdr) => Some(VCell::Ptr(*cdr)),
            _ => None,
        }
    }

    pub fn as_fixed_num(&self) -> Option<i64> {
        match self {
            VCell::FixedNum(val) => Some(*val),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            VCell::Symbol(s) => Some(&*s),
            _ => None,
        }
    }

    pub fn as_ptr(&self) -> Option<usize> {
        match self {
            VCell::Ptr(ptr) => Some(*ptr),
            _ => None,
        }
    }

    pub fn as_env_slot(&self) -> Option<usize> {
        match self {
            VCell::EnvSlot(slot) => Some(*slot),
            _ => None,
        }
    }
}

impl From<OpCode> for VCell {
    fn from(op: OpCode) -> Self {
        VCell::OpCode(op)
    }
}

impl From<bool> for VCell {
    fn from(val: bool) -> Self {
        VCell::Bool(val)
    }
}

impl From<i64> for VCell {
    fn from(val: i64) -> Self {
        VCell::FixedNum(val)
    }
}

impl fmt::Display for VCell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VCell::Acc => write!(f, "%acc"),
            VCell::Bool(true) => write!(f, "#t"),
            VCell::Bool(false) => write!(f, "#f"),
            VCell::Ptr(ptr) => write!(f, "${:02x}", ptr),
            VCell::EnvSlot(slot) => write!(f, "g${:02x}", slot),
            VCell::OpCode(val) => write!(f, "{:?}", val),
            VCell::Pair(car, cdr) => write!(f, "(${:02x} . ${:02x})", car, cdr),
            VCell::Undefined => write!(f, "undefined"),
            VCell::FixedNum(val) => write!(f, "{}", val),
            VCell::Nil => write!(f, "()"),
            VCell::Symbol(s) => write!(f, "sym\"{}\"", *s),
            VCell::Void => write!(f, "#<void>"),
        }
    }
}

impl AsRef<VCell> for VCell {
    fn as_ref(&self) -> &VCell {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cell_size() {
        assert_eq!(std::mem::size_of::<VCell>(), 24);
    }

    #[test]
    fn new_cell_is_undefined() {
        assert!(matches!(VCell::undefined(), VCell::Undefined));
    }
}
