use crate::vm::opcode::OpCode;
use crate::vm::vcell::TaggedPtr::{SymbolPtr, VCellPtr};
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VCell {
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
    VCellPtr(usize),
    SymbolPtr(usize),
}

impl Ptr {
    const fn mask() -> u64 {
        0xFFFFFFFF << 48
    }

    const fn sym_flag() -> u64 {
        0b1 << 48
    }

    pub fn new_vcell_ptr(ptr: usize) -> Ptr {
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
            TaggedPtr::VCellPtr(self.0 as usize)
        }
    }

    pub fn as_usize(&self) -> usize {
        match self.get() {
            TaggedPtr::SymbolPtr(ptr) | TaggedPtr::VCellPtr(ptr) => ptr,
        }
    }
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
        VCell::Ptr(Ptr::new_vcell_ptr(val))
    }

    pub fn pair(car: Ptr, cdr: Ptr) -> VCell {
        VCell::Pair(car, cdr)
    }

    pub fn env_slot<T: Into<usize>>(slot: T) -> VCell {
        VCell::EnvSlot(slot.into())
    }

    pub fn symbol<T: Into<String>>(ptr: T) -> VCell {
        VCell::Symbol(Rc::new(ptr.into()))
    }

    pub fn symbol_ptr<T: Into<usize>>(ptr: T) -> VCell {
        Ptr::new_symbol_ptr(ptr.into()).into()
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
            VCell::Pair(car, _) => Some(car.into()),
            _ => None,
        }
    }

    pub fn as_cdr(&self) -> Option<VCell> {
        match self {
            VCell::Pair(_, cdr) => Some(cdr.into()),
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

    pub fn as_ptr(&self) -> Option<Ptr> {
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

    pub fn as_symbol_ptr(&self) -> Option<usize> {
        match self {
            VCell::Ptr(ptr) => match ptr.get() {
                SymbolPtr(ptr) => Some(ptr),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_vcell_ptr(&self) -> Option<usize> {
        match self {
            VCell::Ptr(ptr) => match ptr.get() {
                VCellPtr(ptr) => Some(ptr),
                _ => None,
            },
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

impl From<Ptr> for VCell {
    fn from(ptr: Ptr) -> Self {
        VCell::Ptr(ptr)
    }
}

impl From<&Ptr> for VCell {
    fn from(ptr: &Ptr) -> Self {
        VCell::Ptr(*ptr)
    }
}

impl fmt::Display for VCell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VCell::Bool(true) => write!(f, "#t"),
            VCell::Bool(false) => write!(f, "#f"),
            VCell::Ptr(ptr) => match ptr.get() {
                SymbolPtr(ptr) => write!(f, "sym[{}]", ptr),
                VCellPtr(ptr) => write!(f, "[{}]", ptr),
            },
            VCell::EnvSlot(slot) => write!(f, "env[{}]", slot),
            VCell::OpCode(val) => write!(f, "{:?}", val),
            VCell::Pair(car, cdr) => write!(f, "([{}], [{}])", car.as_usize(), cdr.as_usize()),
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

    #[test]
    fn ptr_tagged_type() {
        let vcell_ref = Ptr::new_vcell_ptr(50);
        assert_eq!(vcell_ref.get(), TaggedPtr::VCellPtr(50));

        let sym_ref = Ptr::new_symbol_ptr(50);
        assert_eq!(sym_ref.get(), TaggedPtr::SymbolPtr(50));
    }
}
