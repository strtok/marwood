use crate::vm::opcode::OpCode;
use crate::vm::vcell::Binding::Global;
use std::fmt;
use std::rc::Rc;

/// VCell
///
/// VCell is a 24 byte (3x64bit) type that's used to represent values
/// in the VM runtime:
///
/// * Primitive list values (bool, fixednum, pairs, nil, etc)
/// * Opcodes
/// * References into the heap (Ptr)
/// * Environment slot references (EnvSlot)
/// * Other runtime sentinel values, such as Undefined and Void.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VCell {
    Acc,
    BasePointer(usize),
    BasePointerOffset(i64),
    Bool(bool),
    Closure(usize, usize),
    EnvSlot(usize),
    FixedNum(i64),
    InstructionPointer(usize, usize),
    Lambda(Rc<Lambda>),
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

    pub fn lambda<T: Into<Lambda>>(lambda: T) -> VCell {
        VCell::Lambda(Rc::new(lambda.into()))
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

    pub fn is_envslot(&self) -> bool {
        matches!(self, VCell::EnvSlot(_))
    }

    pub fn is_reference(&self) -> bool {
        self.is_ptr() || self.is_envslot()
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

    pub fn is_lambda(&self) -> bool {
        matches!(self, VCell::Lambda(_))
    }

    pub fn is_closure(&self) -> bool {
        matches!(self, VCell::Closure(_, _))
    }

    pub fn is_procedure(&self) -> bool {
        self.is_lambda() || self.is_closure()
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

    pub fn as_lambda(&self) -> Option<&Lambda> {
        match self {
            VCell::Lambda(lambda) => Some(&*lambda),
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

    pub fn as_ip(&self) -> Option<(usize, usize)> {
        match self {
            VCell::InstructionPointer(lambda, ip) => Some((*lambda, *ip)),
            _ => None,
        }
    }

    pub fn as_bp(&self) -> Option<usize> {
        match self {
            VCell::BasePointer(bp) => Some(*bp),
            _ => None,
        }
    }

    pub fn as_bp_offset(&self) -> Option<i64> {
        match self {
            VCell::BasePointerOffset(offset) => Some(*offset),
            _ => None,
        }
    }
}

impl From<OpCode> for VCell {
    fn from(op: OpCode) -> Self {
        VCell::OpCode(op)
    }
}

impl From<Lambda> for VCell {
    fn from(lambda: Lambda) -> Self {
        VCell::lambda(lambda)
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
            VCell::BasePointer(bp) => write!(f, "bp[${:+02x}]", bp),
            VCell::BasePointerOffset(offset) => write!(f, "bp[{:+02x}]", offset),
            VCell::Bool(true) => write!(f, "#t"),
            VCell::Bool(false) => write!(f, "#f"),
            VCell::Closure(_, _) => write!(f, "#<closure>"),
            VCell::EnvSlot(slot) => write!(f, "g${:02x}", slot),
            VCell::FixedNum(val) => write!(f, "{}", val),
            VCell::InstructionPointer(lambda, ip) => {
                write!(f, "ip${:02x},{:02x}", *lambda, *ip)
            }
            VCell::Lambda(_) => write!(f, "#<lambda>"),
            VCell::Nil => write!(f, "()"),
            VCell::OpCode(val) => write!(f, "{:?}", val),
            VCell::Pair(car, cdr) => write!(f, "(${:02x} . ${:02x})", car, cdr),
            VCell::Ptr(ptr) => write!(f, "${:02x}", ptr),
            VCell::Symbol(s) => write!(f, "sym\"{}\"", *s),
            VCell::Undefined => write!(f, "undefined"),
            VCell::Void => write!(f, "#<void>"),
        }
    }
}

impl AsRef<VCell> for VCell {
    fn as_ref(&self) -> &VCell {
        self
    }
}

/// Lambda
///
/// Lambda represents a unit of executable bytecode constructed
/// by the compiler with an entry point of bc[0].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lambda {
    pub args: Vec<VCell>,
    pub bc: Vec<VCell>,
}

impl Lambda {
    /// New
    ///
    /// Create a new lambda with an empty bytecode vector
    /// and the given argument list.
    ///
    /// # Arguments
    /// `args` - A vector of VCell::Ptr, each guaranteed to
    ///          point to a symbol representing a formal argument.
    pub fn new(args: Vec<VCell>) -> Lambda {
        Lambda { args, bc: vec![] }
    }

    /// Get
    ///
    /// Get the opcode or operand at the given index
    pub fn get(&self, index: usize) -> Option<&VCell> {
        self.bc.get(index)
    }

    /// Emit
    ///
    /// Emit the byte code to the internal bc vector. This
    /// method is used during compilation when forming a lambda.
    pub fn emit<T: Into<VCell>>(&mut self, vcell: T) {
        self.bc.push(vcell.into());
    }

    /// Binding
    ///
    /// Return the binding for the given symbol
    pub fn binding(&self, sym: &VCell) -> Binding {
        match self.args.iter().enumerate().find(|it| it.1 == sym) {
            Some((it, _)) => Binding::Argument(it),
            _ => Global,
        }
    }

    /// Argument Count
    ///
    /// Return the number of arguments
    pub fn argc(&self) -> usize {
        self.args.len()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Binding {
    Global,
    Argument(usize),
}

impl From<Vec<VCell>> for Lambda {
    fn from(bc: Vec<VCell>) -> Self {
        Lambda { args: vec![], bc }
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
    fn lambda_binding() {
        let lambda = Lambda::new(vec![VCell::ptr(100), VCell::ptr(200)]);
        assert_eq!(lambda.binding(&VCell::ptr(100)), Binding::Argument(0));
        assert_eq!(lambda.binding(&VCell::ptr(200)), Binding::Argument(1));
        assert_eq!(lambda.binding(&VCell::ptr(300)), Binding::Global);
    }
}
