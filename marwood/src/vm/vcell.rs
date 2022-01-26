use crate::vm::environment::LexicalEnvironment;
use crate::vm::lambda::Lambda;
use crate::vm::opcode::OpCode;
use crate::vm::transform::Transform;
use crate::vm::Error::ExpectedType;
use crate::vm::{Error, Vm};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

/// VCell
///
/// VCell is a 24 byte (3x64bit) type that's used to represent values
/// in the VM runtime.
///
/// * Primitive list values (bool, fixednum, pairs, nil, etc)
/// * Opcodes
/// * References into the heap (Ptr)
/// * Global and lexical environment slot references (EnvSlot)
/// * Other runtime sentinel values, such as Undefined and Void.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VCell {
    Acc,
    BasePointer(usize),
    BasePointerOffset(i64),
    Bool(bool),
    BuiltInProc(BuiltInProc),
    Closure(usize, usize),
    GlobalEnvSlot(usize),
    LexicalEnv(Rc<LexicalEnvironment>),
    LexicalEnvSlot(usize),
    LexicalEnvPtr(usize, usize),
    FixedNum(i64),
    InstructionPointer(usize, usize),
    Macro(Rc<Transform>),
    Lambda(Rc<Lambda>),
    Nil,
    OpCode(OpCode),
    Pair(usize, usize),
    Ptr(usize),
    Symbol(Rc<String>),
    Undefined,
    Void,
}

#[derive(Clone)]
pub struct BuiltInProc(pub fn(&mut Vm) -> Result<(), Error>);

impl Debug for BuiltInProc {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "VCell::SysCall")
    }
}

impl PartialEq<Self> for BuiltInProc {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0 as *mut fn(&mut Vm), other.0 as *mut fn(&mut Vm))
    }
}

impl Eq for BuiltInProc {}

pub const ACC_TYPE_TEXT: &str = "#<acc>";
pub const BASE_POINTER_TYPE_TEXT: &str = "#<base-pointer>";
pub const BASE_POINTER_OFFSET_TYPE_TEXT: &str = "#<base-pointer-offset>";
pub const BOOL_TYPE_TEXT: &str = "#<bool>";
pub const CLOSURE_TYPE_TEXT: &str = "#<closure>";
pub const GLOBAL_ENV_SLOT_TYPE_TEXT: &str = "#<global-environment-slot>";
pub const MACRO_TYPE_TEXT: &str = "#<macro>";
pub const LEXICAL_ENV_TYPE_TEXT: &str = "#<lexical-environment>";
pub const LEXICAL_ENV_TYPE_SLOT: &str = "#<lexical-environment-slot>";
pub const LEXICAL_ENV_POINTER_TYPE_TEXT: &str = "#<lexical-environment-pointer>";
pub const FIXEDNUM_TYPE_TEXT: &str = "#<fixednum>";
pub const INSTRUCTION_POINTER_TYPE_TEXT: &str = "#<instruction-pointer>";
pub const LAMBDA_TYPE_TEXT: &str = "#<lambda>";
pub const NIL_TYPE_TEXT: &str = "#<nil>";
pub const OPCODE_TYPE_TEXT: &str = "#<opcode>";
pub const PAIR_TYPE_TEXT: &str = "#<pair>";
pub const PTR_TYPE_TEXT: &str = "#<ptr>";
pub const SYMBOL_TYPE_TEXT: &str = "#<symbol>";
pub const SYSCALL_TYPE_TEXT: &str = "#<syscall>";
pub const UNDEFINED_TYPE_TEXT: &str = "#<undefined>";
pub const VOID_TYPE_TEXT: &str = "#<void>";

impl VCell {
    /// TYPE_TEXT Text
    ///
    /// TYPE_TEXT text is only used during error paths to print
    /// the TYPE_TEXT of a VCell
    pub fn type_text(&self) -> &'static str {
        match self {
            VCell::Acc => ACC_TYPE_TEXT,
            VCell::BasePointer(_) => BASE_POINTER_TYPE_TEXT,
            VCell::BasePointerOffset(_) => BASE_POINTER_OFFSET_TYPE_TEXT,
            VCell::Bool(_) => BOOL_TYPE_TEXT,
            VCell::Closure(_, _) => CLOSURE_TYPE_TEXT,
            VCell::GlobalEnvSlot(_) => GLOBAL_ENV_SLOT_TYPE_TEXT,
            VCell::LexicalEnv(_) => LEXICAL_ENV_TYPE_TEXT,
            VCell::LexicalEnvSlot(_) => LEXICAL_ENV_TYPE_SLOT,
            VCell::LexicalEnvPtr(_, _) => LEXICAL_ENV_POINTER_TYPE_TEXT,
            VCell::FixedNum(_) => FIXEDNUM_TYPE_TEXT,
            VCell::InstructionPointer(_, _) => INSTRUCTION_POINTER_TYPE_TEXT,
            VCell::Lambda(_) => LAMBDA_TYPE_TEXT,
            VCell::Nil => NIL_TYPE_TEXT,
            VCell::OpCode(_) => OPCODE_TYPE_TEXT,
            VCell::Pair(_, _) => PAIR_TYPE_TEXT,
            VCell::Ptr(_) => PTR_TYPE_TEXT,
            VCell::Symbol(_) => SYMBOL_TYPE_TEXT,
            VCell::BuiltInProc(_) => SYSCALL_TYPE_TEXT,
            VCell::Macro(_) => MACRO_TYPE_TEXT,
            VCell::Undefined => UNDEFINED_TYPE_TEXT,
            VCell::Void => VOID_TYPE_TEXT,
        }
    }

    pub fn syscall(func: fn(&mut Vm) -> Result<(), Error>) -> VCell {
        VCell::BuiltInProc(BuiltInProc(func))
    }

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
        VCell::GlobalEnvSlot(slot.into())
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

    pub fn is_boolean(&self) -> bool {
        matches!(self, VCell::Bool(_))
    }

    pub fn is_fixednum(&self) -> bool {
        matches!(self, VCell::FixedNum(_))
    }

    pub fn is_number(&self) -> bool {
        self.is_fixednum()
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, VCell::Pair(_, _))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, VCell::Symbol(_))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, VCell::Ptr(_))
    }

    pub fn is_envslot(&self) -> bool {
        matches!(self, VCell::GlobalEnvSlot(_))
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

    pub fn is_builtin_proc(&self) -> bool {
        matches!(self, VCell::BuiltInProc(_))
    }

    pub fn is_procedure(&self) -> bool {
        self.is_lambda() || self.is_closure() || self.is_builtin_proc()
    }

    pub fn is_lexical_env(&self) -> bool {
        matches!(self, VCell::LexicalEnv(_))
    }

    pub fn is_macro(&self) -> bool {
        matches!(self, VCell::Macro(_))
    }

    pub fn as_opcode(&self) -> Result<OpCode, Error> {
        match self {
            VCell::OpCode(op) => Ok(op.clone()),
            _ => Err(ExpectedType(OPCODE_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_car(&self) -> Result<VCell, Error> {
        match self {
            VCell::Pair(car, _) => Ok(VCell::Ptr(*car)),
            _ => Err(ExpectedType(PAIR_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_cdr(&self) -> Result<VCell, Error> {
        match self {
            VCell::Pair(_, cdr) => Ok(VCell::Ptr(*cdr)),
            _ => Err(ExpectedType(PAIR_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_fixed_num(&self) -> Result<i64, Error> {
        match self {
            VCell::FixedNum(val) => Ok(*val),
            _ => Err(ExpectedType(FIXEDNUM_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_symbol(&self) -> Result<&str, Error> {
        match self {
            VCell::Symbol(s) => Ok(&*s),
            _ => Err(ExpectedType(SYMBOL_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_lambda(&self) -> Result<&Lambda, Error> {
        match self {
            VCell::Lambda(lambda) => Ok(&*lambda),
            _ => Err(ExpectedType(LAMBDA_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_macro(&self) -> Result<&Transform, Error> {
        match self {
            VCell::Macro(transform) => Ok(&*transform),
            _ => Err(ExpectedType(MACRO_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_ptr(&self) -> Result<usize, Error> {
        match self {
            VCell::Ptr(ptr) => Ok(*ptr),
            _ => Err(ExpectedType(PTR_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_env_slot(&self) -> Result<usize, Error> {
        match self {
            VCell::GlobalEnvSlot(slot) => Ok(*slot),
            _ => Err(ExpectedType(LEXICAL_ENV_TYPE_SLOT, self.type_text())),
        }
    }

    pub fn as_lexical_env(&self) -> Result<&LexicalEnvironment, Error> {
        match self {
            VCell::LexicalEnv(env) => Ok(&*env),
            _ => Err(ExpectedType(LEXICAL_ENV_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_ip(&self) -> Result<(usize, usize), Error> {
        match self {
            VCell::InstructionPointer(lambda, ip) => Ok((*lambda, *ip)),
            _ => Err(ExpectedType(
                INSTRUCTION_POINTER_TYPE_TEXT,
                self.type_text(),
            )),
        }
    }

    pub fn as_bp(&self) -> Result<usize, Error> {
        match self {
            VCell::BasePointer(bp) => Ok(*bp),
            _ => Err(ExpectedType(BASE_POINTER_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_bp_offset(&self) -> Result<i64, Error> {
        match self {
            VCell::BasePointerOffset(offset) => Ok(*offset),
            _ => Err(ExpectedType(
                BASE_POINTER_OFFSET_TYPE_TEXT,
                self.type_text(),
            )),
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
            VCell::BasePointer(bp) => write!(f, "%bp[${:02x}]", bp),
            VCell::BasePointerOffset(offset) => write!(f, "%bp[{:+}]", *offset),
            VCell::Bool(true) => write!(f, "#t"),
            VCell::Bool(false) => write!(f, "#f"),
            VCell::Closure(_, _) => write!(f, "#<closure>"),
            VCell::GlobalEnvSlot(slot) => write!(f, "genv[${:02x}]", slot),
            VCell::FixedNum(val) => write!(f, "{}", val),
            VCell::InstructionPointer(lambda, ip) => {
                write!(f, "%ip[${:02x}][${:02x}]", *lambda, *ip)
            }
            VCell::Macro(_) => write!(f, "#<macro>"),
            VCell::LexicalEnv(_) => write!(f, "#<lexical-environment>"),
            VCell::LexicalEnvSlot(slot) => write!(f, "env[${:02x}]", slot),
            VCell::LexicalEnvPtr(env, slot) => write!(f, "env[${:02x}][${:02x}]", env, slot),
            VCell::Lambda(_) => write!(f, "#<lambda>"),
            VCell::Nil => write!(f, "()"),
            VCell::OpCode(val) => write!(f, "{:?}", val),
            VCell::Pair(car, cdr) => write!(f, "(${:02x} . ${:02x})", car, cdr),
            VCell::Ptr(ptr) => write!(f, "${:02x}", ptr),
            VCell::Symbol(s) => write!(f, "{}", *s),
            VCell::BuiltInProc(_) => write!(f, "#<syscall>"),
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
