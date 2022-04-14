use crate::char::write_escaped_char;
use crate::error::Error;
use crate::error::Error::ExpectedType;
use crate::number::Number;
use crate::vm::continuation::Continuation;
use crate::vm::environment::LexicalEnvironment;
use crate::vm::lambda::Lambda;
use crate::vm::opcode::OpCode;
use crate::vm::transform::Transform;
use crate::vm::vector::Vector;
use crate::vm::Vm;
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
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
    // Scheme primitive types
    Bool(bool),
    Char(char),
    Nil,
    Number(Number),
    Pair(usize, usize),
    Symbol(Rc<String>),
    String(Rc<RefCell<String>>),
    Vector(Rc<Vector>),

    // other scheme values
    Undefined,
    Void,

    // lambda, closure and lexical environments
    Continuation(Rc<Continuation>),
    Closure(usize, usize),
    Lambda(Rc<Lambda>),
    LexicalEnv(Rc<LexicalEnvironment>),
    LexicalEnvSlot(usize),
    LexicalEnvPtr(usize, usize),
    Macro(Rc<Transform>),

    // VM registers and stack values
    Acc,
    ArgumentCount(usize),
    BasePointer(usize),
    BasePointerOffset(i64),
    BuiltInProc(Rc<BuiltInProc>),
    EnvironmentPointer(usize),
    GlobalEnvSlot(usize),
    InstructionPointer(usize, usize),
    OpCode(OpCode),
    Ptr(usize),
}

#[derive(Clone)]
pub struct BuiltInProc {
    desc: &'static str,
    proc: fn(&mut Vm) -> Result<VCell, Error>,
}

impl BuiltInProc {
    pub fn eval(&self, vm: &mut Vm) -> Result<VCell, Error> {
        (self.proc)(vm)
    }

    pub fn desc(&self) -> &'static str {
        self.desc
    }
}

impl Debug for BuiltInProc {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "#<builtin:{}>", self.desc())
    }
}

impl PartialEq<Self> for BuiltInProc {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(
            self.proc as *mut fn(&mut Vm),
            other.proc as *mut fn(&mut Vm),
        ) && self.desc().eq(other.desc())
    }
}

impl Eq for BuiltInProc {}

pub const ACC_TYPE_TEXT: &str = "#<acc>";
pub const ARGUMENT_COUNT_TYPE_TEXT: &str = "#<argument-count>";
pub const BASE_POINTER_TYPE_TEXT: &str = "#<base-pointer>";
pub const BASE_POINTER_OFFSET_TYPE_TEXT: &str = "#<base-pointer-offset>";
pub const BOOL_TYPE_TEXT: &str = "#<bool>";
pub const CHAR_TYPE_TEXT: &str = "#<char>";
pub const CLOSURE_TYPE_TEXT: &str = "#<closure>";
pub const CONTINUATION_TYPE_TEXT: &str = "#<continuation>";
pub const GLOBAL_ENV_SLOT_TYPE_TEXT: &str = "#<global-environment-slot>";
pub const ENVIRONMENT_POINTER_TYPE_TEXT: &str = "#<environment-pointer>";
pub const MACRO_TYPE_TEXT: &str = "#<macro>";
pub const LEXICAL_ENV_TYPE_TEXT: &str = "#<lexical-environment>";
pub const LEXICAL_ENV_TYPE_SLOT: &str = "#<lexical-environment-slot>";
pub const LEXICAL_ENV_POINTER_TYPE_TEXT: &str = "#<lexical-environment-pointer>";
pub const FIXEDNUM_TYPE_TEXT: &str = "#<fixednum>";
pub const INSTRUCTION_POINTER_TYPE_TEXT: &str = "#<instruction-pointer>";
pub const NUMBER_TYPE_TEXT: &str = "#<number>";
pub const LAMBDA_TYPE_TEXT: &str = "#<lambda>";
pub const NIL_TYPE_TEXT: &str = "#<nil>";
pub const OPCODE_TYPE_TEXT: &str = "#<opcode>";
pub const PAIR_TYPE_TEXT: &str = "#<pair>";
pub const PTR_TYPE_TEXT: &str = "#<ptr>";
pub const STRING_TYPE_TEXT: &str = "#<string>";
pub const SYMBOL_TYPE_TEXT: &str = "#<symbol>";
pub const SYSCALL_TYPE_TEXT: &str = "#<syscall>";
pub const UNDEFINED_TYPE_TEXT: &str = "#<undefined>";
pub const VECTOR_TYPE_TEXT: &str = "#<vector>";
pub const VOID_TYPE_TEXT: &str = "#<void>";

impl VCell {
    /// TYPE_TEXT Text
    ///
    /// TYPE_TEXT text is only used during error paths to print
    /// the TYPE_TEXT of a VCell
    pub fn type_text(&self) -> &'static str {
        match self {
            VCell::Acc => ACC_TYPE_TEXT,
            VCell::ArgumentCount(_) => ARGUMENT_COUNT_TYPE_TEXT,
            VCell::BasePointer(_) => BASE_POINTER_TYPE_TEXT,
            VCell::BasePointerOffset(_) => BASE_POINTER_OFFSET_TYPE_TEXT,
            VCell::Bool(_) => BOOL_TYPE_TEXT,
            VCell::Char(_) => CHAR_TYPE_TEXT,
            VCell::Continuation(_) => CONTINUATION_TYPE_TEXT,
            VCell::Closure(_, _) => CLOSURE_TYPE_TEXT,
            VCell::EnvironmentPointer(_) => ENVIRONMENT_POINTER_TYPE_TEXT,
            VCell::GlobalEnvSlot(_) => GLOBAL_ENV_SLOT_TYPE_TEXT,
            VCell::LexicalEnv(_) => LEXICAL_ENV_TYPE_TEXT,
            VCell::LexicalEnvSlot(_) => LEXICAL_ENV_TYPE_SLOT,
            VCell::LexicalEnvPtr(_, _) => LEXICAL_ENV_POINTER_TYPE_TEXT,
            VCell::InstructionPointer(_, _) => INSTRUCTION_POINTER_TYPE_TEXT,
            VCell::Lambda(_) => LAMBDA_TYPE_TEXT,
            VCell::Nil => NIL_TYPE_TEXT,
            VCell::Number(_) => NUMBER_TYPE_TEXT,
            VCell::OpCode(_) => OPCODE_TYPE_TEXT,
            VCell::Pair(_, _) => PAIR_TYPE_TEXT,
            VCell::Ptr(_) => PTR_TYPE_TEXT,
            VCell::String(_) => STRING_TYPE_TEXT,
            VCell::Symbol(_) => SYMBOL_TYPE_TEXT,
            VCell::BuiltInProc(_) => SYSCALL_TYPE_TEXT,
            VCell::Macro(_) => MACRO_TYPE_TEXT,
            VCell::Undefined => UNDEFINED_TYPE_TEXT,
            VCell::Vector(_) => VECTOR_TYPE_TEXT,
            VCell::Void => VOID_TYPE_TEXT,
        }
    }

    pub fn number<T: Into<Number>>(num: T) -> VCell {
        VCell::Number(num.into())
    }

    pub fn builtin(desc: &'static str, proc: fn(&mut Vm) -> Result<VCell, Error>) -> VCell {
        VCell::BuiltInProc(Rc::new(BuiltInProc { desc, proc }))
    }

    pub fn undefined() -> VCell {
        VCell::Undefined
    }

    pub fn void() -> VCell {
        VCell::Void
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

    pub fn string<T: Into<String>>(s: T) -> VCell {
        VCell::String(Rc::new(RefCell::new(s.into())))
    }

    pub fn symbol<T: Into<String>>(sym: T) -> VCell {
        VCell::Symbol(Rc::new(sym.into()))
    }

    pub fn vector<T: Into<Vec<VCell>>>(vector: T) -> VCell {
        VCell::Vector(Rc::new(Vector::new(vector.into())))
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

    pub fn is_number(&self) -> bool {
        matches!(self, VCell::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, VCell::String(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, VCell::Char(_))
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

    pub fn is_continuation(&self) -> bool {
        matches!(self, VCell::Continuation(_))
    }

    pub fn is_builtin_proc(&self) -> bool {
        matches!(self, VCell::BuiltInProc(_))
    }

    pub fn is_procedure(&self) -> bool {
        self.is_lambda() || self.is_closure() || self.is_builtin_proc() || self.is_continuation()
    }

    pub fn is_lexical_env(&self) -> bool {
        matches!(self, VCell::LexicalEnv(_))
    }

    pub fn is_macro(&self) -> bool {
        matches!(self, VCell::Macro(_))
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, VCell::Vector(_))
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

    pub fn as_symbol(&self) -> Result<&str, Error> {
        match self {
            VCell::Symbol(s) => Ok(&*s),
            _ => Err(ExpectedType(SYMBOL_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_string(&self) -> Result<&RefCell<String>, Error> {
        match self {
            VCell::String(s) => Ok(&*s),
            _ => Err(ExpectedType(SYMBOL_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_char(&self) -> Result<char, Error> {
        match self {
            VCell::Char(c) => Ok(*c),
            _ => Err(ExpectedType(CHAR_TYPE_TEXT, self.type_text())),
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

    pub fn as_vector(&self) -> Result<&Vector, Error> {
        match self {
            VCell::Vector(vector) => Ok(&*vector),
            _ => Err(ExpectedType(VECTOR_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_argc(&self) -> Result<usize, Error> {
        match self {
            VCell::ArgumentCount(bp) => Ok(*bp),
            _ => Err(ExpectedType(ARGUMENT_COUNT_TYPE_TEXT, self.type_text())),
        }
    }

    pub fn as_number(&self) -> Result<&Number, Error> {
        match self {
            VCell::Number(num) => Ok(num),
            _ => Err(ExpectedType(NUMBER_TYPE_TEXT, self.type_text())),
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

    pub fn as_ep(&self) -> Result<usize, Error> {
        match self {
            VCell::EnvironmentPointer(bp) => Ok(*bp),
            _ => Err(ExpectedType(
                ENVIRONMENT_POINTER_TYPE_TEXT,
                self.type_text(),
            )),
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

impl From<char> for VCell {
    fn from(val: char) -> Self {
        VCell::Char(val)
    }
}

impl From<i64> for VCell {
    fn from(num: i64) -> Self {
        VCell::Number(Number::from(num))
    }
}

impl From<i32> for VCell {
    fn from(num: i32) -> Self {
        VCell::Number(Number::from(num))
    }
}

impl From<Number> for VCell {
    fn from(num: Number) -> Self {
        VCell::Number(num)
    }
}

impl fmt::Display for VCell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VCell::Acc => write!(f, "%acc"),
            VCell::ArgumentCount(argc) => write!(f, "%argc[{}]", argc),
            VCell::BasePointer(bp) => write!(f, "%bp[${:02x}]", bp),
            VCell::BasePointerOffset(offset) => write!(f, "%bp[{:+}]", *offset),
            VCell::Bool(true) => write!(f, "#t"),
            VCell::Bool(false) => write!(f, "#f"),
            VCell::Char(c) => write_escaped_char(*c, f),
            VCell::Closure(_, _) => write!(f, "#<closure>"),
            VCell::Continuation(_) => write!(f, "#<continuation>"),
            VCell::EnvironmentPointer(ep) => write!(f, "%ep[${:02x}]", ep),
            VCell::GlobalEnvSlot(slot) => write!(f, "genv[${:02x}]", slot),
            VCell::InstructionPointer(lambda, ip) => {
                write!(f, "%ip[${:02x}][${:02x}]", *lambda, *ip)
            }
            VCell::Macro(_) => write!(f, "#<macro>"),
            VCell::LexicalEnv(_) => write!(f, "#<lexical-environment>"),
            VCell::LexicalEnvSlot(slot) => write!(f, "env[${:02x}]", slot),
            VCell::LexicalEnvPtr(env, slot) => write!(f, "env[${:02x}][${:02x}]", env, slot),
            VCell::Lambda(lambda) => write!(f, "#<lambda:{}>", lambda),
            VCell::Nil => write!(f, "()"),
            VCell::Number(number) => write!(f, "{:?}", number),
            VCell::OpCode(val) => write!(f, "{:?}", val),
            VCell::Pair(car, cdr) => write!(f, "(${:02x} . ${:02x})", car, cdr),
            VCell::Ptr(ptr) => write!(f, "${:02x}", ptr),
            VCell::String(s) => write!(f, "\"{}\"", (**s).borrow().deref()),
            VCell::Symbol(s) => write!(f, "{}", *s),
            VCell::BuiltInProc(proc) => write!(f, "#<builtin:{}>", proc.desc()),
            VCell::Undefined => write!(f, "undefined"),
            VCell::Vector(_) => write!(f, "#<vector>"),
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
