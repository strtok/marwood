use crate::number::Number;
use crate::vm::vcell::VCell;
use crate::vm::vector::Vector;
use crate::vm::Error::{InvalidNumArgs, InvalidSyntax};
use crate::vm::{Error, Vm};
use std::cell::RefCell;
use std::rc::Rc;

mod char;
mod list;
mod number;
mod ports;
mod predicate;
mod procedure;
mod rand;
mod string;
mod symbol;
mod vector;

/// Built Ins
///
/// Built ins are implementations for scheme primitive or library procedures
/// that either must be written in rust (e.g. type predicates), or are implemented
/// in rust for performance reasons.
///
/// Like normal procedures, these procdues are the target of a CALL or TCALL
/// instruction where the procedure being applied is a VCell::BuiltIn object
/// instead of a Lambda or Closure.
///
/// The CALL and TCALL instruction will bypass normal frame setup and pass
/// control to the procedure in this file.
///
/// Because a call frame was not setup, the top of the stack for a built in
/// is argc, and immediately preceding argc are the arguments being applied.
/// All builtins must pop the argc value along with argc* arguments off
/// the stack before returning.

impl Vm {
    pub fn load_builtins(&mut self) {
        char::load_builtins(self);
        list::load_builtins(self);
        number::load_builtins(self);
        ports::load_builtins(self);
        predicate::load_builtins(self);
        procedure::load_builtins(self);
        rand::load_builtins(self);
        string::load_builtins(self);
        symbol::load_builtins(self);
        vector::load_builtins(self);
    }

    pub fn load_builtin(
        &mut self,
        symbol: &'static str,
        func: fn(&mut Vm) -> Result<VCell, Error>,
    ) {
        let syscall = self.heap.put(VCell::builtin(symbol, func));
        let symbol = self.heap.put(VCell::symbol(symbol));
        let slot = self.globenv.get_binding(symbol.as_ptr().unwrap());
        self.globenv.put_slot(slot, syscall);
    }
}

/// Pop Argc
///
/// Pop the number of arguments applied to a procedure off the top of
/// the stack. Return an error if they don't match the expected.
fn pop_argc(vm: &mut Vm, min: usize, max: Option<usize>, proc: &str) -> Result<usize, Error> {
    let argc = vm.stack.pop()?.as_argc()?;
    if argc < min || (max.is_some() && argc > max.unwrap()) {
        Err(InvalidNumArgs(proc.into()))
    } else {
        Ok(argc)
    }
}

///
/// Numerical Procedures
///
///

fn pop_number(vm: &mut Vm) -> Result<Number, Error> {
    match vm.heap.get(vm.stack.pop()?) {
        VCell::Number(num) => Ok(num),
        vcell => {
            return Err(InvalidSyntax(format!(
                "{} is not a valid number",
                vm.heap.get_as_cell(&vcell)
            )))
        }
    }
}

fn pop_integer(vm: &mut Vm) -> Result<Number, Error> {
    match pop_number(vm) {
        Ok(num) if num.is_integer() => Ok(num),
        Ok(num) => return Err(InvalidSyntax(format!("{} is not a valid integer", num))),
        Err(e) => Err(e),
    }
}

fn pop_usize(vm: &mut Vm) -> Result<usize, Error> {
    match pop_number(vm) {
        Ok(num) if num.is_integer() && num >= Number::from(0) && num.to_usize().is_some() => {
            Ok(num.to_usize().unwrap())
        }
        Ok(num) => return Err(InvalidSyntax(format!("{} is not a valid size", num))),
        Err(e) => Err(e),
    }
}

fn pop_char(vm: &mut Vm) -> Result<char, Error> {
    match vm.heap.get(vm.stack.pop()?) {
        VCell::Char(c) => Ok(c),
        vcell => {
            return Err(InvalidSyntax(format!(
                "{} is not a valid character",
                vm.heap.get_as_cell(&vcell)
            )))
        }
    }
}

fn pop_string(vm: &mut Vm, proc: &str) -> Result<Rc<RefCell<String>>, Error> {
    match vm.heap.get(vm.stack.pop()?) {
        VCell::String(s) => Ok(s),
        vcell => {
            return Err(InvalidSyntax(format!(
                "bad argument to {}: {} is not a string",
                proc,
                vm.heap.get_as_cell(&vcell)
            )))
        }
    }
}

fn pop_symbol(vm: &mut Vm, proc: &str) -> Result<Rc<String>, Error> {
    match vm.heap.get(vm.stack.pop()?) {
        VCell::Symbol(s) => Ok(s),
        vcell => {
            return Err(InvalidSyntax(format!(
                "bad argument to {}: {} is not a symbol",
                proc,
                vm.heap.get_as_cell(&vcell)
            )))
        }
    }
}

fn pop_index(vm: &mut Vm) -> Result<usize, Error> {
    match vm.heap.get(vm.stack.pop()?) {
        VCell::Number(num) => num
            .to_usize()
            .ok_or_else(|| InvalidSyntax(format!("{} is not a valid index", num))),
        vcell => {
            return Err(InvalidSyntax(format!(
                "{} is not a valid index",
                vm.heap.get_as_cell(&vcell)
            )))
        }
    }
}

fn pop_vector(vm: &mut Vm) -> Result<Rc<Vector>, Error> {
    match vm.heap.get(vm.stack.pop()?) {
        VCell::Vector(vector) => Ok(vector),
        vcell => {
            return Err(InvalidSyntax(format!(
                "{} is not a vector",
                vm.heap.get_as_cell(&vcell)
            )))
        }
    }
}
