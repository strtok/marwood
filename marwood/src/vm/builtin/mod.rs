use crate::number::Number;
use crate::vm::vcell::VCell;
use crate::vm::vector::Vector;
use crate::vm::Error::{InvalidNumArgs, InvalidSyntax};
use crate::vm::{Error, Vm};
use std::rc::Rc;

mod list;
mod number;
mod ports;
mod predicate;
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
        self.load_builtin("*", number::multiply);
        self.load_builtin("/", number::divide);
        self.load_builtin("+", number::plus);
        self.load_builtin("-", number::minus);
        self.load_builtin("<", number::lt);
        self.load_builtin("<=", number::lteq);
        self.load_builtin("=", number::num_equal);
        self.load_builtin(">", number::gt);
        self.load_builtin(">=", number::gteq);
        self.load_builtin("%", number::remainder);
        self.load_builtin("abs", number::abs);
        self.load_builtin("append", list::append);
        self.load_builtin("boolean?", predicate::is_boolean);
        self.load_builtin("car", list::car);
        self.load_builtin("cdr", list::cdr);
        self.load_builtin("char?", predicate::is_char);
        self.load_builtin("cons", list::cons);
        self.load_builtin("display", ports::display);
        self.load_builtin("eq?", predicate::eq);
        self.load_builtin("equal?", predicate::equal);
        self.load_builtin("eqv?", predicate::eqv);
        self.load_builtin("even?", number::even);
        self.load_builtin("exact->inexact", number::exact_inexact);
        self.load_builtin("inexact->exact", number::inexact_exact);
        self.load_builtin("make-vector", vector::make_vector);
        self.load_builtin("list?", predicate::is_list);
        self.load_builtin("list-ref", list::list_ref);
        self.load_builtin("list-tail", list::list_tail);
        self.load_builtin("negative?", number::negative);
        self.load_builtin("not", predicate::not);
        self.load_builtin("null?", predicate::is_null);
        self.load_builtin("number?", predicate::is_number);
        self.load_builtin("odd?", number::odd);
        self.load_builtin("pair?", predicate::is_pair);
        self.load_builtin("port?", predicate::is_port);
        self.load_builtin("positive?", number::positive);
        self.load_builtin("procedure?", predicate::is_procedure);
        self.load_builtin("quotient", number::quotient);
        self.load_builtin("remainder", number::remainder);
        self.load_builtin("reverse", list::reverse);
        self.load_builtin("set-car!", list::set_car);
        self.load_builtin("set-cdr!", list::set_cdr);
        self.load_builtin("string?", predicate::is_string);
        self.load_builtin("symbol?", predicate::is_symbol);
        self.load_builtin("vector", vector::vector);
        self.load_builtin("vector-length", vector::vector_length);
        self.load_builtin("vector->list", vector::vector_to_list);
        self.load_builtin("list->vector", vector::list_to_vector);
        self.load_builtin("vector-ref", vector::vector_ref);
        self.load_builtin("vector-set!", vector::vector_set);
        self.load_builtin("vector-fill!", vector::vector_fill);
        self.load_builtin("vector?", predicate::is_vector);
        self.load_builtin("zero?", number::zero);
    }

    fn load_builtin(&mut self, symbol: &str, func: fn(&mut Vm) -> Result<VCell, Error>) {
        let syscall = self.heap.put(VCell::syscall(func));
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
