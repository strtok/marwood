use crate::vm::vcell::VCell;
use crate::vm::Error::{ExpectedPairButFound, InvalidArgs, InvalidNumArgs};
use crate::vm::{Error, Vm};

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
        self.load_builtin("+", plus);
        self.load_builtin("-", minus);
        self.load_builtin("*", multiply);
        self.load_builtin("boolean?", is_boolean);
        self.load_builtin("car", car);
        self.load_builtin("cdr", cdr);
        self.load_builtin("char?", is_char);
        self.load_builtin("cons", cons);
        self.load_builtin("eq?", eq);
        self.load_builtin("eqv?", eqv);
        self.load_builtin("list?", is_list);
        self.load_builtin("not", not);
        self.load_builtin("null?", is_null);
        self.load_builtin("number?", is_number);
        self.load_builtin("pair?", is_pair);
        self.load_builtin("port?", is_port);
        self.load_builtin("procedure?", is_procedure);
        self.load_builtin("string?", is_string);
        self.load_builtin("symbol?", is_symbol);
        self.load_builtin("vector?", is_vector);
    }

    fn load_builtin(&mut self, symbol: &str, func: fn(&mut Vm) -> Result<(), Error>) {
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
    let argc = vm.stack.pop()?.as_fixed_num()? as usize;
    if argc < min || (max.is_some() && argc > max.unwrap()) {
        Err(InvalidNumArgs(proc.into()))
    } else {
        Ok(argc)
    }
}

fn car(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "car")?;
    let arg = vm.heap.get(vm.stack.pop()?);
    match arg {
        VCell::Pair(car, _) => vm.acc = VCell::ptr(car),
        _ => return Err(ExpectedPairButFound(vm.heap.get_as_cell(&arg).to_string())),
    }
    Ok(())
}

fn cdr(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "cdr")?;
    let arg = vm.heap.get(vm.stack.pop()?);
    match arg {
        VCell::Pair(_, cdr) => vm.acc = VCell::ptr(cdr),
        _ => return Err(ExpectedPairButFound(vm.heap.get_as_cell(&arg).to_string())),
    }
    Ok(())
}

fn cons(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 2, Some(2), "cons")?;
    let cdr = vm.stack.pop()?.as_ptr()?;
    let car = vm.stack.pop()?.as_ptr()?;
    let pair = VCell::Pair(car, cdr);
    vm.acc = vm.heap.put(pair);
    Ok(())
}

fn is_boolean(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "boolean?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    vm.acc = vm.heap.put(result.is_boolean());
    Ok(())
}

fn is_char(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "char?")?;
    let _ = vm.stack.pop()?;
    vm.acc = vm.heap.put(false);
    Ok(())
}

fn is_list(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "list?")?;
    let mut rest = vm.heap.get(vm.stack.pop()?);
    loop {
        if !rest.is_pair() {
            vm.acc = vm.heap.put(rest.is_nil());
            break;
        }
        rest = vm.heap.get(&rest.as_cdr()?);
    }
    Ok(())
}

fn is_null(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "null?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    vm.acc = vm.heap.put(result.is_nil());
    Ok(())
}

fn is_number(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "number?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    vm.acc = vm.heap.put(result.is_number());
    Ok(())
}

fn is_pair(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "pair?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    vm.acc = vm.heap.put(result.is_pair());
    Ok(())
}

fn is_port(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "port?")?;
    let _ = vm.stack.pop()?;
    vm.acc = vm.heap.put(false);
    Ok(())
}

fn is_procedure(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "procedure?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    vm.acc = vm.heap.put(result.is_procedure());
    Ok(())
}

fn is_string(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "string?")?;
    let _ = vm.stack.pop()?;
    vm.acc = vm.heap.put(false);
    Ok(())
}

fn is_symbol(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "symbol?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    vm.acc = vm.heap.put(result.is_symbol());
    Ok(())
}

fn is_vector(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "vector?")?;
    let _ = vm.stack.pop()?;
    vm.acc = vm.heap.put(false);
    Ok(())
}

fn eq(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 2, Some(2), "eq?")?;
    let left = vm.stack.pop()?.clone();
    let right = vm.stack.pop()?.clone();
    vm.acc = vm.heap.put(vm.eqv(&left, &right)?);
    Ok(())
}

fn eqv(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 2, Some(2), "eqv?")?;
    let left = vm.stack.pop()?.clone();
    let right = vm.stack.pop()?.clone();
    vm.acc = vm.heap.put(vm.eqv(&left, &right)?);
    Ok(())
}

fn not(vm: &mut Vm) -> Result<(), Error> {
    pop_argc(vm, 1, Some(1), "not")?;
    vm.acc = match vm.heap.get(vm.stack.pop()?) {
        VCell::Bool(val) => vm.heap.put(!val),
        _ => vm.heap.put(false),
    };
    Ok(())
}

fn plus(vm: &mut Vm) -> Result<(), Error> {
    let argc = pop_argc(vm, 0, None, "+")?;
    let mut sum = 0_i64;
    for _ in 0..argc {
        sum += match vm.heap.get(vm.stack.pop()?) {
            VCell::FixedNum(n) => n,
            vcell => {
                return Err(InvalidArgs(
                    "+".to_string(),
                    "number".to_string(),
                    vm.heap.get_as_cell(&vcell).to_string(),
                ));
            }
        }
    }
    vm.acc = vm.heap.put(VCell::FixedNum(sum));
    Ok(())
}

fn minus(vm: &mut Vm) -> Result<(), Error> {
    let argc = pop_argc(vm, 1, None, "-")?;
    let mut result = 0_i64;
    for _ in 0..(argc - 1) {
        result += match vm.heap.get(vm.stack.pop()?) {
            VCell::FixedNum(n) => n,
            vcell => {
                return Err(InvalidArgs(
                    "+".to_string(),
                    "number".to_string(),
                    vm.heap.get_as_cell(&vcell).to_string(),
                ));
            }
        }
    }

    if let VCell::FixedNum(n) = vm.heap.get(vm.stack.pop()?) {
        result = n - result;
    }

    if argc == 1 {
        result *= -1;
    }

    vm.acc = vm.heap.put(VCell::FixedNum(result));
    Ok(())
}

fn multiply(vm: &mut Vm) -> Result<(), Error> {
    let argc = pop_argc(vm, 0, None, "*")?;
    let mut sum = 1_i64;
    for _ in 0..argc {
        sum *= match vm.heap.get(vm.stack.pop()?) {
            VCell::FixedNum(n) => n,
            vcell => {
                return Err(InvalidArgs(
                    "+".to_string(),
                    "number".to_string(),
                    vm.heap.get_as_cell(&vcell).to_string(),
                ));
            }
        }
    }
    vm.acc = vm.heap.put(VCell::FixedNum(sum));
    Ok(())
}
