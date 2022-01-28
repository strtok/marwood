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
        self.load_builtin("=", num_equal);
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
        self.load_builtin("equal?", equal);
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
    let argc = vm.stack.pop()?.as_fixed_num()? as usize;
    if argc < min || (max.is_some() && argc > max.unwrap()) {
        Err(InvalidNumArgs(proc.into()))
    } else {
        Ok(argc)
    }
}

fn car(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "car")?;
    Ok(match vm.heap.get(vm.stack.pop()?) {
        VCell::Pair(car, _) => VCell::ptr(car),
        arg => return Err(ExpectedPairButFound(vm.heap.get_as_cell(&arg).to_string())),
    })
}

fn cdr(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "cdr")?;
    Ok(match vm.heap.get(vm.stack.pop()?) {
        VCell::Pair(_, cdr) => VCell::ptr(cdr),
        arg => return Err(ExpectedPairButFound(vm.heap.get_as_cell(&arg).to_string())),
    })
}

fn cons(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "cons")?;
    let cdr = vm.stack.pop()?.as_ptr()?;
    let car = vm.stack.pop()?.as_ptr()?;
    Ok(VCell::Pair(car, cdr))
}

fn is_boolean(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "boolean?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_boolean().into())
}

fn is_char(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char?")?;
    let _ = vm.stack.pop()?;
    Ok(false.into())
}

fn is_list(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "list?")?;
    let mut rest = vm.heap.get(vm.stack.pop()?);
    loop {
        if !rest.is_pair() {
            return Ok(rest.is_nil().into());
        }
        rest = vm.heap.get(&rest.as_cdr()?);
    }
}

fn is_null(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "null?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_nil().into())
}

fn is_number(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "number?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_number().into())
}

fn is_pair(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "pair?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_pair().into())
}

fn is_port(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "port?")?;
    let _ = vm.stack.pop()?;
    Ok(false.into())
}

fn is_procedure(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "procedure?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_procedure().into())
}

fn is_string(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string?")?;
    let _ = vm.stack.pop()?;
    Ok(false.into())
}

fn is_symbol(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "symbol?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_symbol().into())
}

fn is_vector(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "vector?")?;
    let _ = vm.stack.pop()?;
    Ok(false.into())
}

fn eq(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "eq?")?;
    let left = vm.stack.pop()?.clone();
    let right = vm.stack.pop()?.clone();
    Ok(vm.eqv(&left, &right)?.into())
}

fn eqv(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "eqv?")?;
    let left = vm.stack.pop()?.clone();
    let right = vm.stack.pop()?.clone();
    Ok(vm.eqv(&left, &right)?.into())
}

fn equal(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "eqv?")?;
    let left = vm.stack.pop()?.clone();
    let right = vm.stack.pop()?.clone();
    Ok(vm.equal(&left, &right)?.into())
}

fn not(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "not")?;
    Ok(match vm.heap.get(vm.stack.pop()?) {
        VCell::Bool(val) => !val,
        _ => false,
    }
    .into())
}

fn num_equal(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "=")?;

    let val = match vm.heap.get(vm.stack.pop()?) {
        VCell::FixedNum(val) => val,
        _ => {
            vm.heap.put(false);
            return Ok(false.into());
        }
    };

    for _ in 0..argc - 1 {
        match vm.heap.get(vm.stack.pop()?) {
            VCell::FixedNum(next_val) if next_val == val => {
                continue;
            }
            _ => {
                return Ok(false.into());
            }
        };
    }
    Ok(true.into())
}

fn plus(vm: &mut Vm) -> Result<VCell, Error> {
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
    Ok(VCell::FixedNum(sum))
}

fn minus(vm: &mut Vm) -> Result<VCell, Error> {
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

    Ok(VCell::FixedNum(result))
}

fn multiply(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 0, None, "*")?;
    let mut result = 1_i64;
    for _ in 0..argc {
        result *= match vm.heap.get(vm.stack.pop()?) {
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
    Ok(VCell::FixedNum(result))
}
