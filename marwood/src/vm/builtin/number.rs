use crate::number::Number;
use crate::vm::vcell::VCell;
use crate::vm::Error::{InvalidArgs, InvalidSyntax};
use crate::vm::{builtin, Error, Vm};

pub fn num_equal(vm: &mut Vm) -> Result<VCell, Error> {
    num_comp(vm, "=", |x, y| x == y)
}

pub fn lt(vm: &mut Vm) -> Result<VCell, Error> {
    num_comp(vm, "<", |x, y| x < y)
}

pub fn gt(vm: &mut Vm) -> Result<VCell, Error> {
    num_comp(vm, ">", |x, y| x > y)
}

pub fn lteq(vm: &mut Vm) -> Result<VCell, Error> {
    num_comp(vm, "<=", |x, y| x <= y)
}

pub fn gteq(vm: &mut Vm) -> Result<VCell, Error> {
    num_comp(vm, ">=", |x, y| x >= y)
}

fn num_comp(
    vm: &mut Vm,
    name: &str,
    comp: impl Fn(&Number, &Number) -> bool,
) -> Result<VCell, Error> {
    let argc = builtin::pop_argc(vm, 1, None, name)?;
    let mut result = true;

    let mut y = match vm.heap.get(vm.stack.pop()?) {
        VCell::Number(val) => val,
        _ => {
            result = false;
            Number::from(0)
        }
    };

    for _ in 0..argc - 1 {
        match vm.heap.get(vm.stack.pop()?) {
            VCell::Number(x) if comp(&x, &y) => {
                y = x;
                continue;
            }
            _ => {
                result = false;
            }
        };
    }

    Ok(result.into())
}

pub fn zero(vm: &mut Vm) -> Result<VCell, Error> {
    num_unary_predicate(vm, "zero?", |x| x == &Number::from(0))
}

pub fn positive(vm: &mut Vm) -> Result<VCell, Error> {
    num_unary_predicate(vm, "positive?", |x| x > &Number::from(0))
}

pub fn negative(vm: &mut Vm) -> Result<VCell, Error> {
    num_unary_predicate(vm, "negative?", |x| x < &Number::from(0))
}

pub fn odd(vm: &mut Vm) -> Result<VCell, Error> {
    num_unary_predicate(vm, "odd?", |x| {
        x % &Number::from(2) != Some(Number::from(0))
    })
}

pub fn even(vm: &mut Vm) -> Result<VCell, Error> {
    num_unary_predicate(vm, "even?", |x| {
        x % &Number::from(2) == Some(Number::from(0))
    })
}

fn num_unary_predicate(
    vm: &mut Vm,
    name: &str,
    predicate: impl Fn(&Number) -> bool,
) -> Result<VCell, Error> {
    builtin::pop_argc(vm, 1, Some(1), name)?;
    let x = match vm.heap.get(vm.stack.pop()?) {
        VCell::Number(val) => val,
        _ => return Ok(false.into()),
    };
    Ok(predicate(&x).into())
}

pub fn plus(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = builtin::pop_argc(vm, 0, None, "+")?;
    let mut sum = Number::from(0);
    for _ in 0..argc {
        sum += match vm.heap.get(vm.stack.pop()?) {
            VCell::Number(n) => n,
            vcell => {
                return Err(InvalidArgs(
                    "+".to_string(),
                    "number".to_string(),
                    vm.heap.get_as_cell(&vcell).to_string(),
                ));
            }
        }
    }
    Ok(sum.into())
}

pub fn minus(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = builtin::pop_argc(vm, 1, None, "-")?;
    let mut result = Number::from(0);
    for _ in 0..(argc - 1) {
        result += match vm.heap.get(vm.stack.pop()?) {
            VCell::Number(n) => n,
            vcell => {
                return Err(InvalidArgs(
                    "-".to_string(),
                    "number".to_string(),
                    vm.heap.get_as_cell(&vcell).to_string(),
                ));
            }
        }
    }

    if let VCell::Number(n) = vm.heap.get(vm.stack.pop()?) {
        result = n - result;
    }

    if argc == 1 {
        result *= Number::from(-1);
    }

    Ok(VCell::Number(result))
}

pub fn multiply(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = builtin::pop_argc(vm, 0, None, "*")?;
    let mut result = Number::from(1);
    for _ in 0..argc {
        result *= match vm.heap.get(vm.stack.pop()?) {
            VCell::Number(n) => n,
            vcell => {
                return Err(InvalidArgs(
                    "*".to_string(),
                    "number".to_string(),
                    vm.heap.get_as_cell(&vcell).to_string(),
                ));
            }
        }
    }
    Ok(VCell::Number(result))
}

pub fn divide(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = builtin::pop_argc(vm, 1, Some(2), "/")?;
    let y = builtin::pop_number(vm)?;

    if y.is_zero() {
        return Err(InvalidSyntax("/ is undefined for 0".into()));
    }

    if argc == 1 {
        let result = Number::from(1) / y;
        Ok(result.into())
    } else {
        let x = builtin::pop_number(vm)?;
        let result = x / y;
        Ok(result.into())
    }
}

pub fn remainder(vm: &mut Vm) -> Result<VCell, Error> {
    builtin::pop_argc(vm, 2, Some(2), "remainder")?;
    let y = builtin::pop_integer(vm)?;
    let x = builtin::pop_integer(vm)?;

    if y.is_zero() {
        return Err(InvalidSyntax("remainder is undefined for 0".into()));
    }

    let result = match &x % &y {
        Some(num) => num,
        None => {
            return Err(InvalidSyntax(format!(
                "remainder is undefined for {} % {}",
                x, y
            )))
        }
    };
    Ok(result.into())
}

pub fn quotient(vm: &mut Vm) -> Result<VCell, Error> {
    builtin::pop_argc(vm, 2, Some(2), "quotient")?;
    let y = builtin::pop_integer(vm)?;
    let x = builtin::pop_integer(vm)?;

    if y.is_zero() {
        return Err(InvalidSyntax("quotient is undefined for 0".into()));
    }

    let result = match x.quotient(&y) {
        Some(num) => num,
        None => {
            return Err(InvalidSyntax(format!(
                "quotient is undefined for {} % {}",
                x, y
            )))
        }
    };
    Ok(result.into())
}

pub fn exact_inexact(vm: &mut Vm) -> Result<VCell, Error> {
    builtin::pop_argc(vm, 1, Some(1), "exact->inexact")?;
    let x = builtin::pop_number(vm)?;
    match x.to_inexact() {
        Some(num) => Ok(num.into()),
        None => Ok(x.into()),
    }
}

pub fn inexact_exact(vm: &mut Vm) -> Result<VCell, Error> {
    builtin::pop_argc(vm, 1, Some(1), "inexact->exact")?;
    let x = builtin::pop_number(vm)?;
    match x.to_exact() {
        Some(num) => Ok(num.into()),
        None => Ok(x.into()),
    }
}

pub fn abs(vm: &mut Vm) -> Result<VCell, Error> {
    builtin::pop_argc(vm, 1, Some(1), "exact->inexact")?;
    let x = builtin::pop_number(vm)?;
    let x = x.abs();
    Ok(x.into())
}