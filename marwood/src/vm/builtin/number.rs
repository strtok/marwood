use crate::number::{Exactness, Number};
use crate::vm::builtin::{pop_argc, pop_integer, pop_number, pop_string, pop_usize};
use crate::vm::vcell::VCell;
use crate::vm::Error::{InvalidArgs, InvalidSyntax};
use crate::vm::{Error, Vm};

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("*", multiply);
    vm.load_builtin("/", divide);
    vm.load_builtin("+", plus);
    vm.load_builtin("-", minus);
    vm.load_builtin("<", lt);
    vm.load_builtin("<=", lteq);
    vm.load_builtin("=", num_equal);
    vm.load_builtin(">", gt);
    vm.load_builtin(">=", gteq);
    vm.load_builtin("%", remainder);
    vm.load_builtin("abs", abs);
    vm.load_builtin("acos", acos);
    vm.load_builtin("asin", asin);
    vm.load_builtin("atan", atan);
    vm.load_builtin("ceiling", ceiling);
    vm.load_builtin("cos", cos);
    vm.load_builtin("denominator", denominator);
    vm.load_builtin("even?", even);
    vm.load_builtin("exact->inexact", exact_inexact);
    vm.load_builtin("exp", exp);
    vm.load_builtin("expt", expt);
    vm.load_builtin("floor", floor);
    vm.load_builtin("inexact->exact", inexact_exact);
    vm.load_builtin("log", log);
    vm.load_builtin("min", min);
    vm.load_builtin("max", max);
    vm.load_builtin("modulo", modulo);
    vm.load_builtin("numerator", numerator);
    vm.load_builtin("number->string", number_string);
    vm.load_builtin("negative?", negative);
    vm.load_builtin("odd?", odd);
    vm.load_builtin("pow", expt);
    vm.load_builtin("positive?", positive);
    vm.load_builtin("quotient", quotient);
    vm.load_builtin("remainder", remainder);
    vm.load_builtin("round", round);
    vm.load_builtin("sin", sin);
    vm.load_builtin("sqrt", sqrt);
    vm.load_builtin("string->number", string_number);
    vm.load_builtin("tan", tan);
    vm.load_builtin("truncate", truncate);
    vm.load_builtin("zero?", zero);
}

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
    let argc = pop_argc(vm, 1, None, name)?;
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
    pop_argc(vm, 1, Some(1), name)?;
    let x = match vm.heap.get(vm.stack.pop()?) {
        VCell::Number(val) => val,
        _ => return Ok(false.into()),
    };
    Ok(predicate(&x).into())
}

pub fn plus(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 0, None, "+")?;
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
    let argc = pop_argc(vm, 1, None, "-")?;
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
    let argc = pop_argc(vm, 0, None, "*")?;
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
    let argc = pop_argc(vm, 1, Some(2), "/")?;
    let y = pop_number(vm)?;

    if y.is_zero() {
        return Err(InvalidSyntax("/ is undefined for 0".into()));
    }

    if argc == 1 {
        let result = Number::from(1) / y;
        Ok(result.into())
    } else {
        let x = pop_number(vm)?;
        let result = x / y;
        Ok(result.into())
    }
}

pub fn remainder(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "remainder")?;
    let y = pop_integer(vm)?;
    let x = pop_integer(vm)?;

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

pub fn modulo(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "modulo")?;
    let y = pop_integer(vm)?;
    let x = pop_integer(vm)?;

    if y.is_zero() {
        return Err(InvalidSyntax("modulo is undefined for 0".into()));
    }

    let result = match x.modulo(&y) {
        Some(num) => num,
        None => {
            return Err(InvalidSyntax(format!(
                "modulo is undefined for {} % {}",
                x, y
            )))
        }
    };
    Ok(result.into())
}

pub fn quotient(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "quotient")?;
    let y = pop_integer(vm)?;
    let x = pop_integer(vm)?;

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

macro_rules! unary_trig {
    ($vm:ident, $name:ident) => {{
    pop_argc($vm, 1, Some(1), stringify!($name))?;
    let x = pop_number($vm)?;

    if let Some(result) = x.$name() {
        Ok(result.into())
    } else {
        return Err(InvalidSyntax(format!(
            "{} is undefined for {}",
            stringify!($name),
            x
        )))
    }
    }};
}

pub fn exp(vm: &mut Vm) -> Result<VCell, Error> {
    unary_trig!(vm, exp)
}

pub fn log(vm: &mut Vm) -> Result<VCell, Error> {
    unary_trig!(vm, log)
}

pub fn sin(vm: &mut Vm) -> Result<VCell, Error> {
  unary_trig!(vm, sin)
}

pub fn cos(vm: &mut Vm) -> Result<VCell, Error> {
    unary_trig!(vm, cos)
}

pub fn tan(vm: &mut Vm) -> Result<VCell, Error> {
    unary_trig!(vm, tan)
}

pub fn asin(vm: &mut Vm) -> Result<VCell, Error> {
    unary_trig!(vm, asin)
}

pub fn acos(vm: &mut Vm) -> Result<VCell, Error> {
    unary_trig!(vm, acos)
}

pub fn atan(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(2), "atan2")?;
    match argc {
        2 => {
            let y = pop_number(vm)?;
            let x = pop_number(vm)?;

            if let Some(result) = y.atan2(x.clone()) {
                Ok(result.into())
            } else {
                return Err(InvalidSyntax(format!(
                    "atan is undefined for {} {}",
                    y, x
                )))
            }
        }
        _ => {
            let y = pop_number(vm)?;

            if let Some(result) = y.atan() {
                Ok(result.into())
            } else {
                return Err(InvalidSyntax(format!(
                    "atan is undefined for {}",
                    y
                )))
            }
        },
    }
}

pub fn sqrt(vm: &mut Vm) -> Result<VCell, Error> {
    unary_trig!(vm, sqrt)
}

pub fn expt(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "expt")?;
    let exp = pop_integer(vm)?;
    let x = pop_number(vm)?;

    let exp = match exp.to_u32() {
        Some(exp) => exp,
        None => {
            return Err(InvalidSyntax("expt: exponent is too large".into()));
        }
    };

    Ok(x.pow(exp).into())
}

pub fn exact_inexact(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "exact->inexact")?;
    let x = pop_number(vm)?;
    match x.to_inexact() {
        Some(num) => Ok(num.into()),
        None => Ok(x.into()),
    }
}

pub fn inexact_exact(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "inexact->exact")?;
    let x = pop_number(vm)?;
    match x.to_exact() {
        Some(num) => Ok(num.into()),
        None => Ok(x.into()),
    }
}

pub fn abs(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "exact->inexact")?;
    let x = pop_number(vm)?;
    let x = x.abs();
    Ok(x.into())
}

pub fn ceiling(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "ceiling")?;
    let x = pop_number(vm)?;
    let x = x.ceil();
    Ok(x.into())
}

pub fn floor(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "floor")?;
    let x = pop_number(vm)?;
    let x = x.floor();
    Ok(x.into())
}

pub fn round(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "round")?;
    let x = pop_number(vm)?;
    let x = x.round();
    Ok(x.into())
}

pub fn truncate(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "truncate")?;
    let x = pop_number(vm)?;
    let x = x.truncate();
    Ok(x.into())
}

pub fn numerator(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "numerator")?;
    let x = pop_number(vm)?;
    let x = x.numerator();
    Ok(x.into())
}

pub fn denominator(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "denominator")?;
    let x = pop_number(vm)?;
    let x = x.denominator();
    Ok(x.into())
}

fn min(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 2, None, "min")?;
    let mut result = pop_number(vm)?;
    for _ in 0..argc - 1 {
        let num = pop_number(vm)?;
        if num < result {
            result = num;
        }
    }
    Ok(result.into())
}

fn max(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 2, None, "max")?;
    let mut result = pop_number(vm)?;
    for _ in 0..argc - 1 {
        let num = pop_number(vm)?;
        if num > result {
            result = num;
        }
    }
    Ok(result.into())
}

fn number_string(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(2), "number->string")?;

    let radix = match argc {
        2 => pop_usize(vm)?,
        _ => 10,
    };
    let num = pop_number(vm)?;
    let result = match radix {
        16 => format!("{:x}", num),
        8 => format!("{:o}", num),
        2 => format!("{:b}", num),
        _ => format!("{}", num),
    };

    Ok(VCell::string(result))
}

fn string_number(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(2), "string->number")?;

    let radix = match argc {
        2 => pop_usize(vm)? as u32,
        _ => 10_u32,
    };
    let s = pop_string(vm, "string->number")?;
    let s = s.borrow();
    let s = s.as_str();
    match Number::parse_with_exactness(s, Exactness::Unspecified, radix) {
        Some(num) => Ok(VCell::Number(num)),
        None => Ok(false.into()),
    }
}
