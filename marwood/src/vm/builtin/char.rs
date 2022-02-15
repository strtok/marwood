use crate::number::Number;
use crate::vm::builtin::{pop_argc, pop_char, pop_integer};
use crate::vm::vcell::VCell;
use crate::vm::Error::InvalidSyntax;
use crate::vm::{Error, Vm};

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("char->integer", char_to_integer);
    vm.load_builtin("char-alphabetic?", is_alphabetic);
    vm.load_builtin("char-lower-case?", is_lower_case);
    vm.load_builtin("char-numeric?", is_numeric);
    vm.load_builtin("char-upper-case?", is_upper_case);
    vm.load_builtin("char-whitespace?", is_whtespace);
    vm.load_builtin("char=?", char_eq);
    vm.load_builtin("char<?", char_lt);
    vm.load_builtin("char<=?", char_lt_eq);
    vm.load_builtin("char>?", char_gt);
    vm.load_builtin("char>=?", char_gt_eq);
    vm.load_builtin("char-ci=?", char_ci_eq);
    vm.load_builtin("char-ci<?", char_ci_lt);
    vm.load_builtin("char-ci<=?", char_ci_lt_eq);
    vm.load_builtin("char-ci>?", char_ci_gt);
    vm.load_builtin("char-ci>=?", char_ci_gt_eq);
    vm.load_builtin("char-upcase", char_upcase);
    vm.load_builtin("char-downcase", char_downcase);
    vm.load_builtin("char-foldcase", char_foldcase);
    vm.load_builtin("digit-value", digit_value);
    vm.load_builtin("integer->char", integer_to_char);
}

pub fn is_alphabetic(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char-alphabetic?")?;
    let c = pop_char(vm)?;
    Ok(c.is_alphabetic().into())
}

pub fn is_numeric(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char-numeric?")?;
    let c = pop_char(vm)?;
    Ok(c.is_numeric().into())
}

pub fn is_lower_case(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char-lower-case?")?;
    let c = pop_char(vm)?;
    Ok(c.is_lowercase().into())
}

pub fn is_upper_case(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char-upper-case?")?;
    let c = pop_char(vm)?;
    Ok(c.is_uppercase().into())
}

pub fn is_whtespace(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char-whitespace?")?;
    let c = pop_char(vm)?;
    Ok(c.is_whitespace().into())
}

pub fn integer_to_char(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "integer->char")?;
    let err = |num: Number| Err(InvalidSyntax(format!("{} is not valid unicode", num)));
    let num = pop_integer(vm)?;
    match num.to_u32() {
        Some(num) => match char::from_u32(num) {
            Some(c) => Ok(c.into()),
            None => err(num.into()),
        },
        None => err(num),
    }
}

pub fn char_to_integer(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char->integer")?;
    let c = pop_char(vm)?;
    Ok(Number::from(c as u32).into())
}

pub fn char_upcase(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char-upcase")?;
    let c = pop_char(vm)?;
    if c.is_ascii() {
        Ok(c.to_ascii_uppercase().into())
    } else if c.to_uppercase().into_iter().count() == 1 {
        Ok(c.to_uppercase().into_iter().next().unwrap().into())
    } else {
        Ok(c.into())
    }
}

pub fn char_downcase(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char-downcase")?;
    let c = pop_char(vm)?;
    if c.is_ascii() {
        Ok(c.to_ascii_lowercase().into())
    } else if c.to_lowercase().into_iter().count() == 1 {
        Ok(c.to_lowercase().into_iter().next().unwrap().into())
    } else {
        Ok(c.into())
    }
}

pub fn char_foldcase(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char-foldcase")?;
    let c = pop_char(vm)?;
    if c.is_ascii() {
        Ok(c.to_ascii_lowercase().into())
    } else if c.to_lowercase().into_iter().count() == 1 {
        Ok(c.to_lowercase().into_iter().next().unwrap().into())
    } else {
        Ok(c.into())
    }
}

pub fn digit_value(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "digit-value")?;
    let c = pop_char(vm)?;
    if !c.is_digit(10) {
        Ok(false.into())
    } else {
        Ok(Number::from(c.to_digit(10).unwrap()).into())
    }
}

pub fn char_eq(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char=?", |x, y| x == y)
}

pub fn char_lt(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char<?", |x, y| x < y)
}

pub fn char_lt_eq(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char<=?", |x, y| x <= y)
}

pub fn char_gt(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char>?", |x, y| x > y)
}

pub fn char_gt_eq(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char>=?", |x, y| x >= y)
}

pub fn char_ci_eq(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char-ci=?", |x, y| x.eq_ignore_ascii_case(y))
}

pub fn char_ci_lt(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char-ci<?", |x, y| {
        x.to_ascii_lowercase() < y.to_ascii_lowercase()
    })
}

pub fn char_ci_lt_eq(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char-ci<=?", |x, y| {
        x.to_ascii_lowercase() <= y.to_ascii_lowercase()
    })
}

pub fn char_ci_gt(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char-ci>?", |x, y| {
        x.to_ascii_lowercase() > y.to_ascii_lowercase()
    })
}

pub fn char_ci_gt_eq(vm: &mut Vm) -> Result<VCell, Error> {
    char_comp(vm, "char-ci>=?", |x, y| {
        x.to_ascii_lowercase() >= y.to_ascii_lowercase()
    })
}

fn char_comp(vm: &mut Vm, name: &str, comp: impl Fn(&char, &char) -> bool) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, name)?;
    let mut result = true;

    let mut y = pop_char(vm)?;
    for _ in 0..argc - 1 {
        let x = pop_char(vm)?;
        if !comp(&x, &y) {
            result = false;
        }
        y = x;
    }

    Ok(result.into())
}
