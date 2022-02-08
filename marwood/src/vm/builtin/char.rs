use crate::number::Number;
use crate::vm::builtin::{pop_argc, pop_char, pop_integer};
use crate::vm::vcell::VCell;
use crate::vm::Error::InvalidSyntax;
use crate::vm::{Error, Vm};

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
