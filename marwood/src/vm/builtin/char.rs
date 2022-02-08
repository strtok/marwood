use crate::number::Number;
use crate::vm::builtin::{pop_argc, pop_char, pop_integer};
use crate::vm::vcell::VCell;
use crate::vm::Error::InvalidSyntax;
use crate::vm::{Error, Vm};

pub fn integer_to_char(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "integer->char")?;
    let err = |num: Number| {
        Err(InvalidSyntax(format!(
            "{} is not a valid unicode value",
            num
        )))
    };
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
