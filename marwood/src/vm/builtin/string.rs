use crate::number::Number;
use crate::vm::builtin::{pop_argc, pop_char, pop_string, pop_usize};
use crate::vm::vcell::VCell;
use crate::vm::{Error, Vm};

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("make-string", make_string);
    vm.load_builtin("string", string);
    vm.load_builtin("string-length", string_length);
}

pub fn string_length(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string-length")?;
    let s = pop_string(vm, "string-length")?;
    let s = s.borrow();
    Ok(Number::from(s.chars().count() as u64).into())
}

pub fn make_string(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(2), "make-string")?;
    let c = match argc {
        1 => '\0',
        _ => pop_char(vm)?,
    };
    let size = pop_usize(vm)?;
    Ok(VCell::string(std::iter::repeat(c).take(size).collect::<String>()).into())
}

pub fn string(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "string")?;
    let mut v = vec!['\0'; argc];
    for it in 0..argc {
        *v.get_mut(argc - it - 1).unwrap() = pop_char(vm)?;
    }
    Ok(VCell::string(v.into_iter().collect::<String>()).into())
}
