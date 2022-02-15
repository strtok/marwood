use crate::number::Number;
use crate::vm::builtin::{pop_argc, pop_char, pop_index, pop_string, pop_usize};
use crate::vm::vcell::VCell;
use crate::vm::Error::InvalidStringIndex;
use crate::vm::{Error, Vm};

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("make-string", make_string);
    vm.load_builtin("string", string);
    vm.load_builtin("string-append", string_append);
    vm.load_builtin("string-length", string_length);
    vm.load_builtin("string-ref", string_ref);
    vm.load_builtin("string-set!", string_set);
}

pub fn string_append(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "string-append")?;
    let mut output = String::new();
    for _ in 0..argc {
        let s = pop_string(vm, "string-append")?;
        output.insert_str(0, s.borrow().as_str());
    }
    Ok(VCell::string(output))
}

pub fn string_length(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string-length")?;
    let s = pop_string(vm, "string-length")?;
    let s = s.borrow();
    Ok(Number::from(s.chars().count() as u64).into())
}

pub fn string_ref(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "string-ref")?;
    let idx = pop_index(vm)?;
    let s = pop_string(vm, "string-ref")?;
    let s = s.borrow();
    match s.chars().nth(idx) {
        Some(c) => Ok(c.into()),
        None => Err(InvalidStringIndex(idx, s.chars().count() - 1)),
    }
}

pub fn string_set(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 3, Some(3), "string-set!")?;
    let c = pop_char(vm)?;
    let idx = pop_index(vm)?;
    let s = pop_string(vm, "string-set!")?;
    let mut s = s.borrow_mut();
    let range = s
        .char_indices()
        .nth(idx)
        .ok_or_else(|| Error::InvalidStringIndex(idx, s.chars().count() - 1))
        .map(|it| (it.0, it.0 + it.1.len_utf8()))?;
    s.replace_range(range.0..range.1, &c.to_string());
    Ok(VCell::void())
}

pub fn make_string(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(2), "make-string")?;
    let c = match argc {
        1 => '\0',
        _ => pop_char(vm)?,
    };
    let size = pop_usize(vm)?;
    Ok(VCell::string(
        std::iter::repeat(c).take(size).collect::<String>(),
    ))
}

pub fn string(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "string")?;
    let mut v = vec!['\0'; argc];
    for it in 0..argc {
        *v.get_mut(argc - it - 1).unwrap() = pop_char(vm)?;
    }
    Ok(VCell::string(v.into_iter().collect::<String>()))
}
