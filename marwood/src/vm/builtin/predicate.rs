use crate::vm::builtin::pop_argc;
use crate::vm::vcell::VCell;
use crate::vm::{Error, Vm};

pub fn is_boolean(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "boolean?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_boolean().into())
}

pub fn is_char(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char?")?;
    let _ = vm.stack.pop()?;
    Ok(false.into())
}

pub fn is_null(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "null?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_nil().into())
}

pub fn is_number(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "number?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_number().into())
}

pub fn is_pair(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "pair?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_pair().into())
}

pub fn is_port(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "port?")?;
    let _ = vm.stack.pop()?;
    Ok(false.into())
}

pub fn is_procedure(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "procedure?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_procedure().into())
}

pub fn is_string(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string?")?;
    let _ = vm.stack.pop()?;
    Ok(false.into())
}

pub fn is_symbol(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "symbol?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_symbol().into())
}

pub fn is_vector(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "vector?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_vector().into())
}

pub fn eq(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "eq?")?;
    let left = vm.stack.pop()?.clone();
    let right = vm.stack.pop()?.clone();
    Ok(vm.eqv(&left, &right)?.into())
}

pub fn eqv(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "eqv?")?;
    let left = vm.stack.pop()?.clone();
    let right = vm.stack.pop()?.clone();
    Ok(vm.eqv(&left, &right)?.into())
}

pub fn equal(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "eqv?")?;
    let left = vm.stack.pop()?.clone();
    let right = vm.stack.pop()?.clone();
    Ok(vm.equal(&left, &right)?.into())
}

pub fn not(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "not")?;
    Ok(match vm.heap.get(vm.stack.pop()?) {
        VCell::Bool(val) => !val,
        _ => false,
    }
    .into())
}

pub fn is_list(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "list?")?;
    let mut rest = vm.heap.get(vm.stack.pop()?);
    loop {
        if !rest.is_pair() {
            return Ok(rest.is_nil().into());
        }
        rest = vm.heap.get(&rest.as_cdr()?);
    }
}
