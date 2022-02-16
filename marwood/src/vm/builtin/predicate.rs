use crate::vm::builtin::pop_argc;
use crate::vm::vcell::VCell;
use crate::vm::{Error, Vm};

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("boolean?", is_boolean);
    vm.load_builtin("char?", is_char);
    vm.load_builtin("eq?", eq);
    vm.load_builtin("equal?", equal);
    vm.load_builtin("eqv?", eqv);
    vm.load_builtin("list?", is_list);
    vm.load_builtin("not", not);
    vm.load_builtin("null?", is_null);
    vm.load_builtin("pair?", is_pair);
    vm.load_builtin("port?", is_port);
    vm.load_builtin("procedure?", is_procedure);
    vm.load_builtin("string?", is_string);
    vm.load_builtin("symbol?", is_symbol);
    vm.load_builtin("vector?", is_vector);

    vm.load_builtin("number?", is_number);
    vm.load_builtin("complex?", is_complex);
    vm.load_builtin("real?", is_real);
    vm.load_builtin("rational?", is_rational);
    vm.load_builtin("integer?", is_integer);
}

pub fn is_boolean(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "boolean?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_boolean().into())
}

pub fn is_char(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "char?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_char().into())
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

pub fn is_complex(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "complex?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result
        .as_number()
        .map(|it| it.is_complex())
        .unwrap_or(false)
        .into())
}

pub fn is_real(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "real?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result
        .as_number()
        .map(|it| it.is_real())
        .unwrap_or(false)
        .into())
}

pub fn is_rational(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "rational?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result
        .as_number()
        .map(|it| it.is_rational())
        .unwrap_or(false)
        .into())
}

pub fn is_integer(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "integer?")?;
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result
        .as_number()
        .map(|it| it.is_integer())
        .unwrap_or(false)
        .into())
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
    let result = vm.heap.get(vm.stack.pop()?);
    Ok(result.is_string().into())
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
