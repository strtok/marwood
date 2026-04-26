use crate::error::Error;
use crate::vm::builtin::pop_argc;
use crate::vm::vcell::VCell;
use crate::vm::Vm;

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("display", display);
    vm.load_builtin("write", write);
    vm.load_builtin("term-rows", term_rows);
    vm.load_builtin("term-cols", term_cols);
    vm.load_builtin("time-utc", time_utc);
    vm.load_builtin("eof-object", eof_object);
    vm.load_builtin("eof-object?", eof_object_p);
}

pub fn display(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "display")?;
    let obj = vm.heap.get_as_cell(vm.stack.pop()?);
    vm.display(&obj);
    Ok(VCell::Void)
}

pub fn write(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "write")?;
    let obj = vm.heap.get_as_cell(vm.stack.pop()?);
    vm.write(&obj);
    Ok(VCell::Void)
}

pub fn term_rows(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 0, Some(0), "term-rows")?;
    Ok(VCell::Number(vm.term_rows().into()))
}

pub fn term_cols(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 0, Some(0), "term-cols")?;
    Ok(VCell::Number(vm.term_cols().into()))
}

pub fn time_utc(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 0, Some(0), "time-utc")?;
    Ok(VCell::Number(vm.time_utc().into()))
}

pub fn eof_object(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 0, Some(0), "eof-object")?;
    Ok(VCell::Eof)
}

pub fn eof_object_p(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "eof-object?")?;
    let val = vm.heap.get(vm.stack.pop()?);
    Ok(VCell::Bool(val.is_eof()))
}
