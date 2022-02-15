use crate::vm::builtin::pop_argc;
use crate::vm::vcell::VCell;
use crate::vm::{Error, Vm};

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("display", display);
    vm.load_builtin("write", write);
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
