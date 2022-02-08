use crate::vm::vcell::VCell;
use crate::vm::{builtin, Error, Vm};

pub fn display(vm: &mut Vm) -> Result<VCell, Error> {
    builtin::pop_argc(vm, 0, Some(1), "car")?;
    let obj = vm.heap.get_as_cell(vm.stack.pop()?);
    vm.display(&obj);
    Ok(VCell::Void)
}
