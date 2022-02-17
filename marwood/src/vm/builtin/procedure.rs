use crate::cell::Cell;
use crate::vm::builtin::pop_argc;
use crate::vm::vcell::VCell;
use crate::vm::Error::ErrorSignal;
use crate::vm::{Error, Vm};

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("error", error);
}

pub fn error(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "error")?;
    let mut result = vec![Cell::Nil; argc];
    for it in 0..argc {
        let vcell = vm.stack.pop()?;
        *result.get_mut(argc - it - 1).unwrap() = vm.heap.get_as_cell(vcell);
    }
    Err(ErrorSignal(result))
}
