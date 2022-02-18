use crate::cell::Cell;
use crate::vm::builtin::pop_argc;
use crate::vm::vcell::VCell;
use crate::vm::vcell::VCell::ArgumentCount;
use crate::vm::Error::{ErrorSignal, InvalidSyntax};
use crate::vm::{Error, Vm};
use log::trace;
use std::rc::Rc;

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("error", error);
    vm.load_builtin("call/cc", call_cc);
    vm.load_builtin("call-with-current-continuation", call_cc);
}

fn error(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "error")?;
    let mut result = vec![Cell::Nil; argc];
    for it in 0..argc {
        let vcell = vm.stack.pop()?;
        *result.get_mut(argc - it - 1).unwrap() = vm.heap.get_as_cell(vcell);
    }
    Err(ErrorSignal(result))
}

/// call/cc
///
/// 1. Pop the single arg to call/cc off the stack, ensuring it's
///    a procedure.
/// 2. Create a continuation, pushing it back on the stack as a
///    single argument bound for the proc and push an arg count of
///    1.
/// 3. Decrement %ip by 1 so that the next instruction that executes
///    is the CALL %acc that executed this builtin.
/// 4. Return the proc, ensuring that it's immediately put into %acc
///    as a result of this procedure call. This ensures that the CALL %acc
///    setup by the decrement to %ip will apply proc.
fn call_cc(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "call/cc")?;
    let proc = match vm.stack.pop()?.clone() {
        proc if vm.heap.get(&proc).is_procedure() => proc,
        _ => {
            return Err(InvalidSyntax("bad call/cc".into()));
        }
    };
    let cont = Rc::new(vm.to_continuation());
    trace!("cont: {:?}", cont);
    let cont = vm.heap.put(VCell::Continuation(cont));
    vm.stack.push(cont);
    vm.stack.push(ArgumentCount(1));
    vm.ip.1 -= 1;
    Ok(proc)
}
