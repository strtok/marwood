use crate::cell::Cell;
use crate::error::Error;
use crate::error::Error::{ErrorSignal, InvalidSyntax};
use crate::vm::builtin::pop_argc;
use crate::vm::lambda::Lambda;
use crate::vm::opcode::OpCode;
use crate::vm::vcell::VCell;
use crate::vm::vcell::VCell::ArgumentCount;
use crate::vm::Vm;
use log::trace;
use std::rc::Rc;

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("apply", apply);
    vm.load_builtin("call/cc", call_cc);
    vm.load_builtin("call-with-current-continuation", call_cc);
    vm.load_builtin("error", error);
    vm.load_builtin("eval", eval);
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

/// Eval
///
/// Eval pops the expr off the stack to eval, converts ot an AST
/// (Cell) and then compiles a new top level Lambda given the AST
/// with no IOF environment.
///
/// The lambda is returned by eval() to immediately be placed into
/// %acc by the calling code.
///
/// Before returning, this function decrements %ip so that the next
/// instruction to execute is CALL %acc.
fn eval(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "eval")?;

    let expr = vm.pop()?;
    let expr = vm.heap.get_as_cell(&expr);

    let mut lambda = Lambda::new(vec![]);
    lambda.set_top_level();
    lambda.emit(OpCode::Enter);
    vm.compile(&mut lambda, true, &expr)?;
    lambda.emit(OpCode::Ret);
    let lambda = vm.heap.put(lambda);

    vm.stack.push(ArgumentCount(0));
    vm.ip.1 -= 1;
    Ok(lambda)
}

/// Apply
///
/// 1. Pop the last arg off the stack. It -must- be a list of args
///    that will be pushed.
/// 2. Save procedure, which is the first arg on the stack.
/// 3. If there were any additional args, shift them down by 1
///    each to overwrite procedure.
/// 4. Push each arg rest on to the stack.
/// 5. Push a new argc on the stack.
/// 6. Set the applied procedure as the continuation by placing it
///    in %acc and decrementing %ip by 1 so that the next instruction
///    is CALL %acc.
fn apply(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 2, None, "apply")?;

    let mut rest = vm.pop()?;
    if !rest.is_nil() && !rest.is_pair() {
        return Err(InvalidSyntax(
            "the last argument to apply must be a proper list".into(),
        ));
    }

    let proc = vm.stack.get_offset(-(argc as i64 - 2))?.clone();
    for it in (0..argc - 2).rev() {
        let from = 0_i64 - it as i64;
        let to = from - 1;
        let val = vm.stack.get_offset(from)?.clone();
        *vm.stack.get_offset_mut(to)? = val;
    }
    let _ = vm.stack.pop()?;

    let mut argc = argc - 2;
    while rest.is_pair() {
        argc += 1;
        vm.stack.push(rest.as_car()?);
        rest = vm.heap.get(&rest.as_cdr()?);
    }
    if !rest.is_nil() {
        return Err(InvalidSyntax(
            "the last argument to apply must be a proper list".into(),
        ));
    }
    vm.stack.push(VCell::ArgumentCount(argc));
    vm.ip.1 -= 1;
    Ok(proc)
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
