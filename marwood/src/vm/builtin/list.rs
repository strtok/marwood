use crate::error::Error;
use crate::error::Error::{ExpectedPairButFound, InvalidSyntax};
use crate::vm::builtin::{pop_argc, pop_index};
use crate::vm::vcell::VCell;
use crate::vm::Vm;

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("append", append);
    vm.load_builtin("car", car);
    vm.load_builtin("cdr", cdr);
    vm.load_builtin("cons", cons);
    vm.load_builtin("list-ref", list_ref);
    vm.load_builtin("list-tail", list_tail);
    vm.load_builtin("reverse", reverse);
    vm.load_builtin("set-car!", set_car);
    vm.load_builtin("set-cdr!", set_cdr);
}

pub fn car(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "car")?;
    Ok(match vm.heap.get(vm.stack.pop()?) {
        VCell::Pair(car, _) => VCell::ptr(car),
        arg => return Err(ExpectedPairButFound(vm.heap.get_as_cell(&arg).to_string())),
    })
}

pub fn cdr(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "cdr")?;
    Ok(match vm.heap.get(vm.stack.pop()?) {
        VCell::Pair(_, cdr) => VCell::ptr(cdr),
        arg => return Err(ExpectedPairButFound(vm.heap.get_as_cell(&arg).to_string())),
    })
}

pub fn cons(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "cons")?;
    let cdr = vm.stack.pop()?.as_ptr()?;
    let car = vm.stack.pop()?.as_ptr()?;
    Ok(VCell::Pair(car, cdr))
}

pub fn set_car(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "set-car!")?;
    let obj = vm.stack.pop()?.clone();
    let pair = vm.stack.pop()?.clone();
    let new_pair = match vm.heap.get(&pair) {
        VCell::Pair(_, cdr) => VCell::Pair(obj.as_ptr()?, cdr),
        _ => {
            return Err(InvalidSyntax("set-car! expected a pair".into()));
        }
    };
    *vm.heap.get_at_index_mut(pair.as_ptr()?) = new_pair;
    Ok(VCell::Void)
}

pub fn set_cdr(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "set-cdr!")?;
    let obj = vm.stack.pop()?.clone();
    let pair = vm.stack.pop()?.clone();
    let new_pair = match vm.heap.get(&pair) {
        VCell::Pair(car, _) => VCell::Pair(car, obj.as_ptr()?),
        _ => {
            return Err(InvalidSyntax("set-cdr! expected a pair".into()));
        }
    };
    *vm.heap.get_at_index_mut(pair.as_ptr()?) = new_pair;
    Ok(VCell::Void)
}

/// Clone List
///
/// This function clones list, returning an error if list is
/// not a list or is improper.
///
/// # Arguments
/// `vm` - The vm in which to allocate the list
/// `list` - The list to clone
fn clone_list(vm: &mut Vm, list: VCell) -> Result<(VCell, VCell), Error> {
    let mut rest = list.clone();
    if !rest.is_pair() {
        return Err(ExpectedPairButFound(vm.heap.get_as_cell(&rest).to_string()));
    }

    let mut head = VCell::Nil;
    let mut tail = VCell::Nil;

    let nil = vm.heap.put(VCell::Nil).as_ptr()?;

    loop {
        let pair = vm.heap.put(VCell::Pair(rest.as_car()?.as_ptr()?, nil));
        if head.is_nil() {
            head = pair.clone();
        }
        if tail.is_nil() {
            tail = pair.clone();
        } else {
            let last_pair = vm.heap.get(&tail);
            *vm.heap.get_at_index_mut(tail.as_ptr()?) =
                VCell::Pair(last_pair.as_car()?.as_ptr()?, pair.as_ptr()?);
            tail = pair;
        }
        rest = vm.heap.get(&rest.as_cdr()?);
        if !rest.is_pair() {
            if rest.is_nil() {
                return Ok((head, tail));
            } else {
                return Err(InvalidSyntax(format!(
                    "{} is an improper list",
                    vm.heap.get_as_cell(&list)
                )));
            }
        }
    }
}

pub fn append(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 0, None, "append")?;
    if argc == 0 {
        return Ok(VCell::Nil);
    }
    let mut tail = vm.stack.pop()?.clone();
    for _ in 0..(argc - 1) {
        let list = vm.heap.get(&vm.stack.pop()?.clone());
        match list {
            VCell::Nil => {
                continue;
            }
            VCell::Pair(_, _) => {}
            vcell => {
                return Err(ExpectedPairButFound(
                    vm.heap.get_as_cell(&vcell).to_string(),
                ));
            }
        }
        let (head, sub_tail) = clone_list(vm, list)?;
        let sub_pair = vm.heap.get(&sub_tail);
        *vm.heap.get_at_index_mut(sub_tail.as_ptr()?) =
            VCell::Pair(sub_pair.as_car()?.as_ptr()?, tail.as_ptr()?);
        tail = head;
    }

    Ok(tail)
}

pub fn reverse(vm: &mut Vm) -> Result<VCell, Error> {
    let _ = pop_argc(vm, 1, Some(1), "reverse")?;
    let list = vm.heap.get(vm.stack.pop()?);
    let mut rest = list.clone();
    if !rest.is_pair() {
        return if rest.is_nil() {
            Ok(rest)
        } else {
            Err(ExpectedPairButFound(vm.heap.get_as_cell(&rest).to_string()))
        };
    }
    let mut tail = vm.heap.put(VCell::Nil);
    loop {
        tail = vm
            .heap
            .put(VCell::Pair(rest.as_car()?.as_ptr()?, tail.as_ptr()?));
        rest = vm.heap.get(&rest.as_cdr()?);
        if !rest.is_pair() {
            if rest.is_nil() {
                break;
            } else {
                return Err(InvalidSyntax(format!(
                    "{} is an improper list",
                    vm.heap.get_as_cell(&list)
                )));
            }
        }
    }
    Ok(tail)
}

/// Get List Tail
///
/// Return the list tail at the given index, or an error if the
/// index is invalid.
fn get_list_tail(vm: &mut Vm, list: &VCell, idx: usize) -> Result<VCell, Error> {
    let mut rest = list.clone();
    let mut rest_idx = idx;
    loop {
        if rest_idx == 0 {
            return Ok(rest);
        }
        rest_idx -= 1;
        let node = vm.heap.get(&rest).clone();
        if (!node.is_pair() && rest_idx != 0) || node.is_nil() {
            return Err(InvalidSyntax(format!(
                "{} is out of range for {}",
                idx,
                vm.heap.get_as_cell(list)
            )));
        }
        rest = node.as_cdr()?.clone();
    }
}

pub fn list_ref(vm: &mut Vm) -> Result<VCell, Error> {
    let _ = pop_argc(vm, 2, Some(2), "list-ref")?;
    let idx = pop_index(vm, "list-ref")?;
    let list_ptr = vm.stack.pop()?.clone();
    let list = vm.heap.get(&list_ptr);
    if !list.is_pair() && !list.is_nil() {
        return Err(ExpectedPairButFound(vm.heap.get_as_cell(&list).to_string()));
    }
    let tail = get_list_tail(vm, &list_ptr, idx)?;
    match vm.heap.get(&tail) {
        VCell::Pair(car, _) => Ok(VCell::Ptr(car)),
        _ => Err(InvalidSyntax(format!(
            "{} is out of range for {}",
            idx,
            vm.heap.get_as_cell(&list)
        ))),
    }
}

pub fn list_tail(vm: &mut Vm) -> Result<VCell, Error> {
    let _ = pop_argc(vm, 2, Some(2), "list-tail")?;
    let idx = pop_index(vm, "list-tail")?;
    let list_ptr = vm.stack.pop()?.clone();
    let list = vm.heap.get(&list_ptr);
    if !list.is_pair() && !list.is_nil() {
        return Err(ExpectedPairButFound(vm.heap.get_as_cell(&list).to_string()));
    }
    get_list_tail(vm, &list_ptr, idx)
}
