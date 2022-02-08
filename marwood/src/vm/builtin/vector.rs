use crate::number::Number;
use crate::vm::builtin::{pop_argc, pop_index, pop_vector};
use crate::vm::vcell::VCell;
use crate::vm::Error::{ExpectedPairButFound, InvalidSyntax, InvalidVectorIndex};
use crate::vm::{Error, Vm};

pub fn vector(vm: &mut Vm) -> Result<VCell, Error> {
    let len = pop_argc(vm, 0, None, "vector")?;
    let mut outv = vec![VCell::Undefined; len];
    for idx in (0..len).rev() {
        *outv.get_mut(idx).unwrap() = vm.stack.pop()?.clone();
    }
    Ok(VCell::vector(outv))
}

pub fn make_vector(vm: &mut Vm) -> Result<VCell, Error> {
    let err = || InvalidSyntax("make-vector requires an integer size".into());
    let argc = pop_argc(vm, 1, Some(2), "make-vector")?;
    let fill = match argc {
        2 => vm.stack.pop()?.clone(),
        _ => vm.heap.put(VCell::Number(Number::from(0))),
    };

    let len = match vm.heap.get(vm.stack.pop()?) {
        VCell::Number(num) => num.to_usize().ok_or_else(err)?,
        _ => return Err(err()),
    };

    let outv = vec![fill; len];
    Ok(VCell::vector(outv))
}

pub fn vector_length(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "vector-length")?;
    let vector = pop_vector(vm)?;
    Ok(Number::from(vector.len() as i64).into())
}

pub fn vector_ref(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "vector-ref")?;
    let idx = pop_index(vm)?;
    let vector = pop_vector(vm)?;
    match vector.get(idx) {
        Some(value) => Ok(value),
        None => Err(InvalidVectorIndex(idx, vector.len())),
    }
}

pub fn vector_set(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 3, Some(3), "vector-set!")?;
    let value = vm.stack.pop()?.clone();
    let idx = pop_index(vm)?;
    let vector = pop_vector(vm)?;
    if idx > vector.len() - 1 {
        return Err(InvalidVectorIndex(idx, vector.len()));
    }
    vector.put(idx, value);
    Ok(VCell::Void)
}

pub fn vector_fill(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "vector-fill!")?;
    let value = vm.heap.get(vm.stack.pop()?);
    let vector = pop_vector(vm)?;
    for idx in 0..vector.len() {
        vector.put(idx, value.clone());
    }
    Ok(VCell::Void)
}

pub fn vector_to_list(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "vector->list")?;
    let vector = pop_vector(vm)?;
    let mut tail = vm.heap.put(VCell::Nil);
    for idx in (0..vector.len()).rev() {
        let car = vector.get(idx).unwrap();
        tail = vm.heap.put(VCell::Pair(car.as_ptr()?, tail.as_ptr()?));
    }
    Ok(tail)
}

pub fn list_to_vector(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "list->vector")?;
    let mut list = vm.heap.get(vm.stack.pop()?);
    if !list.is_pair() {
        if list.is_nil() {
            return Ok(VCell::vector(vec![]));
        } else {
            return Err(ExpectedPairButFound(vm.heap.get_as_cell(&list).to_string()));
        }
    }
    let mut outv = vec![];
    while list.is_pair() {
        outv.push(list.as_car()?);
        list = vm.heap.get(&list.as_cdr()?);
    }
    Ok(VCell::vector(outv))
}
