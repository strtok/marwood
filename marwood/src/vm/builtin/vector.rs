use crate::error::Error;
use crate::error::Error::{ExpectedPairButFound, InvalidSyntax, InvalidVectorIndex};
use crate::number::Number;
use crate::vm::builtin::{pop_argc, pop_index, pop_vector};
use crate::vm::vcell::VCell;
use crate::vm::Vm;

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("make-vector", make_vector);
    vm.load_builtin("vector", vector);
    vm.load_builtin("vector-length", vector_length);
    vm.load_builtin("vector->list", vector_to_list);
    vm.load_builtin("list->vector", list_to_vector);
    vm.load_builtin("vector-ref", vector_ref);
    vm.load_builtin("vector-set!", vector_set);
    vm.load_builtin("vector-fill!", vector_fill);
    vm.load_builtin("vector-copy", vector_copy);
    vm.load_builtin("vector-copy!", vector_mut_copy);
}

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
    let idx = pop_index(vm, "vector-ref")?;
    let vector = pop_vector(vm)?;
    match vector.get(idx) {
        Some(value) => Ok(value),
        None => Err(InvalidVectorIndex(idx, vector.len())),
    }
}

pub fn vector_set(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 3, Some(3), "vector-set!")?;
    let value = vm.stack.pop()?.clone();
    let idx = pop_index(vm, "vector-set!")?;
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
            return Err(ExpectedPairButFound(vm.heap.get_as_cell(&list)));
        }
    }
    let mut outv = vec![];
    while list.is_pair() {
        outv.push(list.as_car()?);
        list = vm.heap.get(&list.as_cdr()?);
    }
    Ok(VCell::vector(outv))
}

pub fn vector_copy(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(3), "vector-copy")?;

    let mut end = None;
    if argc == 3 {
        end = Some(pop_index(vm, "vector-copy")?);
    }

    let mut start = None;
    if argc > 1 {
        start = Some(pop_index(vm, "vector-copy")?);
    }

    let vector = pop_vector(vm)?;
    let vector = vector.as_ref();

    match (start, end) {
        (Some(start), _) if start > vector.len() - 1 => {
            return Err(InvalidVectorIndex(start, vector.len()));
        }
        (_, Some(end)) if end > vector.len() => {
            return Err(InvalidVectorIndex(end, vector.len()));
        }
        (Some(start), Some(end)) if start > end => {
            return Err(InvalidSyntax("vector-copy requires start <= end".into()));
        }
        _ => {}
    }

    Ok(VCell::vector(vector.clone_vector(start, end)))
}

// (vector-copy! to at from start end)
pub fn vector_mut_copy(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 3, Some(5), "vector-copy!")?;

    let mut end = None;
    if argc == 5 {
        end = Some(pop_index(vm, "vector-copy!")?);
    }

    let mut start = None;
    if argc >= 4 {
        start = Some(pop_index(vm, "vector-copy!")?);
    }

    let from_vector = pop_vector(vm)?;
    let from_vector = from_vector.as_ref();

    let at = pop_index(vm, "vector-copy!")?;

    let to_vector = pop_vector(vm)?;
    let to_vector = to_vector.as_ref();

    if at > to_vector.len() - 1 {
        return Err(InvalidVectorIndex(at, to_vector.len()));
    }

    match (start, end) {
        (Some(start), _) if start > from_vector.len() - 1 => {
            return Err(InvalidVectorIndex(start, from_vector.len()));
        }
        (_, Some(end)) if end > from_vector.len() => {
            return Err(InvalidVectorIndex(end, from_vector.len()));
        }
        (Some(start), Some(end)) if start > end => {
            return Err(InvalidSyntax("vector-copy! requires start <= end".into()));
        }
        _ => {}
    }

    let start = start.unwrap_or(0);
    let end = end.unwrap_or_else(|| from_vector.len());

    if ((end - start) > to_vector.len()) || (at + end) > to_vector.len() {
        return Err(InvalidSyntax("vector-copy!: to vector is too small".into()));
    }

    for i in start..end {
        let val = from_vector.get(i).unwrap();
        to_vector.put(i + at, val);
    }

    Ok(VCell::Void)
}
