use crate::error::Error;
use crate::error::Error::{InvalidStringIndex, InvalidSyntax};
use crate::number::Number;
use crate::vm::Vm;
use crate::vm::builtin::{pop_argc, pop_char, pop_index, pop_string, pop_usize, pop_vector};
use crate::vm::vcell::VCell;
use std::ops::DerefMut;

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("make-string", make_string);
    vm.load_builtin("string", string);
    vm.load_builtin("string=?", string_eq);
    vm.load_builtin("string<?", string_lt);
    vm.load_builtin("string>?", string_gt);
    vm.load_builtin("string<=?", string_lt_eq);
    vm.load_builtin("string>=?", string_gt_eq);
    vm.load_builtin("string-ci=?", string_ci_eq);
    vm.load_builtin("string-ci<?", string_ci_lt);
    vm.load_builtin("string-ci>?", string_ci_gt);
    vm.load_builtin("string-ci<=?", string_ci_lt_eq);
    vm.load_builtin("string-ci>=?", string_ci_gt_eq);
    vm.load_builtin("string-append", string_append);
    vm.load_builtin("string-downcase", string_downcase);
    vm.load_builtin("string-fill!", string_fill);
    vm.load_builtin("string-foldcase", string_foldcase);
    vm.load_builtin("string-length", string_length);
    vm.load_builtin("string-ref", string_ref);
    vm.load_builtin("string-set!", string_set);
    vm.load_builtin("string-upcase", string_upcase);
    vm.load_builtin("string->list", string_list);
    vm.load_builtin("string->vector", string_vector);
    vm.load_builtin("vector->string", vector_string);
    vm.load_builtin("list->string", list_string);
    vm.load_builtin("string-copy", string_copy);
}

pub fn string_append(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "string-append")?;
    let mut output = String::new();
    for _ in 0..argc {
        let s = pop_string(vm, "string-append")?;
        output.insert_str(0, s.borrow().as_str());
    }
    Ok(VCell::string(output))
}

pub fn string_length(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string-length")?;
    let s = pop_string(vm, "string-length")?;
    let s = s.borrow();
    Ok(Number::from(s.chars().count() as u64).into())
}

pub fn string_downcase(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string-downcase")?;
    let s = pop_string(vm, "string-downcase")?;
    let s = s.borrow().to_lowercase();
    Ok(VCell::string(s))
}

pub fn string_upcase(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string-upcase")?;
    let s = pop_string(vm, "string-upcase")?;
    let s = s.borrow().to_uppercase();
    Ok(VCell::string(s))
}

pub fn string_foldcase(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string-foldcase")?;
    let s = pop_string(vm, "string-foldcase")?;
    let s = s.borrow().to_lowercase();
    Ok(VCell::string(s))
}

pub fn string_ref(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 2, Some(2), "string-ref")?;
    let idx = pop_index(vm, "string-ref")?;
    let s = pop_string(vm, "string-ref")?;
    let s = s.borrow();
    match s.chars().nth(idx) {
        Some(c) => Ok(c.into()),
        None => Err(InvalidStringIndex(idx, s.chars().count() - 1)),
    }
}

fn char_offset(s: &str, idx: usize) -> Result<usize, Error> {
    s.char_indices()
        .nth(idx)
        .map(|it| it.0)
        .ok_or_else(|| InvalidStringIndex(idx, s.chars().count() - 1))
}

fn char_offset_inclusive(s: &str, idx: usize) -> Result<usize, Error> {
    s.char_indices()
        .nth(idx)
        .map(|it| it.0 + it.1.len_utf8())
        .ok_or_else(|| InvalidStringIndex(idx, s.chars().count() - 1))
}

fn char_substring_offset(
    s: &str,
    start: Option<usize>,
    end: Option<usize>,
) -> Result<(usize, usize), Error> {
    let len = s.chars().count();

    if let (Some(start), Some(end)) = (start, end) {
        if start == end {
            return Ok((0, 0));
        }
        if end < start {
            return Err(InvalidSyntax(
                "invalid substring indices: end < start".into(),
            ));
        }
    }

    if start == Some(len) {
        return Ok((0, 0));
    }

    let start = match start {
        Some(start) => char_offset(s, start)?,
        None => 0,
    };

    let end = match end {
        Some(end) => char_offset_inclusive(s, end - 1)?,
        None => s.len(),
    };

    Ok((start, end))
}

pub fn string_list(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(3), "string->list")?;

    let end = match argc {
        3 => Some(pop_index(vm, "string->list")?),
        _ => None,
    };

    let start = match argc {
        2 | 3 => Some(pop_index(vm, "string->list")?),
        _ => None,
    };

    let s = pop_string(vm, "string->list")?;
    let s = s.borrow();
    let s = s.as_str();

    let (start, end) = char_substring_offset(s, start, end)?;
    let substr = &s[start..end];

    let mut list = vm.heap.put(VCell::nil());
    for c in substr.chars().rev() {
        let c = vm.heap.put(VCell::from(c));
        list = vm.heap.put(VCell::pair(c.as_ptr()?, list.as_ptr()?));
    }

    Ok(list)
}

pub fn string_vector(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string->vector")?;

    let s = pop_string(vm, "string->vector")?;
    let s = s.borrow();
    let s = s.as_str();
    let v = s.chars().map(VCell::Char).collect::<Vec<_>>();
    Ok(VCell::vector(v))
}

pub fn vector_string(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "vector->string")?;

    let v = pop_vector(vm)?;
    let mut s = String::with_capacity(v.len());
    for it in 0..v.len() {
        let vcell = vm.heap.get(v.get(it).unwrap());
        s.push(vcell.as_char()?);
    }
    Ok(VCell::string(s))
}

pub fn list_string(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "list->string")?;
    let mut rest = vm.heap.get(vm.stack.pop()?);
    if !rest.is_pair() && !rest.is_nil() {
        return Err(Error::ExpectedPairButFound(vm.heap.get_as_cell(&rest)));
    }
    let mut s = String::new();
    while rest.is_pair() {
        match vm.heap.get(&rest.as_car()?) {
            VCell::Char(c) => s.push(c),
            vcell => {
                return Err(InvalidSyntax(format!(
                    "list->string expected char but found {:#}",
                    vm.heap.get_as_cell(&vcell)
                )));
            }
        }
        rest = vm.heap.get(&rest.as_cdr()?);
    }
    Ok(VCell::string(s))
}

pub fn string_copy(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(3), "string-copy")?;

    let end = match argc {
        3 => Some(pop_index(vm, "string-copy")?),
        _ => None,
    };

    let start = match argc {
        2 | 3 => Some(pop_index(vm, "string-copy")?),
        _ => None,
    };

    let s = pop_string(vm, "string-copy")?;
    let s = s.borrow();
    let s = s.as_str();

    let (start, end) = char_substring_offset(s, start, end)?;
    let substr = &s[start..end];
    Ok(VCell::string(substr))
}

pub fn string_fill(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 2, Some(4), "string-fill")?;

    let end = match argc {
        4 => Some(pop_index(vm, "string-fill")?),
        _ => None,
    };

    let start = match argc {
        3 | 4 => Some(pop_index(vm, "string-fill")?),
        _ => None,
    };

    let c = pop_char(vm)?;

    let s = pop_string(vm, "string-fill")?;
    let mut s = s.borrow_mut();
    let s = s.deref_mut();

    let count = match (start, end) {
        (Some(start), Some(end)) if end >= start => end - start,
        (Some(start), None) => s.chars().count() - start,
        _ => s.chars().count(),
    };

    let (start, end) = char_substring_offset(s, start, end)?;
    let fill = std::iter::repeat_n(c, count).collect::<String>();

    s.replace_range(start..end, &fill);
    Ok(VCell::void())
}

pub fn string_set(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 3, Some(3), "string-set!")?;
    let c = pop_char(vm)?;
    let idx = pop_index(vm, "string-set!")?;
    let s = pop_string(vm, "string-set!")?;
    let mut s = s.borrow_mut();
    let range = s
        .char_indices()
        .nth(idx)
        .ok_or_else(|| InvalidStringIndex(idx, s.chars().count() - 1))
        .map(|it| (it.0, it.0 + it.1.len_utf8()))?;
    s.replace_range(range.0..range.1, &c.to_string());
    Ok(VCell::void())
}

pub fn make_string(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, Some(2), "make-string")?;
    let c = match argc {
        1 => '\0',
        _ => pop_char(vm)?,
    };
    let size = pop_usize(vm)?;
    Ok(VCell::string(
        std::iter::repeat_n(c, size).collect::<String>(),
    ))
}

pub fn string(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "string")?;
    let mut v = vec!['\0'; argc];
    for it in 0..argc {
        *v.get_mut(argc - it - 1).unwrap() = pop_char(vm)?;
    }
    Ok(VCell::string(v.into_iter().collect::<String>()))
}

pub fn string_eq(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string=?", |x, y| x == y)
}

pub fn string_lt(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string<?", |x, y| x < y)
}

pub fn string_gt(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string>?", |x, y| x > y)
}

pub fn string_lt_eq(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string<=?", |x, y| x <= y)
}

pub fn string_gt_eq(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string>=?", |x, y| x >= y)
}

pub fn string_ci_eq(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string-ci=?", |x, y| {
        x.to_lowercase() == y.to_lowercase()
    })
}

pub fn string_ci_lt(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string-ci<?", |x, y| {
        x.to_lowercase() < y.to_lowercase()
    })
}

pub fn string_ci_gt(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string-ci>?", |x, y| {
        x.to_lowercase() > y.to_lowercase()
    })
}

pub fn string_ci_lt_eq(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string-ci<=?", |x, y| {
        x.to_lowercase() <= y.to_lowercase()
    })
}

pub fn string_ci_gt_eq(vm: &mut Vm) -> Result<VCell, Error> {
    string_comp(vm, "string-ci>=?", |x, y| {
        x.to_lowercase() >= y.to_lowercase()
    })
}

fn string_comp(vm: &mut Vm, name: &str, comp: impl Fn(&str, &str) -> bool) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, name)?;
    let mut result = true;

    let mut y = pop_string(vm, name)?;
    for _ in 0..argc - 1 {
        let x = pop_string(vm, name)?;
        {
            let y_s = y.borrow();
            let x_s = x.borrow();
            if !comp(x_s.as_str(), y_s.as_str()) {
                result = false;
            }
        }
        y = x;
    }

    Ok(result.into())
}
