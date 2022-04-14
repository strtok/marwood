use crate::error::Error;
use crate::vm::builtin::{pop_argc, pop_string, pop_symbol};
use crate::vm::vcell::VCell;
use crate::vm::Vm;
use crate::{lex, parse};

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("string->symbol", string_symbol);
    vm.load_builtin("symbol->string", symbol_string);
    vm.load_builtin("symbol=?", symbol_eq);
}

pub fn string_symbol(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "string->symbol")?;
    let s = pop_string(vm, "string->append")?;
    let s = s.borrow();
    let s = s.as_str();
    let sym = s
        .char_indices()
        .map(|(idx, c)| match c {
            c if idx == 0 && lex::is_initial_identifier(c) => c.to_string(),
            c if idx > 0 && lex::is_subsequent_identifier(c) => c.to_string(),
            c => format!("\\x{:x};", c as u32),
        })
        .collect::<String>();
    Ok(VCell::symbol(sym))
}

pub fn symbol_string(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "symbol->string")?;
    let sym = pop_symbol(vm, "symbol->string")?;
    let s = parse::parse_string(sym.as_str())?;
    Ok(VCell::string(s.to_string()))
}

fn symbol_eq(vm: &mut Vm) -> Result<VCell, Error> {
    let argc = pop_argc(vm, 1, None, "symbol=?")?;
    let mut result = true;

    let mut y = pop_symbol(vm, "symbol=?")?;
    for _ in 0..argc - 1 {
        let x = pop_symbol(vm, "symbol=?")?;
        {
            if x.as_str() != y.as_str() {
                result = false;
            }
        }
        y = x;
    }

    Ok(result.into())
}
