use crate::builtin::*;
use crate::cell::Cell;

pub fn eval(cell: Cell) -> Result<Cell, String> {
    match cell {
        Cell::Cons(car, cdr) => match *car {
            Cell::Symbol(s) if s.eq("cons") => cons(*cdr),
            Cell::Symbol(s) if s.eq("car") => crate::builtin::car(*cdr),
            Cell::Symbol(s) if s.eq("cdr") => crate::builtin::cdr(*cdr),
            Cell::Symbol(s) if s.eq("+") => add(*cdr),
            Cell::Symbol(s) if s.eq("*") => mul(*cdr),
            Cell::Symbol(s) if s.eq("quote") => quote(*cdr),
            _ => Err(format!("{} is not a known procedure", car)),
        },
        Cell::Nil => Err("invalid syntax ()".into()),
        _ => Ok(cell),
    }
}

#[cfg(test)]
mod tests {}
