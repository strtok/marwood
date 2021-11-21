use crate::cell::Cell;

pub fn eval(cell: Cell) -> Result<Cell, String> {
    match cell {
        Cell::Cons(car, cdr) => match *car {
            Cell::Symbol(s) if s.eq("+") => eval_add(*cdr),
            Cell::Symbol(s) if s.eq("*") => eval_mul(*cdr),
            Cell::Symbol(s) if s.eq("quote") => eval_quote(*cdr),
            _ => Err(format!("{} is not a known procedure", car)),
        },
        Cell::Nil => Err("invalid syntax ()".into()),
        _ => Ok(cell),
    }
}

pub fn eval_quote(cdr: Cell) -> Result<Cell, String> {
    match cdr {
        Cell::Cons(car, cdr) => {
            return if cdr.is_nil() {
                Ok(*car)
            } else {
                Err(format!("invalid syntax for quote"))
            }
        }
        _ => Err("invalid syntax for quote".into()),
    }
}

pub fn eval_add(cdr: Cell) -> Result<Cell, String> {
    let mut val = 0;
    for cell in cdr {
        let cell = eval(cell)?;
        val += cell
            .as_number()
            .ok_or_else(|| format!("{} is not a number", cell))?;
    }

    Ok(Cell::Number(val))
}

pub fn eval_mul(cdr: Cell) -> Result<Cell, String> {
    let mut val = 1;
    for cell in cdr {
        let cell = eval(cell)?;
        val *= cell
            .as_number()
            .ok_or_else(|| format!("{} is not a number", cell))?;
    }
    Ok(Cell::Number(val))
}
