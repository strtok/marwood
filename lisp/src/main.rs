use crate::cell::Cell;
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod cell;
mod parser;

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                parse_and_eval(&line);
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                println!("error: {:?}", err);
                break;
            }
        }
    }
}

fn parse_and_eval(mut line: &str) {
    while !line.is_empty() {
        match parser::expression(line) {
            Ok((rest, Some(cell))) => {
                line = rest;
                match eval(cell) {
                    Ok(cell) => println!("{}", cell),
                    Err(e) => eprintln!("{}", e),
                };
            }
            Ok((_, None)) => {
                eprintln!("error: expected expression");
                break;
            }
            Err(e) => {
                eprintln!("error parsing '{}'", e);
                break;
            }
        }
    }
}

fn eval(cell: Cell) -> Result<Cell, String> {
    match cell {
        Cell::Cons(car, cdr) => match *car {
            Cell::Symbol(s) if s.eq("+") => eval_add(*cdr),
            Cell::Symbol(s) if s.eq("*") => eval_mul(*cdr),
            _ => Err(format!("'{}' is not a known procedure", car)),
        },
        _ => Ok(cell),
    }
}

fn eval_add(cdr: Cell) -> Result<Cell, String> {
    let mut val = 0;
    for cell in cdr {
        let cell = eval(cell)?;
        val += cell
            .as_number()
            .ok_or_else(|| format!("'{}' is not a number", cell))?;
    }

    Ok(Cell::Number(val))
}

fn eval_mul(cdr: Cell) -> Result<Cell, String> {
    let mut val = 1;
    for cell in cdr {
        let cell = eval(cell)?;
        val *= cell
            .as_number()
            .ok_or_else(|| format!("'{}' is not a number", cell))?;
    }
    Ok(Cell::Number(val))
}
