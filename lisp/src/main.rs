use crate::cell::Cell;
use crate::cell::Cell::Number;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::ops::Deref;

mod cell;
mod parser;

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let mut remaining = line.as_str();
                while !remaining.is_empty() {
                    match parser::expression(remaining) {
                        Ok((rest, Some(cell))) => {
                            remaining = rest;
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
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                println!("error: {:?}", err);
                break;
            }
        }
    }
}

fn eval_add(cdr: Cell) -> Result<Cell, String> {
    let mut val = 0;
    for cell in cdr {
        let cell = eval(cell)?;
        match cell {
            Cell::Number(n) => {
                val = val + n;
            }
            _ => {
                return Err(format!("'{}' is not a number", cell));
            }
        }
    }
    return Ok(Cell::Number(val));
}

fn eval_mul(cdr: Cell) -> Result<Cell, String> {
    let mut val = 1;
    for cell in cdr {
        let cell = eval(cell)?;
        match cell {
            Cell::Number(n) => {
                val = val * n;
            }
            _ => {
                return Err(format!("'{}' is not a number", cell));
            }
        }
    }
    return Ok(Cell::Number(val));
}

fn eval(cell: Cell) -> Result<Cell, String> {
    match cell {
        Cell::Cons(car, cdr) => match *car {
            Cell::Symbol(s) => match s.as_str() {
                "+" => eval_add(*cdr),
                "*" => eval_mul(*cdr),
                _ => Err(format!("'{}' is not a known procedure", s)),
            },
            _ => Err(format!("'{}' is not a known procedure", car)),
        },
        _ => Ok(cell),
    }
}
