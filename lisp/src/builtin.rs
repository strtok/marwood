use crate::cell::Cell;
use crate::cons;
use crate::eval::eval;

pub fn quote(cdr: Cell) -> Result<Cell, String> {
    match cdr {
        Cell::Cons(car, cdr) => {
            if cdr.is_nil() {
                Ok(*car)
            } else {
                Err("invalid syntax for quote".into())
            }
        }
        _ => Err("invalid syntax for quote".into()),
    }
}

pub fn car(cdr: Cell) -> Result<Cell, String> {
    let l = eval(cdr.car().ok_or("invalid syntax for car")?.clone())?;
    let elt = l.car().ok_or("invalid syntax for car")?;
    Ok(elt.clone())
}

pub fn cdr(cdr: Cell) -> Result<Cell, String> {
    let l = eval(cdr.car().ok_or("invalid syntax for car")?.clone())?;
    let elt = l.cdr().ok_or("invalid syntax for car")?;
    Ok(elt.clone())
}

pub fn cons(cdr: Cell) -> Result<Cell, String> {
    let arg1 = cdr.car().ok_or("invalid syntax for cons")?;
    let arg2 = cdr.cdr().ok_or("invalid syntax for cons")?;
    let arg2 = arg2.car().ok_or("invalid syntax for cons")?;

    Ok(cons![eval(arg1.clone())?, eval(arg2.clone())?])
}

pub fn add(cdr: Cell) -> Result<Cell, String> {
    let mut val = 0;
    for cell in cdr {
        let cell = eval(cell)?;
        val += cell
            .as_number()
            .ok_or_else(|| format!("{} is not a number", cell))?;
    }

    Ok(Cell::Number(val))
}

pub fn mul(cdr: Cell) -> Result<Cell, String> {
    let mut val = 1;
    for cell in cdr {
        let cell = eval(cell)?;
        val *= cell
            .as_number()
            .ok_or_else(|| format!("{} is not a number", cell))?;
    }
    Ok(Cell::Number(val))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex;
    use crate::parse;

    macro_rules! evals {
        ($($lhs:expr => $rhs:expr),+) => {{
             $(
                assert_eq!(eval(parse!($lhs)), Ok(parse!($rhs)));
             )+
        }};
    }

    macro_rules! fails {
        ($($lhs:expr),+) => {{
             $(
                assert!(matches!(eval(
                    parse!($lhs)
                ), Err(_)));
             )+
        }};
    }

    #[test]
    fn cons() {
        evals! {
            "(cons 1 2)" => "(1 . 2)",
            "(cons 1 '())" => "(1)",
            "(cons 1 2 3)" => "(1 . 2)"
        }

        fails!["(cons)", "(cons 1)"]
    }

    #[test]
    fn quote() {
        evals! {
            "(quote (1 2 3))" => "(1 2 3)"
        };
    }

    #[test]
    fn car_and_cdr() {
        evals! {
            "(car '(1 2 3))" => "1",
            "(cdr '(1 2 3))" => "(2 3)"
        };
    }

    #[test]
    fn add() {
        evals! {
            "(+)" => "0",
            "(+ 10)" => "10",
            "(+ 10 20)" => "30",
            "(+ 10 20 30)" => "60"
        };

        fails!["(+ 'foo)"];
    }

    #[test]
    fn multiple() {
        evals! {
            "(*)" => "1",
            "(* 10)" => "10",
            "(* 10 20)" => "200",
            "(* 10 20 30)" => "6000"
        };

        fails!["(* 'foo)"];
    }
}
