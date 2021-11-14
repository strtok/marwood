use lisp::eval::eval;
use lisp::parse;
use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                parse_and_eval(&line);
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("error: {:?}", err);
                break;
            }
        }
    }
}

fn parse_and_eval(mut line: &str) {
    while !line.is_empty() {
        match parse::expression(line) {
            Ok((rest, Some(cell))) => {
                line = rest;
                match eval(cell) {
                    Ok(cell) => println!("{}", cell),
                    Err(e) => eprintln!("{}", e),
                };
            }
            Ok((_, None)) => {
                eprintln!("'{}' is not an expression", line);
                break;
            }
            Err(e) => {
                eprintln!("'{}' could not be parsed", e);
                break;
            }
        }
    }
}
