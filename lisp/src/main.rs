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
                let mut remaining = line.as_str();
                while !remaining.is_empty() {
                    match parser::expression(&remaining) {
                        Ok((rest, Some(cell))) => {
                            remaining = rest;
                            println!("{:?}", cell);
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
