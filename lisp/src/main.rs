use rustyline::error::ReadlineError;
use rustyline::Editor;

mod cell;
mod parser;

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => match parser::expression(&line) {
                Ok((_, Some(cell))) => {
                    println!("{:?}", cell);
                }
                Ok((_, None)) => {
                    eprintln!("error: expected expression");
                }
                Err(e) => {
                    eprintln!("error parsing '{}'", e);
                }
            },
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                println!("error: {:?}", err);
                break;
            }
        }
    }
}
