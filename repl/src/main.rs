use lisp::eval::eval;
use lisp::lex::scan;
use lisp::parse::parse;
use log::trace;
use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    pretty_env_logger::init();
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                parse_and_eval(&line);
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("error: {:#?}", err);
                break;
            }
        }
    }
}

fn parse_and_eval(text: &str) {
    let tokens = match scan(text) {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("error: {}", e.error_type);
            return;
        }
    };

    let mut cur = tokens.iter().peekable();

    trace!("lexer: {:?}", tokens);

    while cur.peek().is_some() {
        match parse(text, &mut cur) {
            Ok(Some(cell)) => {
                trace!("parser: {}", cell);
                match eval(cell) {
                    Ok(cell) => println!("{}", cell),
                    Err(e) => eprintln!("error: {}", e),
                };
            }
            Ok(None) => {
                trace!("parse() returned none");
                break;
            }
            Err(_) => {
                eprintln!("error: ()");
                break;
            }
        }
    }
}
