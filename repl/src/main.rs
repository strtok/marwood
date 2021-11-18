use lisp::eval::eval;
use lisp::lexer::tokenize;
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
    let tokens = tokenize(text);
    let mut cur = tokens.iter().peekable();

    trace!("tokens: {:?}", tokens);

    while cur.peek().is_some() {
        trace!("passing '{:?}' to parse()", cur.peek());
        match parse(text, &mut cur) {
            Ok(Some(cell)) => {
                trace!("{}", cell);
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
            }
        }
    }
}
