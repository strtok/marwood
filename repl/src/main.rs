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
            println!("error: {}", e.error_type);
            return;
        }
    };

    let mut cur = tokens.iter().peekable();

    trace!("lexer: {:?}", tokens);

    while cur.peek().is_some() {
        match parse(text, &mut cur) {
            Ok(cell) => {
                trace!("parser: {}", cell);
                match eval(cell) {
                    Ok(cell) => println!("{}", cell),
                    Err(e) => println!("error: {}", e),
                };
            }
            Err(e) => {
                println!("error: {}", e);
                break;
            }
        }
    }
}
