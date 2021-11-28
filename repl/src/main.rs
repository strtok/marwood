use lisp::eval::eval;
use lisp::lex::{scan, Token};
use lisp::parse;
use lisp::parse::parse;
use log::trace;
use rustyline::error::ReadlineError;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Editor, Result};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

#[derive(Completer, Helper, Highlighter, Hinter)]
struct InputValidator {}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult> {
        match scan(ctx.input()) {
            Ok(tokens) => match parse(ctx.input(), &mut tokens.iter().peekable()) {
                Ok(_) => Ok(ValidationResult::Valid(None)),
                Err(parse::Error::Eof) => Ok(ValidationResult::Incomplete),
                Err(_) => Ok(ValidationResult::Valid(None)),
            },
            Err(_) => Ok(ValidationResult::Valid(None)),
        }
    }
}

fn main() {
    pretty_env_logger::init();
    let validator = InputValidator {};
    let mut rl = Editor::new();
    rl.set_helper(Some(validator));
    let mut remaining = "".to_string();

    loop {
        let readline = rl.readline_with_initial("> ", (&remaining, ""));
        match readline {
            Ok(line) => {
                remaining = parse_and_eval(&line).trim().to_string();
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("error: {:#?}", err);
                break;
            }
        }
    }
}

/// Evaluate one expression from the input text and return
/// any text that was not evaluated.
fn parse_and_eval(text: &str) -> &str {
    // Tokenize the entire input
    let tokens = match scan(text) {
        Ok(tokens) => tokens,
        Err(e) => {
            println!("error: {}", e.error_type);
            return "";
        }
    };

    // Parse one expression from the token stream
    let mut cur = tokens.iter().peekable();
    trace!("lexer: {:?}", tokens);
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
        }
    }

    // Given the position of the next token in the stream, determine
    // how far the parser consumed and return the remaining text
    match cur.peek() {
        Some(Token { span, .. }) => &text[span.0..],
        None => "",
    }
}
