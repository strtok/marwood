use marwood::cell::Cell;
use marwood::lex::scan;
use marwood::parse::parse;
use marwood::syntax::ReplHighlighter;
use marwood::vm::trace::StackTrace;
use marwood::vm::{SystemInterface, Vm};
use marwood::{lex, parse};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Editor, Result};
use rustyline_derive::{Completer, Helper, Hinter};
use std::borrow::Cow::Owned;
use std::time::UNIX_EPOCH;

#[derive(Completer, Helper, Hinter)]
struct InputValidator {
    highlighter: ReplHighlighter,
}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult> {
        match scan(ctx.input()) {
            Ok(tokens) => match parse(ctx.input(), &mut tokens.iter().peekable()) {
                Ok(_) => Ok(ValidationResult::Valid(None)),
                Err(parse::Error::Incomplete) => Ok(ValidationResult::Incomplete),
                Err(_) => Ok(ValidationResult::Valid(None)),
            },
            Err(lex::Error::Incomplete) => Ok(ValidationResult::Incomplete),
            Err(_) => Ok(ValidationResult::Valid(None)),
        }
    }
}

impl Highlighter for InputValidator {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> std::borrow::Cow<'l, str> {
        Owned(
            self.highlighter
                .highlight(line, pos)
                .replace("[1;34m", "[4m"),
        )
    }

    fn highlight_char(&self, line: &str, pos: usize, _forced: bool) -> bool {
        self.highlighter.highlight_check(line, pos + 1)
    }
}

#[derive(Debug)]
struct ReplSystemInterface {
    term_dimensions: (usize, usize),
}
impl SystemInterface for ReplSystemInterface {
    fn display(&self, cell: &Cell) {
        print!("{}", cell);
    }

    fn write(&self, cell: &Cell) {
        print!("{:#}", cell);
    }

    fn terminal_dimensions(&self) -> (usize, usize) {
        self.term_dimensions
    }

    fn time_utc(&self) -> u64 {
        match std::time::SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(n) => n.as_millis() as u64,
            Err(_) => 0,
        }
    }
}

fn main() {
    pretty_env_logger::init();
    let validator = InputValidator {
        highlighter: ReplHighlighter::new(),
    };
    let mut rl = Editor::new().expect("expected an editor");
    rl.set_helper(Some(validator));
    let mut remaining = "".to_string();

    let mut vm = Vm::new();
    let term_dimensions = match rl.dimensions() {
        Some((cols, rows)) => (cols, rows),
        None => (0, 0),
    };
    vm.set_system_interface(Box::new(ReplSystemInterface { term_dimensions }));
    loop {
        let readline = rl.readline_with_initial("> ", (&remaining, ""));
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                remaining = eval(&mut vm, &line).trim().to_string();
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
fn eval<'a>(vm: &mut Vm, text: &'a str) -> &'a str {
    match vm.eval_text(text) {
        Ok((cell, remaining_text)) => {
            match cell {
                Cell::Void => {
                    println!();
                }
                _ => {
                    println!("{:#}", cell);
                }
            }
            remaining_text.unwrap_or("")
        }
        Err(e) => {
            println!("error: {}", e);
            if let Some(trace) = vm.last_stacktrace() {
                print_stacktrace(trace);
            }
            ""
        }
    }
}

fn print_stacktrace(trace: &StackTrace) {
    println!("\nstack trace:");
    for frame in &trace.frames {
        let name = match &frame.name {
            Some(name) => name.to_owned(),
            _ => "λ".to_owned(),
        };

        let desc = match &frame.desc {
            Some(desc) => desc.clone(),
            _ => Cell::Nil,
        };

        match desc {
            Cell::Nil => {
                println!("\t({})", name)
            }
            _ => {
                println!("\t({} {})", name, desc);
            }
        }
    }
}
