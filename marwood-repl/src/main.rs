use crossterm::terminal::{disable_raw_mode, enable_raw_mode, is_raw_mode_enabled};
use marwood::cell::Cell;
use marwood::lex::scan;
use marwood::parse::parse;
use marwood::syntax::ReplHighlighter;
use marwood::vm::trace::StackTrace;
use marwood::vm::{SystemInterface, Vm};
use marwood::{lex, parse};
use rustyline::error::ReadlineError;
use rustyline::highlight::{CmdKind, Highlighter};
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Editor, Result};
use rustyline_derive::{Completer, Helper, Hinter};
use std::borrow::Cow::Owned;
use std::cell::RefCell;
use std::io::{Read, Write};
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

    fn highlight_char(&self, line: &str, pos: usize, _forced: CmdKind) -> bool {
        self.highlighter.highlight_check(line, pos + 1)
    }
}

#[derive(Debug)]
struct ReplSystemInterface {
    term_dimensions: (usize, usize),
    peek: RefCell<Option<char>>,
}
impl SystemInterface for ReplSystemInterface {
    fn display(&self, cell: &Cell) {
        print!("{}", cell);
        let _ = std::io::stdout().flush();
    }

    fn write(&self, cell: &Cell) {
        print!("{:#}", cell);
        let _ = std::io::stdout().flush();
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

    fn read_char(&self) -> Option<char> {
        if let Some(c) = self.peek.borrow_mut().take() {
            return Some(c);
        }
        read_one_char()
    }

    fn peek_char(&self) -> Option<char> {
        let mut peek = self.peek.borrow_mut();
        if let Some(c) = *peek {
            return Some(c);
        }
        let c = read_one_char()?;
        *peek = Some(c);
        Some(c)
    }

    fn char_ready(&self) -> bool {
        // Conservative: only report ready if a peeked char is buffered.
        // R7RS allows #f when status is unknown; this never causes a
        // spurious non-blocking guarantee.
        self.peek.borrow().is_some()
    }
}

/// RAII guard that puts the terminal into raw mode for the duration
/// of a read_char call. On drop, restores cooked mode so rustyline's
/// next readline behaves correctly even if a panic unwinds through us.
struct RawModeGuard {
    was_raw: bool,
}

impl RawModeGuard {
    fn new() -> Option<Self> {
        let was_raw = is_raw_mode_enabled().unwrap_or(false);
        if !was_raw {
            enable_raw_mode().ok()?;
        }
        Some(RawModeGuard { was_raw })
    }
}

impl Drop for RawModeGuard {
    fn drop(&mut self) {
        if !self.was_raw {
            let _ = disable_raw_mode();
        }
    }
}

/// Read one Unicode scalar value from stdin in raw mode. Ctrl-D
/// returns None (EOF). Ctrl-C exits the process with status 130.
fn read_one_char() -> Option<char> {
    let _guard = RawModeGuard::new();
    let mut stdin = std::io::stdin().lock();
    let mut buf = [0u8; 4];
    let mut len = 0;
    loop {
        if stdin.read(&mut buf[len..len + 1]).ok()? == 0 {
            return None;
        }
        // Handle terminal control bytes on the first byte of a char.
        if len == 0 {
            match buf[0] {
                0x03 => {
                    // Ctrl-C: restore terminal then terminate.
                    drop(_guard);
                    std::process::exit(130);
                }
                0x04 => return None, // Ctrl-D
                _ => {}
            }
        }
        len += 1;
        // Try to decode whatever we have so far as UTF-8.
        match std::str::from_utf8(&buf[..len]) {
            Ok(s) => return s.chars().next(),
            Err(_) if len < 4 => continue,
            Err(_) => return None,
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
    vm.set_system_interface(Box::new(ReplSystemInterface {
        term_dimensions,
        peek: RefCell::new(None),
    }));
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
                    let width = vm.term_cols();
                    let width = if width == 0 { 80 } else { width };
                    println!("{}", marwood::pretty::format(&cell, width));
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
