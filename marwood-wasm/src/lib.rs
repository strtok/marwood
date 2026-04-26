#![allow(clippy::unused_unit)]

use js_sys::{Atomics, Date, Int32Array, SharedArrayBuffer};
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::syntax::ReplHighlighter;
use marwood::vm::{SystemInterface, Vm};
use std::borrow::Cow;
use wasm_bindgen::prelude::*;

// Wire protocol for the worker<->main sync RPC channel. Indices into a
// SharedArrayBuffer-backed Int32Array. The worker writes a request,
// notifies the main thread, then Atomics.wait()s on STATE until the
// main thread fills in the result and flips STATE to RESPONSE.
const SLOT_STATE: u32 = 0;
const SLOT_OPCODE: u32 = 1;
const SLOT_RESULT_A: u32 = 2; // char codepoint, or 0/1 for char-ready
const SLOT_RESULT_B: u32 = 3; // 0 = have value, 1 = EOF (read/peek only)

const STATE_IDLE: i32 = 0;
const STATE_REQUEST: i32 = 1;
const STATE_RESPONSE: i32 = 2;

const OP_READ_CHAR: i32 = 1;
const OP_PEEK_CHAR: i32 = 2;
const OP_CHAR_READY: i32 = 3;

#[wasm_bindgen(module = "/display.js")]
extern "C" {
    fn display(text: &str);
    fn termRows() -> JsValue;
    fn termCols() -> JsValue;
}

#[wasm_bindgen]
#[derive(Default)]
pub struct Marwood {
    vm: Vm,
    hl: ReplHighlighter,
}

#[derive(Debug)]
struct WasmSystemInterface {
    /// Int32Array view over a SharedArrayBuffer. When set, blocking
    /// input methods perform a sync RPC to the main thread. When None,
    /// they fall back to EOF.
    input: Option<Int32Array>,
}

impl WasmSystemInterface {
    fn rpc(&self, opcode: i32) -> Option<(i32, i32)> {
        let arr = self.input.as_ref()?;
        Atomics::store(arr, SLOT_OPCODE, opcode).ok()?;
        Atomics::store(arr, SLOT_RESULT_A, 0).ok()?;
        Atomics::store(arr, SLOT_RESULT_B, 0).ok()?;
        Atomics::store(arr, SLOT_STATE, STATE_REQUEST).ok()?;
        Atomics::notify(arr, SLOT_STATE).ok()?;
        loop {
            let cur = Atomics::load(arr, SLOT_STATE).ok()?;
            if cur == STATE_RESPONSE {
                break;
            }
            // Atomics.wait returns "ok" / "not-equal" / "timed-out";
            // we ignore the result and re-check the state.
            let _ = Atomics::wait(arr, SLOT_STATE, cur);
        }
        let a = Atomics::load(arr, SLOT_RESULT_A).ok()?;
        let b = Atomics::load(arr, SLOT_RESULT_B).ok()?;
        Atomics::store(arr, SLOT_STATE, STATE_IDLE).ok()?;
        Some((a, b))
    }
}

impl SystemInterface for WasmSystemInterface {
    fn display(&self, cell: &Cell) {
        display(&format!("{}", cell))
    }

    fn write(&self, cell: &Cell) {
        display(&format!("{:#}", cell))
    }

    fn terminal_dimensions(&self) -> (usize, usize) {
        (
            termCols().as_f64().unwrap_or(0_f64) as usize,
            termRows().as_f64().unwrap_or(0_f64) as usize,
        )
    }

    fn time_utc(&self) -> u64 {
        Date::now() as u64
    }

    fn read_char(&self) -> Option<char> {
        let (a, b) = self.rpc(OP_READ_CHAR)?;
        if b != 0 {
            None
        } else {
            char::from_u32(a as u32)
        }
    }

    fn peek_char(&self) -> Option<char> {
        let (a, b) = self.rpc(OP_PEEK_CHAR)?;
        if b != 0 {
            None
        } else {
            char::from_u32(a as u32)
        }
    }

    fn char_ready(&self) -> bool {
        match self.rpc(OP_CHAR_READY) {
            Some((a, _)) => a != 0,
            None => false,
        }
    }
}

#[wasm_bindgen]
impl Marwood {
    pub fn new() -> Self {
        #[cfg(feature = "console_error_panic_hook")]
        console_error_panic_hook::set_once();
        let mut vm = Vm::new();
        vm.set_system_interface(Box::new(WasmSystemInterface { input: None }));
        Marwood {
            vm,
            hl: ReplHighlighter::new(),
        }
    }

    /// Construct a Marwood instance backed by a SharedArrayBuffer for
    /// blocking input via Atomics.wait. The buffer must be at least 16
    /// bytes (4 Int32 slots) and is shared with the main thread, which
    /// services read-char / peek-char / char-ready requests.
    pub fn new_with_shared(buffer: SharedArrayBuffer) -> Self {
        #[cfg(feature = "console_error_panic_hook")]
        console_error_panic_hook::set_once();
        let mut vm = Vm::new();
        let arr = Int32Array::new(&buffer);
        vm.set_system_interface(Box::new(WasmSystemInterface { input: Some(arr) }));
        Marwood {
            vm,
            hl: ReplHighlighter::new(),
        }
    }

    pub fn eval(&mut self, text: &str, count: usize) -> EvalResult {
        let tokens = match lex::scan(text) {
            Ok(tokens) => tokens,
            Err(lex::Error::Incomplete) => {
                return EvalResult::new_eof();
            }
            Err(e) => {
                return EvalResult::new_error(format!("error: {}", e));
            }
        };

        let mut cur = tokens.iter().peekable();

        match parse::parse(text, &mut cur) {
            Ok(cell) => match self.vm.prepare_eval(&cell) {
                Ok(()) => {}
                Err(e) => return EvalResult::new_error(format!("error: {}", e)),
            },
            Err(parse::Error::Incomplete) => return EvalResult::new_eof(),
            Err(e) => return EvalResult::new_error(format!("error: {}", e)),
        };

        let mut result = self.eval_continue(count);
        result.remaining = match cur.peek() {
            Some(lex::Token { span, .. }) => JsValue::from(&text[span.0..]),
            None => JsValue::null(),
        };

        result
    }

    pub fn eval_continue(&mut self, count: usize) -> EvalResult {
        match self.vm.run_count(count) {
            Ok(Some(Cell::Void)) => EvalResult::new_ok(""),
            Ok(Some(cell)) => EvalResult::new_ok(format!("{:#}", cell)),
            Ok(None) => EvalResult::new_not_completed(),
            Err(e) => EvalResult::new_error(format!(
                "error: {}\ntrace: \n{}",
                e,
                self.build_stacktrace()
            )),
        }
    }

    fn build_stacktrace(&self) -> String {
        let mut trace_text = String::new();
        let trace = match self.vm.last_stacktrace() {
            Some(trace) => trace,
            None => {
                return "".into();
            }
        };

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
                    trace_text = trace_text + &format!("\t({})\n", name);
                }
                _ => {
                    trace_text = trace_text + &format!("\t({} {})\n", name, desc);
                }
            }
        }

        trace_text
    }

    pub fn check(&self, text: &str) -> CheckResult {
        let tokens = match lex::scan(text) {
            Ok(tokens) => tokens,
            Err(lex::Error::Incomplete) => {
                return CheckResult::new(true);
            }
            Err(_) => {
                return CheckResult::new(false);
            }
        };

        let mut cur = tokens.iter().peekable();
        CheckResult::new(matches!(
            parse::parse(text, &mut cur),
            Err(parse::Error::Incomplete)
        ))
    }

    pub fn highlight_check(&self, text: &str, index: usize) -> JsValue {
        JsValue::from(self.hl.highlight_check(text, index))
    }

    pub fn highlight(&self, text: &str, index: usize) -> HighlightResult {
        let result = self.hl.highlight(text, index);
        match result {
            Cow::Borrowed(text) => HighlightResult::new(text.to_owned(), false),
            Cow::Owned(text) => HighlightResult::new(text, true),
        }
    }

    pub fn last_token(&self, text: &str) -> JsValue {
        match lex::scan(text) {
            Ok(tokens) => tokens
                .last()
                .map(|it| JsValue::from(it.span(text)))
                .unwrap_or_else(JsValue::null),
            _ => JsValue::null(),
        }
    }

    pub fn autocomplete(&self, text: &str) -> AutocompleteResult {
        let mut result = AutocompleteResult::new();

        if text.is_empty() || text.chars().last().unwrap().is_whitespace() {
            return result;
        }

        let tokens = match lex::scan(text) {
            Ok(tokens) => tokens,
            _ => {
                return result;
            }
        };

        let word_token = tokens.iter().last().unwrap();
        let word = word_token.span(text);
        let prefix = word_token.span_prefix(text);

        // Include any symbols already referenced, except the one currently being completed.
        let symbols = tokens
            .iter()
            .filter(|it| it.is_symbol())
            .map(|it| it.span(text))
            .filter(|sym| *sym != word)
            .filter(|sym| sym.starts_with(word))
            .collect::<Vec<_>>();

        symbols.iter().for_each(|sym| {
            result
                .completions
                .push(JsValue::from(format!("{}{}", prefix, *sym)))
        });

        self.vm
            .global_symbols()
            .iter()
            .filter(|sym| sym.starts_with(word))
            .for_each(|sym| {
                result
                    .completions
                    .push(JsValue::from(format!("{}{}", prefix, *sym)))
            });

        ["define", "quote", "define-syntax", "lambda", "if", "set!"]
            .iter()
            .filter(|sym| sym.starts_with(word))
            .for_each(|sym| {
                result
                    .completions
                    .push(JsValue::from(format!("{}{}", prefix, *sym)))
            });

        result
    }
}

#[wasm_bindgen]
pub struct EvalResult {
    ok: JsValue,
    error: JsValue,
    remaining: JsValue,
    eof: bool,
    completed: bool,
}

#[wasm_bindgen]
impl EvalResult {
    fn new_error<T: Into<String>>(error: T) -> EvalResult {
        EvalResult {
            ok: JsValue::null(),
            error: JsValue::from(error.into()),
            remaining: JsValue::null(),
            eof: false,
            completed: true,
        }
    }

    fn new_ok<T: Into<String>>(result: T) -> EvalResult {
        EvalResult {
            ok: JsValue::from(result.into()),
            error: JsValue::null(),
            remaining: JsValue::null(),
            eof: false,
            completed: true,
        }
    }

    fn new_eof() -> EvalResult {
        EvalResult {
            ok: JsValue::null(),
            error: JsValue::null(),
            remaining: JsValue::null(),
            eof: true,
            completed: true,
        }
    }

    fn new_not_completed() -> EvalResult {
        EvalResult {
            ok: JsValue::null(),
            error: JsValue::null(),
            remaining: JsValue::null(),
            eof: false,
            completed: false,
        }
    }

    #[wasm_bindgen(getter)]
    pub fn eof(&self) -> JsValue {
        JsValue::from(self.eof)
    }

    #[wasm_bindgen(getter)]
    pub fn ok(&self) -> JsValue {
        self.ok.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn error(&self) -> JsValue {
        self.error.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn remaining(&self) -> JsValue {
        self.remaining.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn completed(&self) -> JsValue {
        JsValue::from(self.completed)
    }
}

#[wasm_bindgen]
pub struct CheckResult {
    eof: bool,
}

#[wasm_bindgen]
impl CheckResult {
    fn new(eof: bool) -> CheckResult {
        CheckResult { eof }
    }

    #[wasm_bindgen(getter)]
    pub fn eof(&self) -> JsValue {
        JsValue::from(self.eof)
    }
}

#[wasm_bindgen]
pub struct HighlightResult {
    text: String,
    highlighted: bool,
}

#[wasm_bindgen]
impl HighlightResult {
    fn new(text: String, highlighted: bool) -> HighlightResult {
        HighlightResult { text, highlighted }
    }

    #[wasm_bindgen(getter)]
    pub fn highlighted(&self) -> JsValue {
        JsValue::from(self.highlighted)
    }

    #[wasm_bindgen(getter)]
    pub fn text(&self) -> JsValue {
        JsValue::from(self.text.clone())
    }
}

#[wasm_bindgen]
pub struct AutocompleteResult {
    completions: Vec<JsValue>,
}

#[wasm_bindgen]
impl AutocompleteResult {
    fn new() -> AutocompleteResult {
        AutocompleteResult {
            completions: vec![],
        }
    }

    #[wasm_bindgen(getter)]
    pub fn completions(&self) -> Box<[JsValue]> {
        self.completions.clone().into_boxed_slice()
    }
}
