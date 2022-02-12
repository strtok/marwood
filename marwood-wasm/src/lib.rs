#![allow(clippy::unused_unit)]
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;
use wasm_bindgen::prelude::*;

// #[cfg(feature = "wee_alloc")]
// #[global_allocator]
// static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

/// How many instructions to execute per eval/eval_continue()
/// call.
const INSTRUCTIONS_PER_EVAL: usize = 50000;

#[wasm_bindgen(module = "/display.js")]
extern "C" {
    fn display(text: &str);
}

#[wasm_bindgen]
#[derive(Default)]
pub struct Marwood {
    vm: Vm,
}

#[wasm_bindgen]
impl Marwood {
    pub fn new() -> Self {
        #[cfg(feature = "console_error_panic_hook")]
        console_error_panic_hook::set_once();
        let mut vm = Vm::new();
        vm.set_display_fn(Box::new(|cell| display(&cell.to_string())));
        Marwood { vm }
    }

    pub fn eval(&mut self, text: &str) -> EvalResult {
        let tokens = match lex::scan(text) {
            Ok(tokens) => tokens,
            Err(lex::Error::Eof) => {
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
            Err(parse::Error::Eof) => return EvalResult::new_eof(),
            Err(e) => return EvalResult::new_error(format!("error: {}", e)),
        };

        let mut result = self.eval_continue();
        result.remaining = match cur.peek() {
            Some(lex::Token { span, .. }) => JsValue::from(&text[span.0..]),
            None => JsValue::null(),
        };

        result
    }

    pub fn eval_continue(&mut self) -> EvalResult {
        match self.vm.run_count(INSTRUCTIONS_PER_EVAL) {
            Ok(Some(Cell::Void)) => EvalResult::new_ok(""),
            Ok(Some(cell)) => EvalResult::new_ok(format!("{:#}", cell)),
            Ok(None) => EvalResult::new_not_completed(),
            Err(e) => EvalResult::new_error(format!("error: {}", e)),
        }
    }

    pub fn check(&self, text: &str) -> CheckResult {
        let tokens = match lex::scan(text) {
            Ok(tokens) => tokens,
            Err(lex::Error::Eof) => {
                return CheckResult::new(true);
            }
            Err(_) => {
                return CheckResult::new(false);
            }
        };

        let mut cur = tokens.iter().peekable();
        CheckResult::new(matches!(
            parse::parse(text, &mut cur),
            Err(parse::Error::Eof)
        ))
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
