#![allow(clippy::unused_unit)]

use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;
use wasm_bindgen::prelude::*;
use web_sys::console;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

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
    pub fn new() -> Marwood {
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

        let mut result = match parse::parse(text, &mut cur) {
            Ok(cell) => match self.vm.eval(&cell) {
                Ok(Cell::Void) => EvalResult::new_ok(""),
                Ok(cell) => EvalResult::new_ok(format!("{:#}", cell)),
                Err(e) => EvalResult::new_error(format!("error: {}", e)),
            },
            Err(parse::Error::Eof) => return EvalResult::new_eof(),
            Err(e) => EvalResult::new_error(format!("error: {}", e)),
        };

        result.remaining = match cur.peek() {
            Some(lex::Token { span, .. }) => JsValue::from(&text[span.0..]),
            None => JsValue::null(),
        };

        result
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

    pub fn autocomplete(&self, text: &str, word: &str) -> AutocompleteResult {
        // Include any symbols already referenced, except the one currently being completed.
        let symbols = match lex::scan(text) {
            Ok(tokens) => tokens
                .iter()
                .filter(|it| it.is_symbol())
                .map(|it| it.span(text))
                .filter(|sym| *sym != word)
                .collect(),
            _ => vec![],
        };

        let mut result = AutocompleteResult::new();
        symbols
            .iter()
            .for_each(|sym| result.completions.push(JsValue::from(*sym)));
        self.vm
            .global_symbols()
            .iter()
            .for_each(|sym| result.completions.push(JsValue::from(*sym)));
        ["define", "quote", "define-syntax", "lambda", "if", "set!"]
            .iter()
            .for_each(|sym| result.completions.push(JsValue::from(*sym)));
        result
    }
}

#[wasm_bindgen]
pub struct EvalResult {
    ok: JsValue,
    error: JsValue,
    remaining: JsValue,
    eof: bool,
}

#[wasm_bindgen]
impl EvalResult {
    fn new_error<T: Into<String>>(error: T) -> EvalResult {
        EvalResult {
            ok: JsValue::null(),
            error: JsValue::from(error.into()),
            remaining: JsValue::null(),
            eof: false,
        }
    }

    fn new_ok<T: Into<String>>(result: T) -> EvalResult {
        EvalResult {
            ok: JsValue::from(result.into()),
            error: JsValue::null(),
            remaining: JsValue::null(),
            eof: false,
        }
    }

    fn new_eof() -> EvalResult {
        EvalResult {
            ok: JsValue::null(),
            error: JsValue::null(),
            remaining: JsValue::null(),
            eof: true,
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

#[wasm_bindgen(start)]
pub fn run() -> Result<(), JsValue> {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
    console::log_1(&"starting Marwood".into());
    Ok(())
}
