#![allow(clippy::unused_unit)]

use marwood::cell::Cell;
use marwood::lex::{scan, Token};
use marwood::parse::parse;
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
        self.parse_and_eval(text)
    }
}

impl Marwood {
    fn parse_and_eval(&mut self, text: &str) -> EvalResult {
        let tokens = match scan(text) {
            Ok(tokens) => tokens,
            Err(e) => {
                return EvalResult::new_error(format!("error: {}", e));
            }
        };

        let mut cur = tokens.iter().peekable();

        let mut result = match parse(text, &mut cur) {
            Ok(cell) => match self.vm.eval(&cell) {
                Ok(Cell::Void) => EvalResult::new_ok(""),
                Ok(cell) => EvalResult::new_ok(format!("{:#}", cell)),
                Err(e) => EvalResult::new_error(format!("error: {}", e)),
            },
            Err(e) => EvalResult::new_error(format!("error: {}", e)),
        };

        result.remaining = match cur.peek() {
            Some(Token { span, .. }) => JsValue::from(&text[span.0..]),
            None => JsValue::null(),
        };

        result
    }
}

#[wasm_bindgen]
pub struct EvalResult {
    ok: JsValue,
    error: JsValue,
    remaining: JsValue,
}

#[wasm_bindgen]
impl EvalResult {
    fn new_error<T: Into<String>>(error: T) -> EvalResult {
        EvalResult {
            ok: JsValue::null(),
            error: JsValue::from(error.into()),
            remaining: JsValue::null(),
        }
    }

    fn new_ok<T: Into<String>>(result: T) -> EvalResult {
        EvalResult {
            ok: JsValue::from(result.into()),
            error: JsValue::null(),
            remaining: JsValue::null(),
        }
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

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    // forward panics to console.error
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
    console::log_1(&"starting Marwood".into());
    Ok(())
}
