#![allow(clippy::unused_unit)]

use marwood::cell::Cell;
use marwood::lex::scan;
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

    pub fn eval(&mut self, text: &str) -> String {
        self.parse_and_eval(text)
    }
}

impl Marwood {
    fn parse_and_eval(&mut self, text: &str) -> String {
        let mut result = String::new();

        let tokens = match scan(text) {
            Ok(tokens) => tokens,
            Err(e) => {
                result.push_str(&format!("error: {}", e));
                return result;
            }
        };

        let mut cur = tokens.iter().peekable();

        while cur.peek().is_some() {
            match parse(text, &mut cur) {
                Ok(cell) => {
                    match self.vm.eval(&cell) {
                        Ok(Cell::Void) => {}
                        Ok(cell) => result.push_str(&format!("{:#}", cell)),
                        Err(e) => result.push_str(&format!("error: {}", e)),
                    };
                }
                Err(e) => {
                    result.push_str(&format!("error: {}", e));
                }
            }

            if cur.peek().is_some() {
                result.push('\n');
            }
        }

        result
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
