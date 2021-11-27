use lisp::eval::eval;
use lisp::lex::scan;
use lisp::parse::parse;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use xterm_js_rs::addons::fit::FitAddon;
use xterm_js_rs::{OnKeyEvent, Terminal, TerminalOptions, Theme};

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

const KEY_ENTER: u32 = 13;
const KEY_BACKSPACE: u32 = 8;
const KEY_UP_ARROW: u32 = 38;
const KEY_DOWN_ARROW: u32 = 40;
const KEY_LEFT_ARROW: u32 = 37;
const KEY_RIGHT_ARROW: u32 = 39;
const KEY_C: u32 = 67;
const KEY_L: u32 = 76;

const CURSOR_LEFT: &str = "\x1b[D";
const CURSOR_RIGHT: &str = "\x1b[C";

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    // forward panics to console.error
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let terminal: Terminal = Terminal::new(
        TerminalOptions::new()
            .with_rows(50)
            .with_cursor_blink(true)
            .with_cursor_width(10)
            .with_font_size(20)
            .with_draw_bold_text_in_bright_colors(true)
            .with_right_click_selects_word(true)
            .with_theme(
                Theme::new()
                    .with_foreground("#98FB98")
                    .with_background("#000000"),
            ),
    );

    let elem = web_sys::window()
        .unwrap()
        .document()
        .unwrap()
        .get_element_by_id("terminal")
        .unwrap();
    terminal.writeln("");
    terminal.open(elem.dyn_into()?);
    write_prompt(&terminal);

    let mut cursor_col = 0;
    let term: Terminal = terminal.clone().dyn_into()?;

    let mut line = String::new();
    let callback = Closure::wrap(Box::new(move |e: OnKeyEvent| {
        let event = e.dom_event();
        match event.key_code() {
            KEY_ENTER => {
                if !line.is_empty() {
                    term.writeln("");
                    parse_and_eval(&term, &line);
                    line.clear();
                    cursor_col = 0;
                }
                write_prompt(&term);
            }
            KEY_BACKSPACE => {
                if cursor_col > 0 {
                    term.write("\u{0008} \u{0008}");
                    line.pop();
                    cursor_col -= 1;
                }
            }
            KEY_LEFT_ARROW => {
                if cursor_col > 0 {
                    term.write(CURSOR_LEFT);
                    cursor_col -= 1;
                }
            }
            KEY_RIGHT_ARROW => {
                if cursor_col < line.len() {
                    term.write(CURSOR_RIGHT);
                    cursor_col += 1;
                }
            }
            KEY_UP_ARROW | KEY_DOWN_ARROW => {}
            KEY_L if event.ctrl_key() => term.clear(),
            KEY_C if event.ctrl_key() => {
                write_prompt(&term);
                line.clear();
                cursor_col = 0;
            }
            _ => {
                if !event.alt_key() && !event.alt_key() && !event.ctrl_key() && !event.meta_key() {
                    term.write(&event.key());
                    line.push_str(&e.key());
                    cursor_col += 1;
                }
            }
        }
    }) as Box<dyn FnMut(_)>);

    terminal.on_key(callback.as_ref().unchecked_ref());
    callback.forget();

    let addon = FitAddon::new();
    terminal.load_addon(addon.clone().dyn_into::<FitAddon>()?.into());
    addon.fit();
    terminal.focus();

    Ok(())
}

fn write_prompt(term: &Terminal) {
    term.writeln("");
    term.write("> ");
}

fn parse_and_eval(term: &Terminal, text: &str) {
    let tokens = match scan(text) {
        Ok(tokens) => tokens,
        Err(e) => {
            term.writeln(&format!("error: {}", e.error_type));
            return;
        }
    };

    let mut cur = tokens.iter().peekable();

    while cur.peek().is_some() {
        match parse(text, &mut cur) {
            Ok(cell) => {
                match eval(cell) {
                    Ok(cell) => term.writeln(&format!("{}", cell)),
                    Err(e) => term.writeln(&format!("error: {}", e)),
                };
            }
            Err(e) => {
                term.writeln(&format!("error: {}", e));
            }
        }
    }
}
