use marwood_wasm::Marwood;
use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn auto_completion_primitive() {
    let marwood = Marwood::new();
    let result = marwood.autocomplete("defi");
    assert!(result.completions().contains(&JsValue::from("define")));
}

#[wasm_bindgen_test]
fn auto_completion_builtin() {
    let marwood = Marwood::new();
    let result = marwood.autocomplete("char");
    assert!(result.completions().contains(&JsValue::from("char=?")));
}

#[wasm_bindgen_test]
fn auto_completion_dynamic() {
    let marwood = Marwood::new();
    let result = marwood.autocomplete("(lambda (foobar) foo");
    assert!(result
        .completions()
        .contains(&JsValue::from("(lambda (foobar) foobar")));
}
