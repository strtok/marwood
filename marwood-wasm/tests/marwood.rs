use marwood_wasm::Marwood;
use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn xxx() {
    let marwood = Marwood::new();
    let result = marwood.autocomplete("defi", "defi");
    assert_eq!(
        result.completions(),
        vec![JsValue::from("define")].into_boxed_slice()
    );
}
