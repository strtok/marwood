# About
This is a WASM based repl.

# Toolchain Dependencies

* nodejs
* wasm-pack

# Building & Running

```bash
export RUSTFLAGS='--cfg getrandom_backend="wasm_js"'
wasm-pack build && cd www && npm install && npm run start
```
