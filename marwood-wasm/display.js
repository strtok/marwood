export function display(text) {
    if (globalThis.marwood_display == null) {
        console.log("marwood_display is not set")
    } else {
        globalThis.marwood_display(text);
    }
}
