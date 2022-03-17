export function display(text) {
    if (globalThis.marwood_display == null) {
        console.log("marwood_display is not set")
    } else {
        globalThis.marwood_display(text);
    }
}

export function termCols() {
    if (globalThis.marwood_termCols == null) {
        console.log("marwood_termCols is not set");
        return 0;
    } else {
        return globalThis.marwood_termCols();
    }
}

export function termRows() {
    if (globalThis.marwood_termRows == null) {
        console.log("marwood_termCols is not set");
        return 0;
    } else {
        return globalThis.marwood_termRows();
    }
}
