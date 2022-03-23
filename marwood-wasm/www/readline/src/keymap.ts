export enum InputType {
    Text,
    AltEnter,
    ArrowUp,
    ArrowDown,
    ArrowLeft,
    ArrowRight,
    Delete,
    Backspace,
    CtrlA,
    CtrlC,
    CtrlD,
    CtrlE,
    CtrlL,
    End,
    Enter,
    Home,
    ShiftEnter,
    UnsupportedControlChar,
    UnsupportedEscape
}

export interface Input {
    inputType: InputType,
    data: string[]
}

export function parseInput(data: string): Input[] {
    return Array.from(splitInput(data));
}

function* splitInput(data: string) {
    let text = [];

    const it = data[Symbol.iterator]();
    for (let next = it.next(); next != undefined && !next.done; next = it.next()) {
        const c = next.value;

        if (c.length > 1) {
            text.push(c);
            continue;
        }

        const val = c.charCodeAt(0);
        if (text.length > 0 && (val < 0x20 || val == 0x7f)) {
            yield {
                inputType: InputType.Text,
                data: text
            }
            text = [];
        }

        if (val == 0x1b) {
            const seq2 = it.next();
            if (seq2.done) {
                text.push("\x1b");
                continue;
            }

            // Console
            if (seq2.value != "[") {
                let inputType = InputType.UnsupportedEscape;
                switch (seq2.value) {
                    case "\r":
                        inputType = InputType.AltEnter;
                        break;
                }
                yield {
                    inputType,
                    data: ["\x1b", seq2.value]
                };
                continue;
            }

            // Ansi Escape
            const seq3 = it.next();
            if (seq3.done) {
                continue;
            }
            let inputType = InputType.UnsupportedEscape;

            // vt sequence
            if (seq3.value >= "0" && seq3.value <= "9") {
                let digit = seq3.value;
                const nextDigit = it.next();
                if (nextDigit.done) {
                    return;
                }
                if (nextDigit.value >= "0" && nextDigit.value <= "9") {
                    digit += nextDigit.value;
                } else if (nextDigit.value != "~") {
                    continue;
                }
                switch (digit) {
                    case "3":
                        inputType = InputType.Delete;
                        break;
                }
                yield {
                    inputType,
                    data: ["\x1b", "[", digit, "~"]
                };
                continue;
            }

            switch (seq3.value) {
                case "A":
                    inputType = InputType.ArrowUp;
                    break;
                case "B":
                    inputType = InputType.ArrowDown;
                    break;
                case "C":
                    inputType = InputType.ArrowRight;
                    break;
                case "D":
                    inputType = InputType.ArrowLeft;
                    break;
                case "F":
                    inputType = InputType.End;
                    break;
                case "H":
                    inputType = InputType.Home;
                    break;
                case "\r":
                    inputType = InputType.AltEnter;
                    break;
            }
            yield {
                inputType,
                data: ["\x1b", "[", seq3.value]
            };
            continue;
        }

        if (val < 0x20 || val == 0x7f) {
            let inputType = InputType.UnsupportedControlChar;
            switch (val) {
                case 0x1:
                    inputType = InputType.CtrlA;
                    break;
                case 0x3:
                    inputType = InputType.CtrlC;
                    break;
                case 0x4:
                    inputType = InputType.CtrlD;
                    break;
                case 0x5:
                    inputType = InputType.CtrlE;
                    break;
                case 0xd:
                    inputType = InputType.Enter;
                    break;
                case 0x7f:
                    inputType = InputType.Backspace;
                    break;
                case 0xc:
                    inputType = InputType.CtrlL;
                    break;
            }
            yield {
                inputType,
                data: [c]
            }
            continue;
        }

        // else this is text
        text.push(c);
    }

    if (text.length > 0) {
        yield {
            inputType: InputType.Text,
            data: text
        }
    }
}