import { Terminal } from 'xterm';
import "xterm/css/xterm.css"
import LocalEchoController from 'local-echo';
import { FitAddon } from 'xterm-addon-fit';
import { WebLinksAddon } from 'xterm-addon-web-links';
import { Unicode11Addon } from 'xterm-addon-unicode11';
import * as XtermWebfont from 'xterm-webfont'
import { Marwood } from "marwood";
import { Buffer } from "buffer";

const vm = Marwood.new();
let vmDisplayed = false;

const localEcho = new LocalEchoController();

function read() {
    localEcho.read("> ", "  ")
        .then(input => {
            evalInput(input);
        })
        .catch(error => alert(`Error reading: ${error}`));
}

function evalInput(input) {
    let firstEval = true;
    let remainingText;

    function do_eval() {
        let result;
        if (firstEval) {
            result = vm.eval(input, 10000);
            remainingText = result.remaining;
            firstEval = false;
        } else {
            result = vm.eval_continue(10000);
        }

        if (result.completed) {
            if (result.ok != null && result.ok.length > 0) {
                localEcho.println(result.ok);
            } else if (result.error != null && result.error.length > 0) {
                localEcho.println(result.error);
            } else if (vmDisplayed) {
                vmDisplayed = false;
                localEcho.println("");
            }
            if (remainingText != null && remainingText.length > 0) {
                setTimeout(() => evalInput(remainingText));
            } else {
                setTimeout(read);
            }
            return true;
        }
        return false;
    }

    function eval_loop() {
        try {
            if (!do_eval()) {
                setTimeout(() => {
                    eval_loop();
                });
            }
        } catch (e) {
            console.log(e);
            return;
        }
    }

    eval_loop();
}

globalThis.marwood_display = (text) => {
    vmDisplayed = true;
    localEcho.print(text);
}

const params = new URLSearchParams(window.location.search);
const term = new Terminal({
    theme: {
        background: '#191A19',
        foreground: '#F5F2E7',
    },
    fontFamily: "Hack",
    fontSize: 14,
    cursorBlink: true,
    cursorStyle: "block"
});

term.loadAddon(new XtermWebfont())


term.loadWebfontAndOpen(document.getElementById('terminal')).then(() => {
    term.loadAddon(localEcho);
    term.loadAddon(new WebLinksAddon());

    const unicode11Addon = new Unicode11Addon();
    term.loadAddon(unicode11Addon);
    term.unicode.activeVersion = '11';

    if (params.has("rows") && params.has("cols")) {
        let rows = params.get("rows");
        let cols = params.get("cols");
        term.resize(cols, rows);
    } else {
        const fitAddon = new FitAddon();
        term.loadAddon(fitAddon);
        fitAddon.fit();
        fitAddon.activate(term);
    }

    localEcho.addCheckHandler((input) => vm.check(input).eof);

    term.focus();
    localEcho.println("λMARWOOD");
    localEcho.println("");

    if (params.has("eval")) {
        let text = params.get("eval");
        text = text.replace("-", "+");
        text = text.replace("_", "/");
        text = Buffer.from(text, "base64")
            .toString('utf8');
        if (text != null) {
            localEcho.history.push(text);
            localEcho.print("> ");
            localEcho.println(text);
            evalInput(text);
        }
    } else {
        read();
    }
});