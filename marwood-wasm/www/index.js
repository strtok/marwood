import { Terminal } from 'xterm';
import "xterm/css/xterm.css"
import LocalEchoController from 'local-echo';
import { FitAddon } from 'xterm-addon-fit';
import { WebLinksAddon } from 'xterm-addon-web-links';
import { Unicode11Addon } from 'xterm-addon-unicode11';
import * as XtermWebfont from 'xterm-webfont'
import { Buffer } from "buffer";
import { Vm } from "./vm";

const localEcho = new LocalEchoController();
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

const vm = new Vm(localEcho);

function read() {
    localEcho.read("> ", "  ")
        .then(input => {
            vm.eval(input).then(() => {
                setTimeout(read);
            })
                .catch(error => {
                    term.write("\x1B[E\x1B[!p");
                    if (error != null) {
                        localEcho.println(`error: ${error}`);
                    }
                    setTimeout(read);
                });
        })
        .catch(error => localEcho.println(`error: ${error}`));
}

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
    localEcho.addCtrlCHandler(() => {
        vm.stop();
    });

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
            vm.eval(text).then(() => {
                read();
            }).catch(error => {
                term.write("\x1B[E\x1B[!p");
                if (error != null) {
                    localEcho.println(`error: ${error}`);
                }
                setTimeout(read);
            });
        }
    } else {
        read();
    }
});