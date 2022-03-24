import { Terminal } from "xterm";
import "xterm/css/xterm.css";
import { Readline } from "readline";
import { FitAddon } from "xterm-addon-fit";
import { WebLinksAddon } from "xterm-addon-web-links";
import { Unicode11Addon } from "xterm-addon-unicode11";
import * as XtermWebfont from "xterm-webfont";
import { Buffer } from "buffer";
import { Vm } from "./vm";

const params = new URLSearchParams(window.location.search);
const term = new Terminal({
  theme: {
    background: "#191A19",
    foreground: "#F5F2E7",
  },
  fontFamily: "Hack",
  fontSize: 14,
  cursorBlink: true,
  cursorStyle: "block",
});

term.loadAddon(new XtermWebfont());
const rl = new Readline();
term.loadAddon(rl);
const vm = new Vm(rl);

function read() {
  rl.read("> ", "  ")
    .then((input) => {
      vm.eval(input)
        .then(() => {
          setTimeout(read);
        })
        .catch((error) => {
          term.write("\n\x1B[E\x1B[!p");
          if (error != null) {
            rl.println(`error: ${error}`);
          }
          setTimeout(read);
        });
    })
    .catch((error) => rl.println(`error: ${error}`));
}

function animate_then_read(text) {
  rl.print([...text][0]);
  text = [...text].splice(1);
  if ([...text].length == 0) {
    rl.println("");
    read();
  } else {
    setTimeout(() => animate_then_read(text), 100);
  }
}

term.loadWebfontAndOpen(document.getElementById("terminal")).then(() => {
  term.loadAddon(new WebLinksAddon());

  const unicode11Addon = new Unicode11Addon();
  term.loadAddon(unicode11Addon);
  term.unicode.activeVersion = "11";

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

  term.focus();

  if (params.has("eval")) {
    rl.println("λMARWOOD");
    rl.println("");

    let text = params.get("eval");
    text = text.replace("-", "+");
    text = text.replace("_", "/");
    text = Buffer.from(text, "base64").toString("utf8");

    if (text != null) {
      rl.appendHistory(text);
      rl.print("> ");
      rl.println(text);
      vm.eval(text)
        .then(() => {
          read();
        })
        .catch((error) => {
          rl.write("\n\x1B[E\x1B[!p");
          if (error != null) {
            rl.println(`error: ${error}`);
          }
          setTimeout(read);
        });
    }
  } else {
    animate_then_read("λMARWOOD");
  }
});
