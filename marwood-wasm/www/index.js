import { Terminal } from "xterm";
import "xterm/css/xterm.css";
import { Readline } from "xterm-readline";
import { FitAddon } from "xterm-addon-fit";
import { WebLinksAddon } from "xterm-addon-web-links";
import { Unicode11Addon } from "xterm-addon-unicode11";
import * as XtermWebfont from "xterm-webfont";
import { Buffer } from "buffer";
import { Vm } from "./vm";
import { inflate } from "pako";

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

  if (params.has("eval") || params.has("zeval")) {
    rl.println("λMARWOOD");
    rl.println("");

    let compressed;
    let text;
    if (params.has("zeval")) {
      compressed = true;
      text = params.get("zeval");
    } else {
      compressed = false;
      text = params.get("eval");
    }

    text = text.replace("-", "+");
    text = text.replace("_", "/");

    if (compressed) {
      text = inflate(Buffer.from(text, "base64"), {to: "string"});
    } else {
      text = Buffer.from(text, "base64").toString("utf8");
    }

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
  } else if (params.has("fetchEval")) {
    rl.println("λMARWOOD");
    rl.println("");

    let url64 = params.get("fetchEval");

    url64 = url64.replace("-", "+");
    url64 = url64.replace("_", "/");

    const url = Buffer.from(url64, "base64").toString("utf8");

    if (url == null) {
      return;
    }

    if (url != null) {
      fetch(url)
        .then((response) => {
          response.text().then((text) => {
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
            }).catch((error) => {
              rl.println(`error: ${error}`);
              setTimeout(read);
            });
        }).catch((error) => {
          rl.println(`error: ${error}`);
          setTimeout(read);
        });
    }
  } else if (params.has("gist")) {
    setTimeout(async () => { 
      try {
        await evalGist(params.get("gist"));
      } catch (error) {
        if (error != null) {
          rl.println(`error: ${error}`);
        }
      }
      setTimeout(read);
    });
  } else {
    animate_then_read("λMARWOOD");
  }
});

async function evalGist(gistId) {
  const gistUrl = "https://api.github.com/gists/" + gistId;
  const gistDesc = await fetch(gistUrl);
  if (gistDesc.status != 200) {
    rl.println(`could not fetch gist ${gistId}: ${gistDesc.status} ${gistDesc.statusText}`);
    return;
  }
  const gistDescBody = await gistDesc.text();
  const gistDescJson = JSON.parse(gistDescBody);
  
  let code = "";
  for (const fileName of Object.keys(gistDescJson.files).sort()) {
    let fileDesc = gistDescJson.files[fileName];
    let rawUrl = fileDesc.raw_url;
    let response = await fetch(rawUrl);
    if (response.status != 200) {
      rl.println("could not fetch ${fileName}: ${response.status} ${response.statusText}");
      return;
    }
    code += await response.text();
  }

  rl.appendHistory(code);
  rl.print("> ");
  rl.println(code);
  await vm.eval(code);
}
