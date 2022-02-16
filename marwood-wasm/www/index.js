import "./node_modules/jquery.terminal/css/jquery.terminal.css";
import "./terminal.css"
import "./node_modules/prismjs/prism"
import "./node_modules/jquery.terminal/js/prism"
import { Marwood, MarwoodWrapper } from "marwood";
import * as parens from "./parens.js";
import "./prism.css";
import "./prism-scheme.js";
import { Buffer } from "buffer";

const $ = require("jquery");

$.terminal.syntax("scheme");
$.terminal.prism_formatters = {
    command: true,
    echo: false,
    prompt: true
};

const vm = Marwood.new();
var term = $('#terminal').terminal((text) => {
    if (text.length == 0) {
        return "";
    }
    return new Promise((resolve, reject) => {
        let first_eval = true;
        let remaining_text;
        function try_eval() {
            let result;
            if (first_eval) {
                result = vm.eval(text, 5000);
                remaining_text = result.remaining;
                first_eval = false;
            } else {
                result = vm.eval_continue(1000);
            }

            if (result.completed) {
                if (result.ok != null && result.ok.length > 0) {
                    setTimeout(() => { term.echo(result.ok); });
                } else if (result.error != null && result.error.length > 0) {
                    setTimeout(() => { term.echo(result.error); });
                } else if (term.marwoodDisplayed) {
                    term.marwoodDisplayed = false;
                    setTimeout(() => { term.echo(""); });
                }
                if (remaining_text != null && remaining_text.length > 0) {
                    console.log(`REMAINING: ${remaining_text}`);
                    setTimeout(() => {
                        term.history().disable();
                        term.exec(remaining_text).then(() => {
                            term.history().enable();
                        });
                    });
                }
                return true;
            }
            return false;
        }

        function eval_loop() {
            try {
                if (!try_eval()) {
                    setTimeout(() => {
                        eval_loop();
                    });
                } else {
                    resolve();
                }
            } catch (e) {
                console.log(e);
                reject("fatal error in try_eval()");
                return;
            }
        }
        eval_loop();
    });
}, {
    name: 'marwood',
    marwoodDisplayed: false,
    greetings: false,
    keydown: () => { parens.set_position(term) },
    keypress: (e) => { parens.paren_match(term, e) },
    keymap: {
        ENTER: (e, original) => {
            let check = vm.check(term.get_command());
            if (check.eof) {
                term.insert('\n')
            } else {
                original();
            }
        }
    },
    doubleTab: (text, matches, echo_command) => {
        echo_command();
        term.echo(matches
            .map((match) => vm.last_token(match))
            .join(" "));
    },
    clear: false,
    completionEscape: false,
    wordAutocomplete: false,
    completion: (text) => {
        let result = vm.autocomplete(text);
        return result.completions;
    }
});

globalThis.marwood_display = (text) => {
    term.marwoodDisplayed = true;
    setTimeout(() => {
        term.echo(text, { newline: false })
    });
}

const params = new URLSearchParams(window.location.search);

if (params.has("eval")) {
    let text = params.get("eval");
    text = text.replace("-", "+");
    text = text.replace("_", "/");
    text = Buffer.from(text, "base64")
        .toString('utf8');
    if (text != null) {
        term.echo("λMARWOOD", { typing: true, delay: 100 }).then(() => {
            term.echo("");
            term.set_prompt("> ");
            term.history().append(text);
            term.exec(text);
        });
    }
} else {
    term.echo("λMARWOOD", { typing: true, delay: 100 }).then(() => {
        term.echo("");
        term.set_prompt("> ");
    });
}