import "./node_modules/jquery.terminal/css/jquery.terminal.css";
import "./terminal.css"
import "./node_modules/prismjs/prism"
import "./node_modules/jquery.terminal/js/prism"
import { Marwood, MarwoodWrapper } from "marwood";
import * as parens from "./parens.js";
import "./prism.css";
import "./prism-scheme.js";
import { terminal } from "jquery";

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
                if (result.ok != null) {
                    term.echo(result.ok, { flush: false });
                } else if (result.error != null) {
                    term.echo(result.error, { flush: false });
                } else {
                    term.echo("");
                }
                term.flush();
                if (remaining_text != null && remaining_text.length > 0) {
                    term.exec(remaining_text);
                }
                return true;
            } else {
                setTimeout(() => { term.flush(); }, 0);
            }
            return false;
        }

        function eval_loop() {
            try {
                if (!try_eval()) {
                    setTimeout(() => {
                        eval_loop();
                    }, 0);
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
    greetings: false,
    keydown: () => { parens.set_position(term) },
    keypress: (e) => { parens.paren_match(term, e) },
    keymap: {
        ENTER: async (e, original) => {
            let check = await vm.check(term.get_command());
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
            .map(async (match) => await vm.last_token(match))
            .join(" "));
    },
    clear: false,
    completionEscape: false,
    wordAutocomplete: false,
    completion: async (text) => {
        let result = await vm.autocomplete(text);
        return result.completions;
    }
});

globalThis.marwood_display = (text) => {
    term.echo(text, { newline: false, flush: false });
}

term.echo("λMARWOOD", { typing: true, delay: 100 });
term.echo("");
term.set_prompt(">");
