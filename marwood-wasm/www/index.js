import "./node_modules/jquery.terminal/css/jquery.terminal.css";
import "./terminal.css"
import { Marwood } from "marwood";
import * as parens from "./parens.js";

const $ = require("jquery");
const terminal = require("jquery.terminal");

const vm = Marwood.new((text) => console.log(text));

var term = $('#terminal').terminal((text) => {
    if (text.length > 0) {
        let result = vm.eval(text);
        if (result.ok != null) {
            term.echo(result.ok);
        } else if (result.error != null) {
            term.error(result.error);
        } else {
            term.echo("")
        }

        if (result.remaining != null) {
            term.exec(result.remaining);
        }
    }
}, {
    name: 'marwood',
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
    completionEscape: false,
    wordAutocomplete: false,
    completion: (text) => {
        let result = vm.autocomplete(text);
        return result.completions;
    }
});

globalThis.marwood_display = (text) => term.echo(text, { newline: false });
term.echo
term.echo("λMARWOOD", { typing: true, delay: 100 });
term.echo("");
term.set_prompt(">");
term.focus(true);