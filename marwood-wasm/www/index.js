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
    }
}, {
    name: 'marwood',
    greetings: false,
    keydown: () => { parens.set_position(term) },
    keypress: (e) => { parens.paren_match(term, e) },
    keymap: {
        ENTER: (e, original) => {
            original();
        }
    }
});

globalThis.marwood_display = (text) => term.echo(text, { newline: false });
term.echo
term.echo("λMARWOOD", { typing: true, delay: 100 });
term.echo("");
term.set_prompt(">");
term.focus(true);