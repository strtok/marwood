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
        if (result.length > 0) {
            term.echo(result, { typing: true, delay: 5 });
        }
    }
}, {
    name: 'marwood',
    greetings: false,
    // below is the code for parenthesis matching (jumping)
    keydown: function () { parens.set_position(term) },
    keypress: function (e) { parens.paren_match(term, e) }
});

globalThis.marwood_display = (text) => term.echo(text, { typing: true, delay: 100 });

term.echo("λMARWOOD", { typing: true, delay: 100 });
term.echo("");
term.set_prompt(">");
term.focus(true);