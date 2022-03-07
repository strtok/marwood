import { Marwood } from "marwood";

export class Vm {
    constructor(term) {
        this.term = term;
        this.displayed = false;
        this.remainingInput = null;
        this.evalIter = 0;
        this.marwood = Marwood.new();

        globalThis.marwood_display = (text) => {
            this.displayed = true;
            this.term.print(text);
        }
    }

    check(input) {
        console.log(input);
        return this.marwood.check(input);
    }

    eval(input) {
        return new Promise((resolve, reject) => {
            this.evalPromise = [resolve, reject];
            this.remainingInput = input;
            this.evalIter = 0;
            this.displayed = false;
            setTimeout(() => this.execute());
        });
    }

    execute() {
        let result;

        if (this.evalIter == 0) {
            result = this.marwood.eval(this.remainingInput, 10000);
            this.remainingInput = result.remaining;
            if (this.remainingInput != null && this.remainingInput.length == 0) {
                this.remainingInput = null;
            }
        } else {
            result = this.marwood.eval_continue(10000);
        }

        if (result.completed) {
            if (result.ok != null && result.ok.length > 0) {
                this.term.println(result.ok);
            } else if (result.error != null && result.error.length > 0) {
                this.term.println(result.error);
            } else if (this.displayed) {
                this.term.println("");
            }

            if (this.remainingInput != null) {
                this.evalIter = 0;
                setTimeout(() => this.execute());
                return;
            }

            let [resolve, reject] = this.evalPromise;
            resolve();
            return;
        }


        this.evalIter += 1;
        setTimeout(() => this.execute());
    }
}