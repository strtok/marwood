import { Marwood } from "marwood";

export class Vm {
    constructor(term) {
        this.term = term;
        this.displayed = false;
        this.remainingInput = null;
        this.evalIter = 0;
        this.stopping = false;
        this.marwood = Marwood.new();

        globalThis.marwood_display = (text) => {
            this.displayed = true;
            this.term.print(text);
        }

        globalThis.marwood_termCols = () => {
            return this.term.term.cols;
        }

        globalThis.marwood_termRows = () => {
            return this.term.term.rows;
        }
    }

    check(input) {
        return this.marwood.check(input);
    }

    eval(input) {
        return new Promise((resolve, reject) => {
            this.evalPromise = [resolve, reject];
            this.remainingInput = input;
            this.evalIter = 0;
            this.displayed = false;
            this.stopping = false;
            setTimeout(() => this.execute());
        });
    }

    stop() {
        this.stopping = true;
    }

    execute() {
        if (this.stopping) {
            let [resolve, reject] = this.evalPromise;
            reject(null);
            return;
        }

        if (this.term.highWater()) {
            setTimeout(() => this.execute(), 1);
            return;
        }

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
                this.displayed = false;
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