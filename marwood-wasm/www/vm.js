import { Marwood } from "marwood";

export class Vm {
    constructor(rl) {
        this.rl = rl;
        this.displayed = false;
        this.remainingInput = null;
        this.evalIter = 0;
        this.stopping = false;
        this.marwood = Marwood.new();

        globalThis.marwood_display = (text) => {
            this.displayed = true;
            this.rl.print(text);
        }

        globalThis.marwood_termCols = () => {
            return this.rl.term.cols;
        }

        globalThis.marwood_termRows = () => {
            return this.rl.term.rows;
        }

        rl.setCheckHandler(this.check.bind(this));
        rl.setCtrlCHandler(this.stop.bind(this));
    }

    check(input) {
        return !this.marwood.check(input).eof;
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

        if (!this.rl.writeReady()) {
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
                this.rl.println(result.ok);
            } else if (result.error != null && result.error.length > 0) {
                this.rl.println(result.error);
            } else if (this.displayed) {
                this.rl.println("");
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