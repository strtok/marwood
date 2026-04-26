import { Marwood } from "marwood";

class Highlighter {
  constructor(vm) {
    this.vm = vm;
  }

  highlight(line, pos) {
    let result = this.vm.marwood.highlight(line, pos);
    if (result.highlighted) {
      return result.text;
    } else {
      return line;
    }
  }

  highlightPrompt(prompt) {
    return prompt;
  }

  highlightChar(line, pos) {
    return this.vm.marwood.highlight_check(line, pos);
  }
}

export class Vm {
  constructor(rl) {
    this.rl = rl;
    this.displayed = false;
    this.evalPromise = null;

    // Main-thread Marwood is used only for synchronous syntax queries:
    // check, highlight, last_token, autocomplete. Eval runs in the
    // worker. Note: this main-thread instance has no eval state, so
    // autocomplete won't see user-defined globals (regression vs the
    // pre-worker REPL); to be addressed in a follow-up.
    this.marwood = Marwood.new();

    this.spawnWorker();

    rl.setHighlighter(new Highlighter(this));
    rl.setCheckHandler(this.check.bind(this));
    rl.setCtrlCHandler(this.stop.bind(this));
    rl.setPauseHandler(() => {});
  }

  spawnWorker() {
    this.worker = new Worker(new URL("./worker.js", import.meta.url), {
      type: "module",
    });
    this.worker.onmessage = (e) => this.handleWorkerMessage(e.data);
    this.sendTermSize();
  }

  sendTermSize() {
    this.worker.postMessage({
      type: "termSize",
      cols: this.rl.term.cols,
      rows: this.rl.term.rows,
    });
  }

  handleWorkerMessage(msg) {
    switch (msg.type) {
      case "ready":
        break;
      case "display":
        this.displayed = true;
        this.rl.print(msg.text);
        break;
      case "output":
        this.rl.println(msg.text);
        break;
      case "done":
        if (this.displayed) {
          this.rl.println("");
          this.displayed = false;
        }
        if (this.evalPromise) {
          const [resolve] = this.evalPromise;
          this.evalPromise = null;
          resolve();
        }
        break;
      case "error":
        if (this.evalPromise) {
          const [, reject] = this.evalPromise;
          this.evalPromise = null;
          reject(msg.text);
        }
        break;
    }
  }

  check(input) {
    return !this.marwood.check(input).eof;
  }

  eval(input) {
    return new Promise((resolve, reject) => {
      this.evalPromise = [resolve, reject];
      this.displayed = false;
      this.sendTermSize();
      this.worker.postMessage({ type: "eval", text: input });
    });
  }

  stop() {
    if (this.worker) {
      this.worker.terminate();
      this.spawnWorker();
    }
    if (this.evalPromise) {
      const [, reject] = this.evalPromise;
      this.evalPromise = null;
      reject(null);
    }
  }
}
