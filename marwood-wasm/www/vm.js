import { Marwood } from "marwood";

// Wire-protocol slots (must match marwood-wasm/src/lib.rs).
const SLOT_STATE = 0;
const SLOT_OPCODE = 1;
const SLOT_RESULT_A = 2;
const SLOT_RESULT_B = 3;

const STATE_IDLE = 0;
const STATE_REQUEST = 1;
const STATE_RESPONSE = 2;

const OP_READ_CHAR = 1;
const OP_PEEK_CHAR = 2;
const OP_CHAR_READY = 3;

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
    this.evalRunning = false;
    this.inputQueue = [];
    this.inputResolver = null;
    this.workerReady = null;

    // Main-thread Marwood for sync syntax queries (check / highlight).
    this.marwood = Marwood.new();

    // SharedArrayBuffer is only available under crossOriginIsolated.
    // When unavailable, the worker is still spawned but read-char
    // returns eof-object.
    if (globalThis.crossOriginIsolated && typeof SharedArrayBuffer !== "undefined") {
      this.sab = new SharedArrayBuffer(16);
      this.i32 = new Int32Array(this.sab);
      this.startRpcLoop();
    } else {
      console.warn(
        "marwood: not crossOriginIsolated — read-char will return eof-object"
      );
      this.sab = null;
      this.i32 = null;
    }

    this.spawnWorker();

    this.rl.term.onData((data) => this.handleTermData(data));

    rl.setHighlighter(new Highlighter(this));
    rl.setCheckHandler(this.check.bind(this));
    rl.setCtrlCHandler(this.stop.bind(this));
    rl.setPauseHandler(() => {});
  }

  spawnWorker() {
    this.worker = new Worker(new URL("./worker.js", import.meta.url), {
      type: "module",
    });
    this.workerReady = new Promise((resolve) => {
      this.workerReadyResolve = resolve;
    });
    this.worker.onmessage = (e) => this.handleWorkerMessage(e.data);
    this.worker.onerror = (e) =>
      console.error("[vm] worker.onerror:", e.message, e.filename, e.lineno);
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
      case "started":
        // Worker is alive and listening; safe to send init (which
        // carries the SharedArrayBuffer).
        this.worker.postMessage({ type: "init", buffer: this.sab });
        this.sendTermSize();
        break;
      case "ready":
        if (this.workerReadyResolve) {
          this.workerReadyResolve();
          this.workerReadyResolve = null;
        }
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
        this.evalRunning = false;
        this.inputQueue = [];
        if (this.evalPromise) {
          const [resolve] = this.evalPromise;
          this.evalPromise = null;
          resolve();
        }
        break;
      case "error":
        this.evalRunning = false;
        this.inputQueue = [];
        if (this.evalPromise) {
          const [, reject] = this.evalPromise;
          this.evalPromise = null;
          reject(msg.text);
        }
        break;
    }
  }

  // While eval is in flight, consume keystrokes from xterm into a
  // queue that the worker reads via SAB. Outside of eval, rustyline
  // owns input as before.
  handleTermData(data) {
    if (!this.evalRunning) return;
    for (const ch of data) {
      if (ch === "\x03") {
        // Ctrl-C: kill the worker and reject the eval.
        this.stop();
        return;
      }
      if (ch === "\x04") {
        // Ctrl-D: signal EOF to the next read-char.
        this.inputQueue.push(null);
      } else {
        this.inputQueue.push(ch);
      }
    }
    if (this.inputResolver) {
      const r = this.inputResolver;
      this.inputResolver = null;
      r();
    }
  }

  async startRpcLoop() {
    while (true) {
      // Wait for the worker to flip STATE to REQUEST.
      let cur = Atomics.load(this.i32, SLOT_STATE);
      while (cur !== STATE_REQUEST) {
        const r = Atomics.waitAsync(this.i32, SLOT_STATE, cur);
        if (r.async) await r.value;
        cur = Atomics.load(this.i32, SLOT_STATE);
      }
      await this.serviceRequest();
    }
  }

  async serviceRequest() {
    const op = Atomics.load(this.i32, SLOT_OPCODE);
    if (op === OP_READ_CHAR || op === OP_PEEK_CHAR) {
      const ch = await this.takeChar(op === OP_PEEK_CHAR);
      if (ch === null) {
        Atomics.store(this.i32, SLOT_RESULT_A, 0);
        Atomics.store(this.i32, SLOT_RESULT_B, 1);
      } else {
        Atomics.store(this.i32, SLOT_RESULT_A, ch.codePointAt(0));
        Atomics.store(this.i32, SLOT_RESULT_B, 0);
      }
    } else if (op === OP_CHAR_READY) {
      Atomics.store(this.i32, SLOT_RESULT_A, this.inputQueue.length > 0 ? 1 : 0);
      Atomics.store(this.i32, SLOT_RESULT_B, 0);
    }
    Atomics.store(this.i32, SLOT_STATE, STATE_RESPONSE);
    Atomics.notify(this.i32, SLOT_STATE);
  }

  async takeChar(peek) {
    while (this.inputQueue.length === 0) {
      await new Promise((resolve) => {
        this.inputResolver = resolve;
      });
    }
    const head = this.inputQueue[0];
    if (peek) {
      // Peek doesn't consume, but if head is the EOF sentinel we leave
      // it in place so the matching read-char also sees EOF.
      return head;
    }
    return this.inputQueue.shift();
  }

  check(input) {
    return !this.marwood.check(input).eof;
  }

  async eval(input) {
    await this.workerReady;
    return new Promise((resolve, reject) => {
      this.evalPromise = [resolve, reject];
      this.displayed = false;
      this.evalRunning = true;
      this.inputQueue = [];
      this.sendTermSize();
      this.worker.postMessage({ type: "eval", text: input });
    });
  }

  stop() {
    if (this.worker) {
      this.worker.terminate();
      this.spawnWorker();
    }
    this.evalRunning = false;
    this.inputQueue = [];
    if (this.inputResolver) {
      const r = this.inputResolver;
      this.inputResolver = null;
      r();
    }
    if (this.evalPromise) {
      const [, reject] = this.evalPromise;
      this.evalPromise = null;
      reject(null);
    }
  }
}
