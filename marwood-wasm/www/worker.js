import { Marwood } from "marwood";

let marwood = null;
let termCols = 80;
let termRows = 24;

self.marwood_display = (text) => {
  self.postMessage({ type: "display", text });
};

self.marwood_termCols = () => termCols;
self.marwood_termRows = () => termRows;

const handleMessage = (e) => {
  const msg = e.data;
  switch (msg.type) {
    case "init":
      try {
        if (msg.buffer != null) {
          marwood = Marwood.new_with_shared(msg.buffer);
        } else {
          marwood = Marwood.new();
        }
        self.postMessage({ type: "ready" });
      } catch (err) {
        self.postMessage({ type: "error", text: "worker init: " + String(err) });
      }
      break;
    case "termSize":
      termCols = msg.cols;
      termRows = msg.rows;
      break;
    case "eval":
      runEval(msg.text);
      break;
  }
};

// Use both addEventListener and onmessage; module workers vary in
// which one delivers messages reliably across browsers.
self.addEventListener("message", handleMessage);
self.onmessage = handleMessage;

// Signal main that the worker is alive and listening before main
// posts the init message containing the SharedArrayBuffer. Sending
// the SAB while the worker is still loading races with Chrome's
// cross-origin-isolated checks and silently drops the message.
self.postMessage({ type: "started" });

function runEval(text) {
  let remaining = text;
  try {
    while (remaining != null) {
      let result = marwood.eval(remaining, 0xffffffff);
      while (!result.completed) {
        result = marwood.eval_continue(0xffffffff);
      }
      remaining = result.remaining;
      if (remaining != null && remaining.length === 0) remaining = null;

      if (result.ok != null && result.ok.length > 0) {
        self.postMessage({ type: "output", text: result.ok });
      } else if (result.error != null && result.error.length > 0) {
        self.postMessage({ type: "output", text: result.error });
      }
    }
    self.postMessage({ type: "done" });
  } catch (err) {
    self.postMessage({ type: "error", text: String(err) });
  }
}
