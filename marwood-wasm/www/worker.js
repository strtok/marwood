import { Marwood } from "marwood";

let marwood = null;
let termCols = 80;
let termRows = 24;

self.marwood_display = (text) => {
  self.postMessage({ type: "display", text });
};

self.marwood_termCols = () => termCols;
self.marwood_termRows = () => termRows;

marwood = Marwood.new();
self.postMessage({ type: "ready" });

self.onmessage = (e) => {
  const msg = e.data;
  switch (msg.type) {
    case "termSize":
      termCols = msg.cols;
      termRows = msg.rows;
      break;
    case "eval":
      runEval(msg.text);
      break;
  }
};

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
