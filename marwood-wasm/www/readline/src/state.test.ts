import { History } from "./history";
import { State } from "./state";
import { Tty } from "./tty";

const PROMPT = "> ";

class Output {
  output: string[] = [];
  write = jest.fn((text: string) => this.output.push(text));
  print = jest.fn((text: string) => this.output.push(text));
  println = jest.fn((text: string) => this.output.push(text));
}

test("edit insert push", () => {
  const out = new Output();
  const tty = new Tty(80, 24, 8, out);
  const state = new State(PROMPT, tty, new History(50));
  state.editInsert("a");
  state.editInsert("b");
  state.editInsert("c");
  expect(state.buffer()).toBe("abc");
  expect(out.write).toHaveBeenNthCalledWith(1, "a");
  expect(out.write).toHaveBeenNthCalledWith(2, "b");
  expect(out.write).toHaveBeenNthCalledWith(3, "c");
});

test("edit insert", () => {
  const out = new Output();
  const tty = new Tty(80, 24, 8, out);
  const state = new State(PROMPT, tty, new History(50));
  state.editInsert("a");
  state.editInsert("b");
  state.editInsert("c");
  state.moveCursorBack(1);
  state.editInsert("d");
  expect(state.buffer()).toBe("abdc");
});

test("edit insert wrap", () => {
  const out = new Output();
  const tty = new Tty(5, 24, 8, out);
  const state = new State(PROMPT, tty, new History(50));
  state.editInsert("a");
  state.editInsert("b");
  state.editInsert("c");
  state.editInsert("d");
  expect(state.buffer()).toBe("abcd");
  expect(out.write).toHaveBeenNthCalledWith(1, "a");
  expect(out.write).toHaveBeenNthCalledWith(2, "b");
  expect(out.write).toHaveBeenNthCalledWith(3, "c");
  expect(out.write).toHaveBeenNthCalledWith(4, "d");

  out.write.mockClear();
  state.editInsert("e");
  expect(state.buffer()).toBe("abcde");
  expect(out.write).toHaveBeenNthCalledWith(1, "\r\x1B[0K");
  expect(out.write).toHaveBeenNthCalledWith(2, "> ");
  expect(out.write).toHaveBeenNthCalledWith(3, "abcde");
  expect(out.write).toHaveBeenNthCalledWith(4, "\r\x1B[2C");
});

test("edit multiline backcursor", () => {
  const out = new Output();
  const tty = new Tty(5, 24, 8, out);
  const state = new State(PROMPT, tty, new History(50));
  state.editInsert("a");
  state.editInsert("\n");
  state.editInsert("b");
  state.editInsert("\n");
  state.editInsert("c");
  state.moveCursorBack(1);
  state.moveCursorBack(1);
  state.editInsert("d");
  state.editBackspace(1);
  expect(state.buffer()).toBe("a\nb\nc");
});

test("cursor arrow movement", () => {
  const out = new Output();
  const tty = new Tty(5, 24, 8, out);
  const state = new State(PROMPT, tty, new History(50));
  state.editInsert("abc\ndef\nghi");
  state.moveCursorBack(1);
  state.moveCursorUp(1);
  state.editInsert("z");
  expect(state.buffer()).toEqual("abc\ndezf\nghi");
  state.moveCursorForward(1);
  state.moveCursorDown(1);
  state.editInsert("y");
  expect(state.buffer()).toEqual("abc\ndezf\nghiy");
});
