import { InputType, parseInput } from "./keymap";

test("simple data", () => {
  expect(parseInput("foo")).toEqual([
    {
      inputType: InputType.Text,
      data: ["f", "o", "o"],
    },
  ]);
});

test("enter key", () => {
  expect(parseInput("\r")).toEqual([
    {
      inputType: InputType.Enter,
      data: ["\r"],
    },
  ]);
});

test("data split by special", () => {
  expect(parseInput("foo\rbar")).toEqual([
    {
      inputType: InputType.Text,
      data: ["f", "o", "o"],
    },
    {
      inputType: InputType.Enter,
      data: ["\r"],
    },
    {
      inputType: InputType.Text,
      data: ["b", "a", "r"],
    },
  ]);
});

test("ansi escape", () => {
  expect(parseInput("\x1b[D")).toEqual([
    {
      inputType: InputType.ArrowLeft,
      data: ["\x1b", "[", "D"],
    },
  ]);

  expect(parseInput("\x1b\r")).toEqual([
    {
      inputType: InputType.AltEnter,
      data: ["\x1b", "\r"],
    },
  ]);

  expect(parseInput("\x1bZ")).toEqual([
    {
      inputType: InputType.UnsupportedEscape,
      data: ["\x1b", "Z"],
    },
  ]);

  expect(parseInput("\x1b[Z")).toEqual([
    {
      inputType: InputType.UnsupportedEscape,
      data: ["\x1b", "[", "Z"],
    },
  ]);

  expect(parseInput("\x1b[3~")).toEqual([
    {
      inputType: InputType.Delete,
      data: ["\x1b", "[", "3", "~"],
    },
  ]);

  expect(parseInput("\x1b[1~")).toEqual([
    {
      inputType: InputType.UnsupportedEscape,
      data: ["\x1b", "[", "1", "~"],
    },
  ]);

  expect(parseInput("\x1b[1d")).toEqual([]);
});
