import { LineBuffer } from "./line";
import { Tty } from "./tty";
import { History } from "./history";
import stringWidth from "string-width";

export class Position {
  public col: number;
  public row: number;

  constructor(rows?: number, cols?: number) {
    if (rows != undefined) {
      this.row = rows;
    } else {
      this.row = 0;
    }
    if (cols != undefined) {
      this.col = cols;
    } else {
      this.col = 0;
    }
  }
}

export class Layout {
  public promptSize: Position;
  public cursor: Position;
  public end: Position;

  constructor(promptSize: Position) {
    this.promptSize = promptSize;
    this.cursor = new Position();
    this.end = new Position();
  }
}

export class State {
  private prompt: string;
  private promptSize: Position;
  private line: LineBuffer = new LineBuffer();
  private tty: Tty;
  private layout: Layout;
  private history: History;

  constructor(prompt: string, tty: Tty, history: History) {
    this.prompt = prompt;
    this.tty = tty;
    this.history = history;
    this.promptSize = tty.calculatePosition(prompt, new Position());
    this.layout = new Layout(this.promptSize);
  }

  public buffer(): string {
    return this.line.buffer();
  }

  public clearScreen() {
    this.tty.clearScreen();
    this.layout.cursor = new Position();
    this.layout.end = new Position();
    this.refresh();
  }

  public editInsert(text: string) {
    const push = this.line.insert(text);
    const multiline = text.includes("\n");
    if (push && !multiline) {
      const width = stringWidth(text);
      if (width > 0 && this.layout.cursor.col + width < this.tty.col) {
        this.layout.cursor.col += width;
        this.layout.end.col += width;
        this.tty.write(text);
      } else {
        this.refresh();
      }
    } else {
      this.refresh();
    }
  }

  public update(text: string) {
    this.line.update(text, text.length);
    this.refresh();
  }

  public editBackspace(n: number) {
    if (this.line.backspace(n)) {
      this.refresh();
    }
  }

  public editDelete(n: number) {
    if (this.line.delete(n)) {
      this.refresh();
    }
  }

  public refresh() {
    const newLayout = this.tty.computeLayout(this.promptSize, this.line);
    this.tty.refreshLine(this.prompt, this.line, this.layout, newLayout);
    this.layout = newLayout;
  }

  public moveCursorBack(n: number) {
    if (this.line.moveBack(n)) {
      this.moveCursor();
    }
  }

  public moveCursorForward(n: number) {
    if (this.line.moveForward(n)) {
      this.moveCursor();
    }
  }

  public moveCursorUp(n: number) {
    if (this.line.moveLineUp(n)) {
      this.moveCursor();
    } else {
      this.previousHistory();
    }
  }

  public moveCursorDown(n: number) {
    if (this.line.moveLineDown(n)) {
      this.moveCursor();
    } else {
      this.nextHistory();
    }
  }

  public moveCursorHome() {
    if (this.line.moveHome()) {
      this.moveCursor();
    }
  }

  public moveCursorEnd() {
    if (this.line.moveEnd()) {
      this.moveCursor();
    }
  }

  public moveCursorToEnd() {
    if (this.layout.cursor === this.layout.end) {
      return;
    }
    this.tty.moveCursor(this.layout.cursor, this.layout.end);
    this.layout.cursor = { ...this.layout.end };
  }

  public previousHistory() {
    if (this.history.cursor == -1 && this.line.length() > 0) {
      return;
    }
    const prev = this.history.prev();
    if (prev != undefined) {
      this.update(prev);
    }
  }

  public nextHistory() {
    if (this.history.cursor == -1) {
      return;
    }
    const next = this.history.next();
    if (next != undefined) {
      this.update(next);
    } else {
      this.update("");
    }
  }

  public moveCursor() {
    const cursor = this.tty.calculatePosition(
      this.line.pos_buffer(),
      this.promptSize
    );
    if (cursor == this.layout.cursor) {
      return;
    }
    this.tty.moveCursor(this.layout.cursor, cursor);
    this.layout.promptSize = { ...this.promptSize };
    this.layout.cursor = { ...cursor };
  }
}
