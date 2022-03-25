import { Position, Layout } from "./state";
import { LineBuffer } from "./line";
import stringWidth from "string-width";
import { Highlighter } from "./highlight";

export interface Output {
  write(text: string): void;
  print(text: string): void;
  println(text: string): void;
}

export class Tty {
  public tabWidth: number;
  public col: number;
  public row: number;
  private out: Output;

  constructor(col: number, row: number, tabWidth: number, out: Output) {
    this.tabWidth = tabWidth;
    this.col = col;
    this.row = row;
    this.out = out;
  }

  public write(text: string) {
    return this.out.write(text);
  }

  public print(text: string) {
    return this.out.print(text);
  }

  public println(text: string) {
    return this.out.println(text);
  }

  public clearScreen() {
    this.out.write("\x1b[H\x1b[2J");
  }

  // Calculate the number of colums and rows required to print
  // text on a this.cols wide terminal starting at orig
  public calculatePosition(text: string, orig: Position): Position {
    const pos = { ...orig };
    let esc_seq = 0;

    [...text].forEach((c) => {
      if (c == "\n") {
        pos.row += 1;
        pos.col = 0;
        return;
      }
      let cw = 0;
      if (c == "\t") {
        cw = this.tabWidth - (pos.col % this.tabWidth);
      } else {
        let size;
        [size, esc_seq] = width(c, esc_seq);
        cw = size;
      }
      pos.col += cw;
      if (pos.col > this.col) {
        pos.row += 1;
        pos.col = cw;
      }
    });

    if (pos.col == this.col) {
      pos.col = 0;
      pos.row += 1;
    }

    return pos;
  }

  public computeLayout(promptSize: Position, line: LineBuffer): Layout {
    const newPromptSize = { ...promptSize };
    const pos = line.pos;
    const cursor = this.calculatePosition(
      line.buf.slice(0, line.pos),
      promptSize
    );
    const end =
      pos == line.buf.length
        ? { ...cursor }
        : this.calculatePosition(line.buf.slice(pos), cursor);
    const newLayout = {
      promptSize: newPromptSize,
      cursor,
      end,
    };
    return newLayout;
  }

  public refreshLine(
    prompt: string,
    line: LineBuffer,
    oldLayout: Layout,
    newLayout: Layout,
    highlighter: Highlighter
  ) {
    const cursor = newLayout.cursor;
    const end_pos = newLayout.end;
    this.clearOldRows(oldLayout);

    this.write(highlighter.highlightPrompt(prompt));
    this.write(highlighter.highlight(line.buf, line.pos));

    if (
      end_pos.col == 0 &&
      end_pos.row > 0 &&
      line.buf[line.buf.length - 1] != "\n"
    ) {
      this.write("\n");
    }

    const newCursorRowMovement = end_pos.row - cursor.row;
    if (newCursorRowMovement > 0) {
      this.write(`\x1b[${newCursorRowMovement}A`);
    }
    if (cursor.col > 0) {
      this.write(`\r\x1b[${cursor.col}C`);
    } else {
      this.write("\r");
    }
  }

  public clearOldRows(layout: Layout) {
    const currentRow = layout.cursor.row;
    const oldRows = layout.end.row;
    const cursorRowMovement = Math.max(oldRows - currentRow, 0);
    if (cursorRowMovement > 0) {
      this.write(`\x1b[${cursorRowMovement}B`);
    }
    for (let i = 0; i < oldRows; i++) {
      this.write("\r\x1b[0K\x1b[A");
    }
    this.write("\r\x1b[0K");
  }

  public moveCursor(oldCursor: Position, newCursor: Position) {
    if (newCursor.row > oldCursor.row) {
      // Move Down
      const rowShift = newCursor.row - oldCursor.row;
      if (rowShift == 1) {
        this.write("\x1b[B");
      } else {
        this.write(`\x1b[${rowShift}B`);
      }
    } else if (newCursor.row < oldCursor.row) {
      // Move Up
      const rowShift = oldCursor.row - newCursor.row;
      if (rowShift == 1) {
        this.write("\x1b[A");
      } else {
        this.write(`\x1b[${rowShift}A`);
      }
    }

    if (newCursor.col > oldCursor.col) {
      // Move Right
      const colShift = newCursor.col - oldCursor.col;
      if (colShift == 1) {
        this.write("\x1b[C");
      } else {
        this.write(`\x1b[${colShift}C`);
      }
    } else if (newCursor.col < oldCursor.col) {
      const colShift = oldCursor.col - newCursor.col;
      if (colShift == 1) {
        this.write("\x1b[D");
      } else {
        this.write(`\x1b[${colShift}D`);
      }
    }
    return;
  }
}

// Return the column width of text when printed
function width(text: string, esc_seq: number): [size: number, esc_seq: number] {
  if (esc_seq == 1) {
    if (text == "[") {
      return [0, 2];
    } else {
      return [0, 0];
    }
  } else if (esc_seq == 2) {
    if (!(text == ";" || (text[0] >= "0" && text[0] <= "9"))) {
      // unsupported
      return [0, 0];
    }
    return [0, esc_seq];
  } else if (text == "\x1b") {
    return [0, 1];
  } else if (text == "\n") {
    return [0, esc_seq];
  } else {
    return [stringWidth(text), esc_seq];
  }
}
