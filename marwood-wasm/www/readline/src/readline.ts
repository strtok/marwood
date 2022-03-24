import { Terminal, ITerminalAddon, IDisposable } from "xterm";
import { Input, InputType, parseInput } from "./keymap";
import { State } from "./state";
import { History } from "./history";
import { Output, Tty } from "./tty";

interface ActiveRead {
  prompt: string;
  resolve: (input: string) => void;
  reject: (e: unknown) => void;
}

type CheckHandler = (text: string) => boolean;
type CtrlCHandler = () => void;
type PauseHandler = (resume: boolean) => void;

export class Readline implements ITerminalAddon {
  private term: Terminal | undefined;
  private history: History = new History(50);
  private activeRead: ActiveRead | undefined;
  private disposables: IDisposable[] = [];
  private watermark = 0;
  private highWatermark = 10000;
  private lowWatermark = 1000;
  private highWater = false;
  private state: State = new State(">", this.tty(), this.history);
  private checkHandler: CheckHandler = () => true;
  private ctrlCHandler: CtrlCHandler = () => {
    return;
  };
  private pauseHandler: PauseHandler = (resume: boolean) => {
    return;
  };

  public activate(term: Terminal): void {
    this.term = term;
    this.term.onData(this.readData.bind(this));
    // this.term.onCursorMove(() => {
    //     console.log(`cursor: ${this.term?.buffer.active.cursorX}, ${this.term?.buffer.active.cursorY}`);
    // });
    this.term.attachCustomKeyEventHandler(this.handleKeyEvent.bind(this));
    this.term.options;
  }

  public dispose(): void {
    this.disposables.forEach((d) => d.dispose());
  }

  public appendHistory(text: string) {
    this.history.append(text);
  }

  public setCheckHandler(fn: CheckHandler) {
    this.checkHandler = fn;
  }

  public setCtrlCHandler(fn: CtrlCHandler) {
    this.ctrlCHandler = fn;
  }

  public setPauseHandler(fn: PauseHandler) {
    this.pauseHandler = fn;
  }

  public writeReady(): boolean {
    return !this.highWater;
  }

  public write(text: string) {
    if (text == "\n") {
      text = "\r\n";
    } else {
      text = text.replace(/([^\r])\n/g, "$1\r\n");
    }
    // console.trace([...text]);
    const outputLength = text.length;
    this.watermark += outputLength;
    if (this.watermark > this.highWatermark) {
      this.highWater = true;
    }
    if (this.term) {
      this.term.write(text, () => {
        this.watermark = Math.max(this.watermark - outputLength, 0);
        if (this.highWater && this.watermark < this.lowWatermark) {
          this.highWater = false;
        }
      });
    }
  }

  public print(text: string) {
    return this.write(text);
  }

  public println(text: string) {
    return this.write(text + "\r\n");
  }

  public output(): Output {
    return this;
  }

  public tty(): Tty {
    if (this.term?.options?.tabStopWidth != undefined) {
      return new Tty(
        this.term.cols,
        this.term.rows,
        this.term.options.tabStopWidth,
        this.output()
      );
    } else {
      return new Tty(0, 0, 8, this.output());
    }
  }

  public read(prompt: string): Promise<string> {
    return new Promise((resolve, reject) => {
      if (this.term == undefined) {
        reject("addon is not active");
        return;
      }
      this.state = new State(prompt, this.tty(), this.history);
      this.state.refresh();
      this.activeRead = { prompt, resolve, reject };
    });
  }

  private handleKeyEvent(event: KeyboardEvent): boolean {
    if (event.key === "Enter" && event.shiftKey) {
      if (event.type == "keydown") {
        this.readKey({
          inputType: InputType.ShiftEnter,
          data: ["\r"],
        });
      }
      return false;
    }
    return true;
  }

  private readData(data: string) {
    const input = parseInput(data);
    if (
      input.length > 1 ||
      (input[0].inputType == InputType.Text && input[0].data.length > 1)
    ) {
      this.readPaste(input);
      return;
    }
    this.readKey(input[0]);
  }

  private readPaste(input: Input[]) {
    const mappedInput = input.map((it) => {
      if (it.inputType == InputType.Enter) {
        return { inputType: InputType.Text, data: ["\n"] };
      }
      return it;
    });

    for (const it of mappedInput) {
      if (it.inputType == InputType.Text) {
        this.state.editInsert(it.data.join(""));
      } else {
        this.readKey(it);
      }
    }
  }

  private readKey(input: Input) {
    switch (input.inputType) {
      case InputType.Text:
        this.state.editInsert(input.data.join(""));
        break;
      case InputType.AltEnter:
      case InputType.ShiftEnter:
        this.state.editInsert("\n");
        break;
      case InputType.Enter:
        if (this.checkHandler(this.state.buffer())) {
          this.term?.write("\r\n");
          this.history.append(this.state.buffer());
          this.activeRead?.resolve(this.state.buffer());
          this.activeRead = undefined;
        } else {
          this.state.editInsert("\n");
        }
        break;
      case InputType.CtrlC:
        if (this.activeRead != undefined) {
          this.state.moveCursorToEnd();
          this.term?.write("^C\r\n");
          this.state = new State(
            this.activeRead.prompt,
            this.tty(),
            this.history
          );
          this.state.refresh();
          return;
        }
        this.ctrlCHandler();
        break;
      case InputType.CtrlS:
        this.pauseHandler(false);
        break;
      case InputType.CtrlQ:
        this.pauseHandler(true);
        break;
      case InputType.CtrlL:
        if (this.activeRead != undefined) {
          this.state.clearScreen();
        } else {
          this.write("\x1b[H\x1b[2J");
        }
        break;
      case InputType.Home:
      case InputType.CtrlA:
        this.state.moveCursorHome();
        break;
      case InputType.End:
      case InputType.CtrlE:
        this.state.moveCursorEnd();
        break;
      case InputType.Backspace:
        this.state.editBackspace(1);
        break;
      case InputType.Delete:
      case InputType.CtrlD:
        this.state.editDelete(1);
        break;
      case InputType.ArrowLeft:
        this.state.moveCursorBack(1);
        break;
      case InputType.ArrowRight:
        this.state.moveCursorForward(1);
        break;
      case InputType.ArrowUp:
        this.state.moveCursorUp(1);
        break;
      case InputType.ArrowDown:
        this.state.moveCursorDown(1);
        break;
      case InputType.UnsupportedControlChar:
      case InputType.UnsupportedEscape:
        console.error("unsupported key:");
        console.dir(input);
        break;
    }
  }
}
