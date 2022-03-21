import { Position } from "./state"
import { Tty } from "./tty";

class Output {
    output: string[] = [];
    write = jest.fn((text: string) => this.output.push(text));
    print = jest.fn((text: string) => this.output.push(text));
    println = jest.fn((text: string) => this.output.push(text));
}

test('calculate position', () => {
    const orig = new Position(0, 0);
    const tty = new Tty(80, 24, 8, new Output);

    expect(tty.calculatePosition("foo", orig))
        .toEqual(new Position(0, 3));

    expect(tty.calculatePosition("\x1b[1;32mfoo", orig))
        .toEqual(new Position(0, 3));

    expect(tty.calculatePosition("foo\nbar", orig))
        .toEqual(new Position(1, 3));
});