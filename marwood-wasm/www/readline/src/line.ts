type RepeatCount = number;

export class LineBuffer {
    public buf = "";
    public pos = 0;

    public buffer(): string {
        return this.buf;
    }

    public pos_buffer(): string {
        return this.buf.slice(0, this.pos);
    }

    // Return length of buffer in bytes
    public length(): number {
        return this.buf.length;
    }

    // Return length of buffer in characters
    public char_length(): number {
        return [...this.buf].length;
    }

    // Set text and position
    public update(text: string, pos: number) {
        this.buf = text;
        this.pos = pos;
    }

    public insert(text: string): boolean {
        const shift = text.length;
        const push = this.pos == this.buf.length;
        if (push) {
            this.buf = this.buf + text;
        } else {
            this.buf = this.buf.slice(0, this.pos) + text + this.buf.slice(this.pos);
        }
        this.pos += shift;
        return push;
    }

    public moveBack(n: number): boolean {
        const pos = this.prevPos(n);
        if (pos != undefined) {
            this.pos = pos;
            return true;
        } else {
            return false;
        }
    }

    public moveForward(n: number): boolean {
        const pos = this.nextPos(n);
        if (pos != undefined) {
            this.pos = pos;
            return true;
        } else {
            return false;
        }
    }

    public moveLineUp(n: number): boolean {
        const off = this.buf.slice(0, this.pos).lastIndexOf("\n");
        if (off == -1) {
            return false;
        }
        const column = [...this.buf.slice(off + 1, this.pos)].length;
        let destStart = this.buf.slice(0, off).lastIndexOf("\n");
        if (destStart == -1) {
            destStart = 0;
        } else {
            destStart = destStart + 1;
        }
        let destEnd = off;

        for (let i = 1; i < n; i++) {
            if (destStart == 0) {
                break;
            }
            destEnd = destStart - 1;
            destStart = this.buf.slice(0, destEnd).lastIndexOf("\n");
            if (destStart == -1) {
                destStart = 0;
            } else {
                destStart = destStart + 1;
            }
        }

        const slice = [...this.buf.slice(destStart, destEnd)]
            .slice(0, column);

        let gIdx = off;
        if (slice.length > 0) {
            gIdx = slice
                .map((c) => c.length)
                .reduce((acc, n) => acc + n, 0);
            gIdx = destStart + gIdx;
        }
        this.pos = gIdx;
        return true;
    }

    public moveLineDown(n: number): boolean {
        const off = this.buf.slice(this.pos).indexOf("\n");
        if (off == -1) {
            return false;
        }

        let lineStart = this.buf.slice(0, this.pos).lastIndexOf("\n");
        if (lineStart == -1) {
            lineStart = 0;
        } else {
            lineStart += 1;
        }

        const column = [...this.buf.slice(lineStart, this.pos)].length;
        let destStart = this.pos + off + 1;

        let destEnd = this.buf.slice(destStart).indexOf("\n");
        if (destEnd == -1) {
            destEnd = this.buf.length;
        } else {
            destEnd = destStart + destEnd
        }

        for (let i = 1; i < n; i++) {
            if (destEnd == this.buf.length) {
                break;
            }
            destStart = destEnd + 1;
            destEnd = this.buf.slice(destStart).indexOf("\n");
            if (destEnd == -1) {
                destEnd = this.buf.length;
            } else {
                destEnd = destStart + destEnd;
            }
        }

        const slice = [...this.buf.slice(destStart, destEnd)];
        if (column < slice.length) {
            this.pos = slice.slice(0, column)
                .map((c) => c.length)
                .reduce((acc, n) => acc + n, 0) + destStart;
        } else {
            this.pos = destEnd;
        }

        return true;
    }

    // Set position of cursor
    public set_pos(pos: number) {
        this.pos = pos;
    }

    // Return the position of the character preceding 
    // pos
    public prevPos(n: RepeatCount): number | undefined {
        if (this.pos == 0) {
            return undefined;
        }
        const buf = this.buf.slice(0, this.pos);
        return this.pos - [...buf].slice(-n)
            .map((c) => c.length)
            .reduce((acc, n) => acc + n, 0);
    }

    // Return the position of the character following the 
    // current pos
    public nextPos(n: RepeatCount): number | undefined {
        if (this.pos == this.buf.length) {
            return undefined;
        }
        const buf = this.buf.slice(this.pos);
        return this.pos + [...buf].slice(0, n)
            .map((c) => c.length)
            .reduce((acc, n) => acc + n, 0);
    }

    public backspace(n: RepeatCount): boolean {
        const newPos = this.prevPos(n);
        if (newPos == undefined) {
            return false;
        }
        this.buf = this.buf.slice(0, newPos) + this.buf.slice(this.pos);
        this.pos = newPos;
        return true;
    }
}

