export class History {
    public entries: string[] = []
    public maxEntries: number;
    public cursor = -1;

    constructor(maxEntries: number) {
        this.maxEntries = maxEntries;
    }

    public append(text: string) {
        this.resetCursor();
        if (!this.entries.includes(text)) {
            this.entries.unshift(text);
        } else {
            this.entries.splice(this.entries.indexOf(text), 1);
            this.entries.unshift(text);
        }
        if (this.entries.length > this.maxEntries) {
            this.entries.pop();
        }
    }

    public resetCursor() {
        this.cursor = -1;
    }

    public next(): string | undefined {
        if (this.cursor == -1) {
            return undefined;
        } else {
            this.cursor -= 1;
        }

        return this.entries[this.cursor];
    }

    public prev(): string | undefined {
        if (this.cursor + 1 >= this.entries.length) {
            return undefined;
        } else {
            this.cursor += 1;
        }

        return this.entries[this.cursor];
    }
}