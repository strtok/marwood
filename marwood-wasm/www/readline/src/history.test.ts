import { History } from "./history";

test('append overwrites old entries', () => {
    const history = new History(2);
    history.append("a");
    history.append("b");
    history.append("c");
    expect(history.entries).toEqual(["c", "b"]);
});

test('cursor', () => {
    const history = new History(3);
    history.append("a");
    history.append("b");
    history.append("c");
    expect(history.prev()).toEqual("c");
    expect(history.prev()).toEqual("b");
    expect(history.prev()).toEqual("a");
    expect(history.prev()).toBeUndefined();
    expect(history.next()).toEqual("b");
    expect(history.next()).toEqual("c");
    expect(history.next()).toBeUndefined();
});