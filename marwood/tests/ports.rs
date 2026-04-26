#[macro_use]
mod common;
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;

#[test]
fn eof_object() {
    prints![
        "(eof-object)" => "#!eof"
    ];

    evals![
        "(eof-object? (eof-object))" => "#t",
        "(eof-object? '())" => "#f",
        "(eof-object? #f)" => "#f",
        "(eof-object? 42)" => "#f",
        "(eof-object? #\\a)" => "#f",
        "(eq? (eof-object) (eof-object))" => "#t"
    ];
}

#[test]
fn read_char_default_returns_eof() {
    // Default StubInterface yields no input; read-char/peek-char return eof-object,
    // and char-ready? returns #f.
    evals![
        "(eof-object? (read-char))" => "#t",
        "(eof-object? (peek-char))" => "#t",
        "(char-ready?)" => "#f"
    ];
}
