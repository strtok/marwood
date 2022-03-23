#[macro_use]
mod common;
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;

use marwood::vm::Error::{InvalidStringIndex, InvalidSyntax};

#[test]
fn eval_string_char_literals() {
    evals![
       "#\\a" => "#\\a",
       "#\\A" => "#\\A",
       "#\\)" => "#\\)",
       "#\\(" => "#\\(",
       "#\\newline" => "#\\newline",
       "#\\space" => "#\\space",
       "#\\ " => "#\\space"
    ];

    evals![
        "#\\x9b" => "#\\x9b"
    ];

    evals![
        r#""foo \"bar\" baz""# => r#""foo \"bar\" baz""#
    ]
}

#[test]
fn char_procedures() {
    evals![
        "(integer->char (char->integer #\\a))" => "#\\a",
        "(integer->char (char->integer #\\𒀀))" => "#\\𒀀",
        "(integer->char (char->integer #\\alarm))" => "#\\alarm",
        "(integer->char (char->integer #\\backspace))" => "#\\backspace",
        "(integer->char (char->integer #\\delete))" => "#\\delete",
        "(integer->char (char->integer #\\escape))" => "#\\escape",
        "(integer->char (char->integer #\\newline))" => "#\\newline",
        "(integer->char (char->integer #\\null))" => "#\\null",
        "(integer->char (char->integer #\\return))" => "#\\return",
        "(integer->char (char->integer #\\space))" => "#\\space",
        "(integer->char (char->integer #\\tab))" => "#\\tab"
    ];
    fails![
        "(integer->char #xffffffffff)" => InvalidSyntax("1099511627775 is not valid unicode".into())
    ];
    evals![
        "(char-alphabetic? #\\a)" => "#t",
        "(char-alphabetic? #\\A)" => "#t",
        "(char-alphabetic? #\\1)" => "#f",
        "(char-numeric? #\\1)" => "#t",
        "(char-numeric? #\\a)" => "#f",
        "(char-whitespace? #\\space)" => "#t",
        "(char-whitespace? #\\tab)" => "#t",
        "(char-upper-case? #\\a)" => "#f",
        "(char-upper-case? #\\A)" => "#t",
        "(char-lower-case? #\\a)" => "#t",
        "(char-lower-case? #\\A)" => "#f"
    ];

    evals![
        "(char=? #\\a #\\a)" => "#t",
        "(char=? #\\a #\\b)" => "#f",
        "(char=? #\\A #\\a)" => "#f",
        "(char-ci=? #\\A #\\a)" => "#t",

        "(char<? #\\a #\\b)" => "#t",
        "(char<? #\\a #\\B)" => "#f",
        "(char-ci<? #\\a #\\B)" => "#t",
        "(char<? #\\a #\\a)" => "#f",
        "(char<=? #\\a #\\a)" => "#t",

        "(char>? #\\b #\\a)" => "#t",
        "(char-ci>? #\\b #\\A)" => "#t",
        "(char>? #\\a #\\a)" => "#f",
        "(char>=? #\\a #\\a)" => "#t"
    ];

    evals![
        "(char-upcase #\\a)" => "#\\A",
        "(char-upcase #\\λ)" => "#\\Λ",

        "(char-downcase #\\A)" => "#\\a",
        "(char-downcase #\\Λ)" => "#\\λ",

        // ß -> SS
        "(char-downcase #\\ß)" => "#\\ß",
        "(char-foldcase #\\Σ)" => "#\\σ"
    ];

    evals![
        "(digit-value #\\9)" => "9",
        "(digit-value #\\a)" => "#f"
    ];
}

#[test]
fn string_procedures() {
    evals![
        "(make-string 5)" => "\"\0\0\0\0\0\"",
        "(make-string 5 #\\a)" => "\"aaaaa\""
    ];
    evals![
        "(string #\\a)" => "\"a\"",
        "(string #\\a #\\b)" => "\"ab\""
    ];
    evals![
        "(string-length \"foo\")" => "3",
        "(string-length \"🐶\")" => "1"
    ];
    evals![
        "(string-ref \"o🐶o\" 0)" => "#\\o",
        "(string-ref \"o🐶o\" 1)" => "#\\🐶",
        "(string-ref \"o🐶o\" 2)" => "#\\o"
    ];
    fails!["(string-ref \"o🐶o\" 3)" => InvalidStringIndex(3, 2)];
    evals![
        "(define owo \"o🐶o\")" => "#<void>",
        "(string-set! owo 0 #\\f)" => "#<void>",
        "owo" => "\"f🐶o\"",
        "(define owo \"o🐶o\")" => "#<void>",
        "(string-set! owo 1 #\\w)" => "#<void>",
        "owo" => "\"owo\"",
        "(define owo \"o🐶o\")" => "#<void>",
        "(string-set! owo 2 #\\f)" => "#<void>",
        "owo" => "\"o🐶f\""
    ];
    fails!["(string-set! \"o🐶o\" 3 #\\f)" => InvalidStringIndex(3, 2)];
    evals![
        "(string-append \"foo\")" => "\"foo\"",
        "(string-append \"foo \" \"bar\")" => "\"foo bar\""
    ];

    evals![
        "(string=? \"foo\" \"foo\")" => "#t",
        "(string<? \"boo\" \"foo\")" => "#t",
        "(string>? \"foo\" \"boo\")" => "#t",
        "(string<=? \"foo\" \"foo\")" => "#t",
        "(string<=? \"boo\" \"foo\")" => "#t",
        "(string>=? \"foo\" \"foo\")" => "#t",
        "(string>=? \"foo\" \"boo\")" => "#t",
        "(string-ci=? \"Foo\" \"foo\")" => "#t",
        "(string-ci<? \"Boo\" \"foo\")" => "#t",
        "(string-ci>? \"Foo\" \"boo\")" => "#t",
        "(string-ci<=? \"Foo\" \"foo\")" => "#t",
        "(string-ci<=? \"Boo\" \"foo\")" => "#t",
        "(string-ci>=? \"Foo\" \"foo\")" => "#t",
        "(string-ci>=? \"Foo\" \"boo\")" => "#t"
    ];
    evals![
        "(string-downcase \"FOO\")" => "\"foo\"",
        "(string-upcase \"foo\")" => "\"FOO\"",
        "(string-foldcase \"FOO\")" => "\"foo\""
    ];

    evals![
        "(substring \"o🐶o\" 0 0)" => "\"\"",
        "(substring \"o🐶o\" 0 1)" => "\"o\"",
        "(substring \"o🐶o\" 0 2)" => "\"o🐶\"",
        "(substring \"o🐶o\" 0 3)" => "\"o🐶o\"",
        "(substring \"o🐶o\" 1 2)" => "\"🐶\"",
        "(substring \"o🐶o\" 2 3)" => "\"o\"",
        "(substring \"o🐶o\" 3 3)" => "\"\""
    ];
    fails![
        "(substring \"o🐶o\" 2 1)" => InvalidSyntax("invalid substring indices: end < start".into())
    ];

    evals![
        "(string-copy \"o🐶o\")" => "\"o🐶o\"",
        "(string-copy \"o🐶o\" 0)" => "\"o🐶o\"",
        "(string-copy \"o🐶o\" 1)" => "\"🐶o\"",
        "(string-copy \"o🐶o\" 2)" => "\"o\"",
        "(string-copy \"o🐶o\" 3)" => "\"\"",
        "(string-copy \"o🐶o\" 0 0)" => "\"\"",
        "(string-copy \"o🐶o\" 0 1)" => "\"o\"",
        "(string-copy \"o🐶o\" 0 2)" => "\"o🐶\"",
        "(string-copy \"o🐶o\" 0 3)" => "\"o🐶o\"",
        "(string-copy \"o🐶o\" 1 2)" => "\"🐶\"",
        "(string-copy \"o🐶o\" 2 3)" => "\"o\"",
        "(string-copy \"o🐶o\" 3 3)" => "\"\""
    ];

    evals![
        "(string->list \"o🐶o\")" => "(#\\o #\\🐶 #\\o)",
        "(string->list \"o🐶o\" 0)" => "(#\\o #\\🐶 #\\o)",
        "(string->list \"o🐶o\" 1)" => "(#\\🐶 #\\o)",
        "(string->list \"o🐶o\" 2)" => "(#\\o)",
        "(string->list \"o🐶o\" 3)" => "()",
        "(string->list \"o🐶o\" 0 1)" => "(#\\o)",
        "(string->list \"o🐶o\" 0 2)" => "(#\\o #\\🐶)",
        "(string->list \"o🐶o\" 0 3)" => "(#\\o #\\🐶 #\\o)",
        "(string->list \"o🐶o\" 1 2)" => "(#\\🐶)",
        "(string->list \"o🐶o\" 2 3)" => "(#\\o)",
        "(string->list \"o🐶o\" 3 3)" => "()"
    ];

    evals![
        "(list->string '(#\\o #\\🐶 #\\o))" => "\"o🐶o\""
    ];
    fails![
        "(list->string '(10))" => InvalidSyntax("list->string expected char but found 10".into())
    ];

    evals![
        "(list->string (string->list \"o🐶o\"))" => "\"o🐶o\""
    ];

    evals![
        "(define owo \"o🐶o\")" => "#<void>",
        "(string-fill! owo #\\z)" => "#<void>",
        "owo" => "\"zzz\"",
        "(define owo \"o🐶o\")" => "#<void>",
        "(string-fill! owo #\\z 1)" => "#<void>",
        "owo" => "\"ozz\"",
        "(define owo \"o🐶o\")" => "#<void>",
        "(string-fill! owo #\\z 1 2)" => "#<void>",
        "owo" => "\"ozo\""
    ];
}

#[test]
fn symbol_procedures() {
    evals!["(string->symbol \"12foo\")" => "\\x31;2foo",
           "(string->symbol \" foo\")" => "\\x20;foo",
           "(symbol->string (string->symbol \"12foo\"))" =>  "\"12foo\"",
           "(symbol->string (string->symbol \" foo\"))" =>  "\" foo\""
    ];
    evals!["(symbol=? 'foo 'foo 'foo)" => "#t",
           "(symbol=? 'foo 'foo 'bar 'foo)" => "#f"
    ];
}

#[test]
fn vector_conversion() {
    evals!["(vector->string (string->vector \"foo\"))" => "\"foo\""];
}
