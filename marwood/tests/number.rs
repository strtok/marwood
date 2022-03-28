#[macro_use]
mod common;
use marwood::cell::Cell;
use marwood::lex;
use marwood::parse;
use marwood::vm::Vm;

use marwood::vm::Error::{InvalidArgs, InvalidNumArgs, InvalidSyntax};

#[test]
fn numerical_prefixes() {
    evals![
        "#d255" => "255",
        "#xFF" => "255",
        "#xff" => "255",
        "#e#xff" => "255",
        "#i#xff" => "255.0",
        "#b11111111" => "255",
        "#b11.11" => "3.75",
        "#i#xff" => "255.0",
        "#o10" => "8",
        "#e#o10" => "8",
        "#i#o10" => "8.0",
        "1e21" => "1e21",
        "1E21" => "1e21"
    ]
}

#[test]
fn number_string() {
    evals![
        "(number->string 42)" => "\"42\"",
        "(number->string 42 10)" => "\"42\"",
        "(number->string 42.42)" => "\"42.42\"",
        "(number->string -42.42)" => "\"-42.42\"",
        "(number->string 92233720368547758070000 10)" => "\"92233720368547758070000\"",
        "(number->string 5/7)" => "\"5/7\"",

        "(number->string 42 16)" => "\"2a\"",
        "(number->string 42.5 16)" => "\"2a.8\"",
        "(number->string 42.42 16)" => "\"2a.6b851eb851ec\"",
        "(number->string -42.42 16)" => "\"-2a.6b851eb851ec\"",
        "(number->string 92233720368547758070000 16)" => "\"1387ffffffffffffd8f0\"",
        "(number->string 12/7 16)" => "\"c/7\"",

        "(number->string 42 8)" => "\"52\"",
        "(number->string 42.5 8)" => "\"52.4\"",
        "(number->string 42.42 8)" => "\"52.3270243656050754\"",
        "(number->string -42.42 8)" => "\"-52.3270243656050754\"",
        "(number->string 92233720368547758070000 8)" => "\"23417777777777777777754360\"",
        "(number->string 12/7 8)" => "\"14/7\"",

        "(number->string 42 2)" => "\"101010\"",
        "(number->string 42.5 2)" => "\"101010.1\"",
        "(number->string 42.42 2)" => "\"101010.0110101110000101000111101011100001010001111011\"",
        "(number->string -42.42 2)" => "\"-101010.0110101110000101000111101011100001010001111011\"",
        "(number->string 92233720368547758070000 2)" => "\"10011100001111111111111111111111111111111111111111111111111111101100011110000\"",
        "(number->string 12/7 2)" => "\"1100/111\""
    ];

    evals![
        "(string->number \"42\")" => "42",
        "(string->number \"42.42\")" => "42.42",
        "(string->number \"5/7\")" => "5/7",
        "(string->number \"ff\" 16)" => "255",
        "(string->number \"-1101\" 2)" => "-13"
    ];
}

#[test]
fn exact_vs_inexact() {
    evals![
        "#i1/2" => "0.5",
        "#e0.5" => "1/2"
    ];
    evals![
        "(exact->inexact 1/2)" => "0.5",
        "(inexact->exact 0.5)" => "1/2"
    ];
}

#[test]
fn arithmetic() {
    evals![
        "(+)" => "0",
        "(+ 10)" => "10",
        "(+ 10 20)" => "30",
        "(+ 10 20 30)" => "60",
        "(+ (+ 100 100) (+ 25 25) 50)" => "300",
        "(- 10)" => "-10",
        "(- 100 10)" => "90",
        "(- 1000 100 10)" => "890",
        "(*)" => "1",
        "(* 10)" => "10",
        "(* 10 10)" => "100",
        "(* 10 10 10)" => "1000"
    ];
    fails!["(+ '(10))" => InvalidArgs("+".into(), "number".into(), "(10)".into()),
           "(+ 10 '(10))" => InvalidArgs("+".into(), "number".into(), "(10)".into()),
           "(-)" => InvalidNumArgs("-".into())
    ];

    evals![
        "(/ 10 5)" => "2",
        "(/ 10 3)" => "10/3"
    ];
    fails![
        "(/ 10 0)" => InvalidSyntax("/ is undefined for 0".into())
    ];

    evals![
        "(quotient 10 3)" => "3",
        "(remainder 10 3)" => "1",
        "(quotient 10/1 3/1)" => "3",
        "(remainder 10/1 3/1)" => "1"
    ];

    fails![
        "(remainder 10 0)" => InvalidSyntax("remainder is undefined for 0".into()),
        "(quotient 10 0)" => InvalidSyntax("quotient is undefined for 0".into())
    ];

    evals![
        "(modulo -21 4)" => "3"
    ];

    evals![
        "(abs -10)" => "10",
        "(abs  10)" => "10",
        "(abs -10.0)" => "10",
        "(abs  10.0)" => "10",
        "(abs -1/2)" => "1/2",
        "(abs  1/2)" => "1/2"
    ];

    evals![
        "(min 100 -100)" => "-100",
        "(min 0 -100 100)" => "-100",
        "(max 0 100)" => "100",
        "(max 0 -100 100)" => "100"
    ];

    evals![
        "(floor -4.3)" => "-5",
        "(ceiling -4.3)" => "-4",
        "(truncate -4.3)" => "-4",
        "(round -4.3)" => "-4",
        "(floor 3.5)" => "3",
        "(ceiling 3.5)" => "4",
        "(truncate 3.5)" => "3",
        "(round 3.5)" => "4",
        "(round 7/2)" => "4"
    ];

    evals![
        "(numerator (/ 6 4))" => "3",
        "(denominator (/ 6 4))" => "2",
        "(denominator (exact->inexact (/ 6 4)))" => "2"
    ];

    evals![
        "(sin 0)" => "0.0",
        "(sin (/ 3.14159265359 2))" => "1.0",
        "(cos 0)" => "1.0",
        "(tan 0)" => "0.0",
        "(asin 1.0)" => "1.5707963267948966",
        "(acos 1.0)" => "0.0",
        "(atan 0.0)" => "0.0",
        "(atan 0.0 0.0)" => "0.0",
        "(atan 1.0 -1.0)" => "-0.7853981633974483",
        "(log 2.718281828459045)" => "1.0",
        "(exp 1.0)" => "2.718281828459045",
        "(sqrt 9)" => "3.0"
    ];

    let mut vm = Vm::new();
    assert!(vm
        .eval(&parse!("(sin (expt 10.0 309))"))
        .unwrap()
        .as_number()
        .unwrap()
        .to_f64()
        .unwrap()
        .is_nan());

    evals![
        "(expt 2 2)" => "4",
        "(expt 3.5 2)" => "12.25",
        "(expt 10.0 309)" => "inf",
        "(expt 5/7 3)" => "125/343"
    ];
}

#[test]
fn num_compare() {
    evals![
    "(= 1 1)" => "#t",
    "(= 1 1 1 1 1)" => "#t",
    "(= 1 1 2 1 1)" => "#f"
    ];
    evals![
      "(> 5 4)" => "#t",
      "(> 5 4 3 2 1)" => "#t",
      "(> 5 4 3 4 2 1)" => "#f"
    ];
    evals![
      "(>= 2 1)" => "#t",
      "(>= 5 4 3 3 2 1)" => "#t",
      "(>= 5 4 3 4 2 1)" => "#f"
    ];
    evals![
      "(< 1 2)" => "#t",
      "(< 1 2 3 4 5)" => "#t",
      "(< 1 2 1 3 4)" => "#f"
    ];
    evals![
      "(>= 5 4)" => "#t",
      "(>= 5 4 3 3 2 1)" => "#t",
      "(>= 5 4 3 2 3 1)" => "#f"
    ];
    evals![
        "(> 2.0 1)" => "#t",
        "(> 2 1.0)" => "#t"
    ];
}

#[test]
fn num_unary() {
    evals![
        "(zero? 0)" => "#t",
        "(zero? 10)" => "#f",
        "(positive? 10)" => "#t",
        "(positive? 0)" => "#f",
        "(positive? -10)" => "#f",
        "(negative? -10)" => "#t",
        "(negative? 0)" => "#f",
        "(negative? 10)" => "#f",
        "(odd? 3)" => "#t",
        "(odd? 2)" => "#f",
        "(even? 2)" => "#t",
        "(even? 3)" => "#f"
    ];
}
