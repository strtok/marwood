use crate::cell::Cell;
use parcom::parcom::{
    between, collect, discard, mapv, one_of, optional, repeat1, repeatc, seq, ParseResult, Parser,
};
use parcom::parcom_str::{alphabetic_char, ch, digit_char, one_of_char, whitespace_char};
use parcom::{one_of, seqc};

pub fn identifier<'a>() -> impl Parser<&'a str, String> {
    let initial_identifier = || one_of!(alphabetic_char(), one_of_char("!$%&*/:<=>?^_~"));
    let peculiar_identifier = one_of_char("+-");
    let subsequent_identifier = one_of!(initial_identifier(), alphabetic_char(), digit_char());

    one_of!(
        seqc!(initial_identifier(), repeatc(subsequent_identifier)),
        peculiar_identifier
    )
}

pub fn variable<'a>() -> impl Parser<&'a str, Cell> {
    mapv(identifier(), Cell::Symbol)
}

pub fn number<'a>() -> impl Parser<&'a str, Cell> {
    let digit = one_of_char("0123456789");
    let sign = one_of_char("+-");
    let num10 = mapv(
        seqc!(optional(sign), collect(repeat1(digit))),
        |s: String| match s.parse::<i64>() {
            Ok(n) => Cell::Number(n),
            Err(_) => Cell::Number(0),
        },
    );

    mapv(one_of!(num10), Cell::from)
}

pub fn ows<'a>() -> impl Parser<&'a str, String> {
    discard(optional(whitespace_char()))
}

pub fn procedure_call<'a>() -> impl Parser<&'a str, Cell> {
    mapv(
        between(ch('('), repeat1(between(ows(), expression, ows())), ch(')')),
        Cell::list,
    )
}

pub fn expression(input: &str) -> ParseResult<&str, Cell> {
    one_of!(procedure_call(), number(), variable()).apply(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{cell, list};

    macro_rules! parses {
        ($parser:expr, $($lhs:expr => $rhs:expr),+) => {{
             $(
                assert_eq!($parser.apply($lhs), Ok(("", Some($rhs))));
             )+
        }};
    }

    macro_rules! fails {
        ($parser:expr, $($lhs:expr),+) => {{
             $(
                assert_eq!($parser.apply($lhs), Err($lhs));
             )+
        }};
    }

    #[test]
    fn variables() {
        parses!(variable(),
            "foo" => cell!["foo"],
            "bar" => cell!["bar"]
        );
        fails!(variable(), "...foo");
    }

    #[test]
    fn procedures() {
        parses!(procedure_call(),
            "(foo)" => list!["foo"],
            "( foo )" => list!["foo"],
            "(foo bar baz)" => list!["foo", "bar", "baz"]
        );

        fails!(procedure_call(), "()", "( )", "(  )");
    }

    #[test]
    fn numbers() {
        parses!(number(),
            "42" => cell![42],
            "+42" => cell![42],
            "-42" => cell![-42]
        );
    }

    #[test]
    fn expressions() {
        parses!(expression,
            "foo" => cell!["foo"],
            "42" => cell![42],
            "-18" => cell![-18],
            "(foo)" => list!["foo"],
            "(foo (bar baz))" => list!["foo", list!["bar", "baz"]]
        );
    }
}
