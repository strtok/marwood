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

    #[test]
    fn identifier() {
        assert_eq!(
            super::identifier().apply("foo"),
            Ok(("", Some("foo".to_owned())))
        );

        assert_eq!(super::identifier().apply("...foo"), Err("...foo",));
    }

    #[test]
    fn procedure_call() {
        assert_eq!(
            super::procedure_call().apply("(foo)"),
            Ok(("", Some(Cell::list(vec!(Cell::symbol("foo"))))))
        );
        assert_eq!(
            super::procedure_call().apply("( foo )"),
            Ok(("", Some(Cell::list(vec!(Cell::symbol("foo"))))))
        );
        assert_eq!(
            super::procedure_call().apply("( foo bar baz )"),
            Ok((
                "",
                Some(Cell::list(vec!(
                    Cell::symbol("foo"),
                    Cell::symbol("bar"),
                    Cell::symbol("baz")
                )))
            ))
        );
        assert_eq!(super::procedure_call().apply("()"), Err("()"));
        assert_eq!(super::procedure_call().apply("( )"), Err("( )"));
        assert_eq!(super::procedure_call().apply("(  )"), Err("(  )"));
    }

    #[test]
    fn expression() {
        assert_eq!(
            super::expression("(foo (bar baz))"),
            Ok((
                "",
                Some(Cell::list(vec!(
                    Cell::symbol("foo"),
                    Cell::list(vec!(Cell::symbol("bar"), Cell::symbol("baz")))
                )))
            ))
        );
    }

    #[test]
    fn number() {
        assert_eq!(
            super::number().apply("42"),
            Ok(("", Some(Cell::Number(42))))
        );
        assert_eq!(
            super::number().apply("+42"),
            Ok(("", Some(Cell::Number(42))))
        );
        assert_eq!(
            super::number().apply("-42"),
            Ok(("", Some(Cell::Number(-42))))
        );
    }
}
