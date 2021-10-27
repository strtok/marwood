use crate::cell::Cell;
use parcom::parcom::{
    between, collect, discard, mapv, one_of, optional, repeat, repeatc, seq, Parser,
};
use parcom::parcom_str::{alphabetic_char, ch, digit_char, one_of_char, whitespace_char};
use parcom::{one_of, seq, seqc};

#[rustfmt::skip]
pub fn initial_identifier<'a>() -> impl Parser<&'a str, String> {
    one_of!(alphabetic_char(),
             one_of_char("!$%&*/:<=>?^_~"))
}

#[rustfmt::skip]
pub fn peculiar_identifier<'a>() -> impl Parser<&'a str, String> {
    one_of_char("+-")
}

#[rustfmt::skip]
pub fn subsequent_identifier<'a>() -> impl Parser<&'a str, String> {
    one_of!(initial_identifier(),
            alphabetic_char(),
            digit_char())
}

#[rustfmt::skip]
pub fn identifier<'a>() -> impl Parser<&'a str, String> {
        one_of!(seqc!(initial_identifier(),
                      repeatc(subsequent_identifier())),
                peculiar_identifier())
}

#[rustfmt::skip]
pub fn variable<'a>() -> impl Parser<&'a str, String> {
    identifier()
}

#[rustfmt::skip]
pub fn ows<'a>() -> impl Parser<&'a str, String> {
    discard(optional(whitespace_char()))
}

#[rustfmt::skip]
pub fn procedure_call<'a>() -> impl Parser<&'a str, Cell> {
    mapv(
        between(ch('('), 
                repeat(
                    collect(
                        seq!(ows(), variable(), ows())
                    )
                ), 
                ch(')')),
        |v: Vec<String>| Cell::Symbol(v.first().unwrap().to_owned()),
    )
}

#[rustfmt::skip]
pub fn expression<'a>() -> impl Parser<&'a str, Cell> {
    one_of!(procedure_call())
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
            Ok(("", Some(Cell::Symbol("foo".to_owned()))))
        );
        assert_eq!(
            super::procedure_call().apply("( foo )"),
            Ok(("", Some(Cell::Symbol("foo".to_owned()))))
        );

        assert_eq!(
            super::procedure_call().apply("( foo bar baz )"),
            Ok(("", Some(Cell::Symbol("foo".to_owned()))))
        );
    }

    #[test]
    fn expression() {
        assert_eq!(
            super::expression().apply("(foo)"),
            Ok(("", Some(Cell::Symbol("foo".to_owned()))))
        );
    }
}
