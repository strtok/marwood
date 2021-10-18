use crate::cell::Cell;
use crate::parcom::{collect, into, mapv, one_of, repeat, repeatc, seq, ParseResult, Parser};
use crate::parcom_str::{alphabetic_char, ch, digit_char, one_of_char};
use std::ops::Deref;

#[rustfmt::skip]
pub fn initial_identifier<'a>() -> impl Parser<&'a str, String> {
    into(one_of!(alphabetic_char(),
                 one_of_char("!$%&*/:<=>?^_~")))
}

#[rustfmt::skip]
pub fn peculiar_identifier<'a>() -> impl Parser<&'a str, String> {
    into(one_of_char("+-"))
}

#[rustfmt::skip]
pub fn subsequent_identifier<'a>() -> impl Parser<&'a str, String> {
    one_of!(initial_identifier(),
            into(alphabetic_char()),
            into(digit_char()))
}

#[rustfmt::skip]
pub fn identifier<'a>() -> impl Parser<&'a str, Cell> {
    mapv(
        one_of!(seqc!(initial_identifier(),
                      repeatc(subsequent_identifier())),
                peculiar_identifier()),
        Cell::Symbol,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifier() {
        assert_eq!(
            super::identifier().apply("foo"),
            Ok(("", Some(Cell::Symbol("foo".to_string()))))
        );
        assert_eq!(super::identifier().apply("...foo"), Err("...foo",));
    }
}
