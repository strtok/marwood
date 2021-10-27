use crate::parcom::*;
use std::collections::HashSet;

pub fn any_char(input: &str) -> ParseResult<&str, String> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], Some(c.into()))),
        None => Err(input),
    }
}

pub fn ch<'a>(expected: char) -> impl Parser<&'a str, String> {
    satisfy(any_char, move |s| s.chars().next().unwrap() == expected)
}

pub fn digit_char<'a>() -> impl Parser<&'a str, String> {
    satisfy(any_char, |s| s.chars().next().unwrap().is_digit(10))
}

pub fn alphabetic_char<'a>() -> impl Parser<&'a str, String> {
    satisfy(any_char, |s| s.chars().next().unwrap().is_alphabetic())
}

pub fn whitespace_char<'a>() -> impl Parser<&'a str, String> {
    satisfy(any_char, |s| s.chars().next().unwrap().is_whitespace())
}

pub fn one_of_char<'a>(set: &str) -> impl Parser<&'a str, String> {
    let set: HashSet<_> = set.chars().clone().collect();
    satisfy(any_char, move |s| set.contains(&s.chars().next().unwrap()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn any_char_returns_any_char() {
        assert_eq!(any_char("dog"), Ok(("og", Some("d".to_owned()))));
    }

    #[test]
    fn chars() {
        assert_eq!(ch('a').apply("abc"), Ok(("bc", Some("a".to_owned()))));
        assert_eq!(ch('b').apply("cba"), Err("cba"));
    }

    #[test]
    fn digits() {
        assert_eq!(digit_char().apply("42"), Ok(("2", Some("4".to_owned()))));
        assert_eq!(digit_char().apply("dog"), Err("dog"));
    }

    #[test]
    fn alphabetics() {
        assert_eq!(
            alphabetic_char().apply("abc"),
            Ok(("bc", Some("a".to_owned())))
        );
        assert_eq!(alphabetic_char().apply("123"), Err("123"));
    }

    #[test]
    fn whitespaces() {
        assert_eq!(
            whitespace_char().apply(" bc"),
            Ok(("bc", Some(" ".to_owned())))
        );
    }

    #[test]
    fn one_of_char_checks_set() {
        assert_eq!(
            one_of_char("abc").apply("abc"),
            Ok(("bc", Some("a".to_owned())))
        );
        assert_eq!(
            one_of_char("abc").apply("bac"),
            Ok(("ac", Some("b".to_owned())))
        );
        assert_eq!(one_of_char("wrt").apply("abc"), Err("abc"));
    }
}
