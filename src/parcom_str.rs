use crate::parcom::*;

fn any_char(input: &str) -> ParseResult<&str, &str> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], Some(&input[0..c.len_utf8()]))),
        None => Err(input),
    }
}

fn ch<'a>(expected: char) -> impl Parser<&'a str, &'a str> {
    satisfy(any_char, move |&s| s.chars().next().unwrap() == expected)
}

fn digit_char<'a>() -> impl Parser<&'a str, &'a str> {
    satisfy(any_char, |&s| s.chars().next().unwrap().is_digit(10))
}

fn alphabetic_char<'a>() -> impl Parser<&'a str, &'a str> {
    satisfy(any_char, |&s| s.chars().next().unwrap().is_alphabetic())
}

fn whitespace_char<'a>() -> impl Parser<&'a str, &'a str> {
    satisfy(any_char, |&s| s.chars().next().unwrap().is_whitespace())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn any_char_returns_any_char() {
        assert_eq!(any_char("dog"), Ok(("og", Some("d"))));
    }

    #[test]
    fn chars() {
        assert_eq!(ch('a').apply("abc"), Ok(("bc", Some("a"))));
        assert_eq!(ch('b').apply("cba"), Err("cba"));
    }

    #[test]
    fn digits() {
        assert_eq!(digit_char().apply("42"), Ok(("2", Some("4"))));
        assert_eq!(digit_char().apply("dog"), Err("dog"));
    }

    #[test]
    fn alphabetics() {
        assert_eq!(alphabetic_char().apply("abc"), Ok(("bc", Some("a"))));
        assert_eq!(alphabetic_char().apply("123"), Err("123"));
    }

    #[test]
    fn whitespaces() {
        assert_eq!(whitespace_char().apply(" bc"), Ok(("bc", Some(" "))));
    }
}
