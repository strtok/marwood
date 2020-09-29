use nom::{InputTake, AsChar};

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
    where
        F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn satisfy<'a>(f: impl Fn(char) -> bool) -> impl Parser<'a, char> {
    move |input: &'a str| {
        match input.chars().next() {
            Some(c) if f(c) => Ok((&input[1..], c)),
            _ => Err(input)
        }
    }
}

fn ch<'a>(expected: char) -> impl Parser<'a, char> {
    satisfy(move |c: char| c == expected)
}

fn digit<'a>() -> impl Parser<'a, char> {
    satisfy(move |c: char| c.is_digit(10))
}

fn alpha<'a>() -> impl Parser<'a, char> {
    satisfy(move |c: char| c.is_alpha())
}

fn empty(input: &str) -> ParseResult<()> {
    Ok((input, ()))
}

fn any<'a>(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[1..], c)),
        None => Err(input)
    }
}

// Combinators

fn map<'a, P, F, A, B>(parser: P, func: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        F: Fn(A) -> B,
{
    move |input|
        parser.parse(input)
            .map(|(rest, output)| (rest, func(output)))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
        move |input: &'a str| match input.get(0..expected.len()) {
            Some(next) if next == expected => Ok((&input[expected.len()..], ())),
            _ => Err(input),
        }
    }

    #[test]
    fn empty_doesnt_consume() {
        assert_eq!(empty("dog"), Ok(("dog", ())));
    }

    #[test]
    fn any_returns_any_token() {
        assert_eq!(any("dog"), Ok(("og", 'd')));
    }

    #[test]
    fn satisfy_returns_matched_char() {
        assert_eq!(satisfy(|ch| ch == 'd').parse("dog"),
                   Ok(("og", 'd')));
        assert_eq!(satisfy(|ch| ch == 'o').parse("dog"),
                   Err("dog"));
    }

    #[test]
    fn is_digit() {
        assert_eq!(digit().parse("42"),
                   Ok(("2", '4')));
        assert_eq!(digit().parse("dog"),
                   Err("dog"));
    }

    #[test]
    fn is_alpha() {
        assert_eq!(alpha().parse("abc"),
                   Ok(("bc", 'a')));
        assert_eq!(alpha().parse("123"),
                   Err("123"));
    }
}