use std::fmt::Debug;

type ParseResult<'a, I, O> = Result<(I, Option<O>), I>;

trait Parser<'a, I, O> {
    fn apply(&self, input: I) -> ParseResult<'a, I, O>;
}

impl<'a, F, I, O> Parser<'a, I, O> for F
    where F: Fn(I) -> ParseResult<'a, I, O>,
{
    fn apply(&self, input: I) -> ParseResult<'a, I, O> {
        self(input)
    }
}

fn any_char<'a>(input: &str) -> ParseResult<&str, &str> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], Some(&input[0..c.len_utf8()]))),
        None => Err(input)
    }
}

fn empty<'a, I>(input: I) -> ParseResult<'a, I, ()> {
    Ok((input, None))
}


fn satisfy<'a, P, F, I, O>(parser: P, f: F) -> impl Parser<'a, I, O>
    where
        F: Fn(&O) -> bool,
        P: Parser<'a, I, O>,
        I: Copy
{
    move |input| {
        match parser.apply(input) {
            Ok((rest, None)) => Ok((rest, None)),
            Ok((rest, Some(output))) if f(&output) => Ok((rest, Some(output))),
            _ => Err(input)
        }
    }
}


fn ch<'a>(expected: char) -> impl Parser<'a, &'a str, &'a str> {
    satisfy(any_char, move  |&s| s.chars().next().unwrap() == expected)
}

fn digit_char<'a>() -> impl Parser<'a, &'a str, &'a str> {
    satisfy(any_char, |&s| s.chars().next().unwrap().is_digit(10))
}

fn alphabetic_char<'a>() -> impl Parser<'a, &'a str, &'a str>  {
    satisfy(any_char,  |&s| s.chars().next().unwrap().is_alphabetic())
}

fn whitespace_char<'a>() -> impl Parser<'a, &'a str, &'a str>  {
    satisfy(any_char,  |&s| s.chars().next().unwrap().is_whitespace())
}

//
// Combinators
//

fn map<'a, P, F, I, A, B>(parser: P, func: F) -> impl Parser<'a, I, B>
    where
        P: Parser<'a, I, A>,
        F: Fn(Option<A>) -> Option<B>,
{
    move |input|
        parser.apply(input)
            .map(|(rest, output)| (rest, func(output)))
}

fn repeat<'a, P, I, A>(parser: P) -> impl Parser<'a, I, Vec<A>>
    where
        P: Parser<'a, I, A>,
        I: Copy
{
    move |input| {
        let mut outputs = Vec::new();
        let mut rest = input;
        while let Ok((next, output)) = parser.apply(rest) {
            rest = next;
            if let Some(output) = output {
                outputs.push(output);
            }
        }
        Ok((rest, match outputs.is_empty() {
            true => None,
            false => Some(outputs)
        }))
    }
}

fn repeat1<'a, P, I, A>(parser: P) -> impl Parser<'a, I, Vec<A>>
    where
        P: Parser<'a, I, A>,
        I: Copy
{
    let parser = repeat(parser);
    move |input| {
        match parser.apply(input) {
            Ok((_, None)) => Err(input),
            Ok(output) => Ok(output),
            Err(e) => Err(e)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_doesnt_consume() {
        assert_eq!(empty("dog"), Ok(("dog", None)));
    }

    #[test]
    fn any_char_returns_any_char() {
        assert_eq!(any_char("dog"), Ok(("og", Some("d"))));
    }

    #[test]
    fn satisfy_returns_matched_char() {
        assert_eq!(satisfy(any_char, |&s| s == "d").apply("dog"),
                   Ok(("og", Some("d"))));
        assert_eq!(satisfy(any_char, |&s| s == "d").apply("cat"),
                   Err("cat"));
    }

    #[test]
    fn chars() {
        assert_eq!(ch('a').apply("abc"),
                   Ok(("bc", Some("a"))));
        assert_eq!(ch('b').apply("cba"),
                   Err("cba"));
    }

    #[test]
    fn digits() {
        assert_eq!(digit_char().apply("42"),
                   Ok(("2", Some("4"))));
        assert_eq!(digit_char().apply("dog"),
                   Err("dog"));
    }

    #[test]
    fn alphabetics() {
        assert_eq!(alphabetic_char().apply("abc"),
                   Ok(("bc", Some("a"))));
        assert_eq!(alphabetic_char().apply("123"),
                   Err("123"));
    }

    #[test]
    fn whitespaces() {
        assert_eq!(whitespace_char().apply(" bc"),
                   Ok(("bc", Some(" "))));
    }

    #[test]
    fn repeat_captures_while_satisfied() {

        // consume up to not satisfied
        assert_eq!(repeat(digit_char()).apply("123abc"),
                   Ok(("abc", Some(vec!("1","2","3")))));

        // consume full input
        assert_eq!(repeat(digit_char()).apply("123"),
                   Ok(("", Some(vec!("1","2","3")))));

        // zero is OK
        assert_eq!(repeat(digit_char()).apply("abc"),
                   Ok(("abc", None)));
    }

    #[test]
    fn repeat1_captures_while_satisfied() {

        // consume up to not satisfied
        assert_eq!(repeat1(digit_char()).apply("123abc"),
                   Ok(("abc", Some(vec!("1","2","3")))));

        // consume full input
        assert_eq!(repeat1(digit_char()).apply("123"),
                   Ok(("", Some(vec!("1","2","3")))));

        // zero is NOT OK
        assert_eq!(repeat1(digit_char()).apply("abc"),
                   Err("abc"));
    }
}