use std::fmt::Debug;

pub type ParseResult<'a, I, O> = Result<(I, Option<O>), I>;

pub trait Parser<'a, I, O> {
    fn apply(&self, input: I) -> ParseResult<'a, I, O>;
}

impl<'a, F, I, O> Parser<'a, I, O> for F
    where F: Fn(I) -> ParseResult<'a, I, O>,
{
    fn apply(&self, input: I) -> ParseResult<'a, I, O> {
        self(input)
    }
}

pub fn empty<'a, I>(input: I) -> ParseResult<'a, I, ()> {
    Ok((input, None))
}

pub fn satisfy<'a, P, F, I, O>(parser: P, f: F) -> impl Parser<'a, I, O>
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


//
// Combinators
//

pub fn map<'a, P, F, I, A, B>(parser: P, f: F) -> impl Parser<'a, I, B>
    where
        P: Parser<'a, I, A>,
        F: Fn(Option<A>) -> Option<B>,
{
    move |input|
        parser.apply(input)
            .map(|(rest, output)| (rest, f(output)))
}

pub fn mapv<'a, P, F, I, A, B>(parser: P, f: F) -> impl Parser<'a, I, B>
    where
        P: Parser<'a, I, A>,
        F: Fn(A) -> B,
{
    map(parser, move |output| {
        match output {
            None => None,
            Some(output) => Some(f(output))
        }
    })
}

pub fn repeat<'a, P, I, A>(parser: P) -> impl Parser<'a, I, Vec<A>>
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

pub fn repeat1<'a, P, I, A>(parser: P) -> impl Parser<'a, I, Vec<A>>
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

    fn any_char<'a>(input: &str) -> ParseResult<&str, &str> {
        match input.chars().next() {
            Some(c) => Ok((&input[c.len_utf8()..], Some(&input[0..c.len_utf8()]))),
            None => Err(input)
        }
    }

    fn digit_char<'a>() -> impl Parser<'a, &'a str, &'a str> {
        satisfy(any_char, |&s| s.chars().next().unwrap().is_digit(10))
    }

    #[test]
    fn empty_doesnt_consume() {
        assert_eq!(empty("dog"), Ok(("dog", None)));
    }

    #[test]
    fn satisfy_returns_matched_char() {
        assert_eq!(satisfy(any_char, |&s| s == "d").apply("dog"),
                   Ok(("og", Some("d"))));
        assert_eq!(satisfy(any_char, |&s| s == "d").apply("cat"),
                   Err("cat"));
    }

    #[test]
    fn map_all() {
        assert_eq!(map(any_char, |input| Some("x")).apply("dog"),
                   Ok(("og", Some("x"))));
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

    #[test]
    fn digit_parsing() {
        let parser =
            mapv( repeat1(digit_char()), |v| {
                v.iter()
                    .map(|s| s.chars().next().unwrap())
                    .map(|c| c as u8 - 48)
                    .fold(0, |acc, e| acc * 10 + e)
            });
        assert_eq!(parser.apply("42"), Ok(("", Some(42))));
        assert_eq!(parser.apply("012"), Ok(("", Some(12))));
        assert_eq!(parser.apply("abc"), Err("abc"));
    }
}