pub type ParseResult<I, O> = Result<(I, Option<O>), I>;

pub trait Parser<I, O> {
    fn apply(&self, input: I) -> ParseResult<I, O>;
}

impl<F, I, O> Parser<I, O> for F
where
    F: Fn(I) -> ParseResult<I, O>,
{
    fn apply(&self, input: I) -> ParseResult<I, O> {
        self(input)
    }
}

pub fn empty<I>(input: I) -> ParseResult<I, ()> {
    Ok((input, None))
}

pub fn optional<P, I, O>(parser: P) -> impl Parser<I, O>
where
    P: Parser<I, O>,
    I: Copy,
{
    move |input| parser.apply(input).or(Ok((input, None)))
}

pub fn discard<P, I, O>(parser: P) -> impl Parser<I, O>
where
    P: Parser<I, O>,
    I: Copy,
{
    move |input| parser.apply(input).map(|(rest, _)| (rest, None))
}

pub fn satisfy<P, F, I, O>(parser: P, f: F) -> impl Parser<I, O>
where
    F: Fn(&O) -> bool,
    P: Parser<I, O>,
    I: Copy,
{
    move |input| match parser.apply(input) {
        Ok((rest, None)) => Ok((rest, None)),
        Ok((rest, Some(output))) if f(&output) => Ok((rest, Some(output))),
        _ => Err(input),
    }
}

pub fn map<P, F, I, A, B>(parser: P, f: F) -> impl Parser<I, B>
where
    P: Parser<I, A>,
    F: Fn(Option<A>) -> Option<B>,
{
    move |input| parser.apply(input).map(|(rest, output)| (rest, f(output)))
}

pub fn mapv<P, F, I, A, B>(parser: P, f: F) -> impl Parser<I, B>
where
    P: Parser<I, A>,
    F: Fn(A) -> B,
{
    map(parser, move |output| output.map(|output| f(output)))
}

pub fn repeat<P, I, O>(parser: P) -> impl Parser<I, Vec<O>>
where
    P: Parser<I, O>,
    I: Copy,
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
        Ok((
            rest,
            match outputs.is_empty() {
                true => None,
                false => Some(outputs),
            },
        ))
    }
}

pub fn repeat1<P, I, O>(parser: P) -> impl Parser<I, Vec<O>>
where
    P: Parser<I, O>,
    I: Copy,
{
    let parser = repeat(parser);
    move |input| match parser.apply(input) {
        Ok((_, None)) => Err(input),
        Ok(output) => Ok(output),
        Err(e) => Err(e),
    }
}

fn one_of<I, O>(parsers: Vec<Box<dyn Parser<I, O>>>) -> impl Parser<I, O>
where
    I: Copy,
{
    move |input| {
        parsers
            .iter()
            .map(|p| p.apply(input))
            .find(|o| o.is_ok())
            .unwrap_or_else(|| Err(input))
    }
}

#[macro_export]
macro_rules! one_of {
    ( $( $x:expr ),* ) => {
        {
            let mut v = Vec::<Box<dyn Parser<_, _>>>::new();
            $(
                v.push(Box::new($x));
            )*
            one_of(v)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    fn any_char(input: &str) -> ParseResult<&str, &str> {
        match input.chars().next() {
            Some(c) => Ok((&input[c.len_utf8()..], Some(&input[0..c.len_utf8()]))),
            None => Err(input),
        }
    }

    fn digit_char<'a>() -> impl Parser<&'a str, &'a str> {
        satisfy(any_char, |&s| s.chars().next().unwrap().is_digit(10))
    }

    fn alphabetic_char<'a>() -> impl Parser<&'a str, &'a str> {
        satisfy(any_char, |&s| s.chars().next().unwrap().is_alphabetic())
    }

    #[test]
    fn empty_does_not_consume() {
        assert_eq!(empty("dog"), Ok(("dog", None)));
    }

    #[test]
    fn satisfy_returns_matched_char() {
        assert_eq!(
            satisfy(any_char, |&s| s == "d").apply("dog"),
            Ok(("og", Some("d")))
        );
        assert_eq!(satisfy(any_char, |&s| s == "d").apply("cat"), Err("cat"));
    }

    #[test]
    fn map_all() {
        assert_eq!(
            map(any_char, |_| Some("x")).apply("dog"),
            Ok(("og", Some("x")))
        );
    }

    #[test]
    fn repeat_captures_while_satisfied() {
        // consume up to not satisfied
        assert_eq!(
            repeat(digit_char()).apply("123abc"),
            Ok(("abc", Some(vec!("1", "2", "3"))))
        );

        // consume full input
        assert_eq!(
            repeat(digit_char()).apply("123"),
            Ok(("", Some(vec!("1", "2", "3"))))
        );

        // zero is OK
        assert_eq!(repeat(digit_char()).apply("abc"), Ok(("abc", None)));
    }

    #[test]
    fn repeat1_captures_while_satisfied() {
        // consume up to not satisfied
        assert_eq!(
            repeat1(digit_char()).apply("123abc"),
            Ok(("abc", Some(vec!("1", "2", "3"))))
        );

        // consume full input
        assert_eq!(
            repeat1(digit_char()).apply("123"),
            Ok(("", Some(vec!("1", "2", "3"))))
        );

        // zero is NOT OK
        assert_eq!(repeat1(digit_char()).apply("abc"), Err("abc"));
    }

    #[test]
    fn digit_parsing() {
        let parser = mapv(repeat1(digit_char()), |v| {
            v.iter()
                .map(|s| s.chars().next().unwrap())
                .map(|c| c as u8 - 48)
                .fold(0, |acc, e| acc * 10 + e)
        });
        assert_eq!(parser.apply("42"), Ok(("", Some(42))));
        assert_eq!(parser.apply("012"), Ok(("", Some(12))));
        assert_eq!(parser.apply("abc"), Err("abc"));
    }

    #[test]
    fn optional_maps_err_to_empty_value() {
        assert_eq!(optional(digit_char()).apply("dog"), Ok(("dog", None)));
        assert_eq!(optional(digit_char()).apply("123"), Ok(("23", Some("1"))));
    }

    #[test]
    fn discard_maps_value_to_empty_value() {
        assert_eq!(discard(digit_char()).apply("dog"), Err("dog"));
        assert_eq!(discard(digit_char()).apply("123"), Ok(("23", None)));
    }

    #[test]
    fn one_of_finds_a_successful_parser() {
        assert_eq!(
            one_of!(digit_char(), alphabetic_char()).apply("1a"),
            Ok(("a", Some("1")))
        );
        assert_eq!(
            one_of!(alphabetic_char(), digit_char()).apply("1a"),
            Ok(("a", Some("1")))
        );
        assert_eq!(one_of!(alphabetic_char()).apply("1a"), Err("1a"));
    }
}
