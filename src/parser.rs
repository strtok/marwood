type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn apply(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
    where F: Fn(&'a str) -> ParseResult<Output>,
{
    fn apply(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn any<'a>(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[1..], c)),
        None => Err(input)
    }
}

fn empty(input: &str) -> ParseResult<()> {
    Ok((input, ()))
}

fn satisfy<'a, P, F, A>(parser: P, f: F) -> impl Parser<'a, A>
    where
        F: Fn(&A) -> bool,
        P: Parser<'a, A>
{
    move |input: &'a str| {
        match parser.apply(input) {
            Ok((rest, output)) if f(&output) => Ok((rest, output)),
            _ => Err(input)
        }
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        None => Err(input),
    }
}

fn ch<'a>(expected: char) -> impl Parser<'a, char> {
    satisfy(any_char, move |&c| c == expected)
}

fn digit_char<'a>() -> impl Parser<'a, char> {
    satisfy(any_char, |&c| c.is_digit(10))
}

fn alphabetic_char<'a>() -> impl Parser<'a, char> {
    satisfy(any_char,  |&c| c.is_alphabetic())
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    satisfy(any_char,  |&c| c.is_whitespace())
}

// Combinators

fn map<'a, P, F, A, B>(parser: P, func: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        F: Fn(A) -> B,
{
    move |input|
        parser.apply(input)
            .map(|(rest, output)| (rest, func(output)))
}

fn repeat<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
        P: Parser<'a, A>
{
    move |input| {
        let mut outputs = Vec::new();
        let mut rest = input;
        while let Ok((next, output)) = parser.apply(rest) {
            rest = next;
            outputs.push(output);
        }
        Ok((rest, outputs))
    }
}

fn repeat1<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
        P: Parser<'a, A>
{
    let parser = repeat(parser);
    move |input| {
        match parser.apply(input) {
            Ok((_, output)) if output.is_empty() => Err(input),
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
        assert_eq!(empty("dog"), Ok(("dog", ())));
    }

    #[test]
    fn any_returns_any_token() {
        assert_eq!(any("dog"), Ok(("og", 'd')));
    }

    #[test]
    fn satisfy_returns_matched_char() {
        assert_eq!(satisfy(any_char, |&c| c == 'd').apply("dog"),
                   Ok(("og", 'd')));
        assert_eq!(satisfy(any_char, |&c| c == 'o').apply("dog"),
                   Err("dog"));
    }

    #[test]
    fn chars() {
        assert_eq!(ch('a').apply("abc"),
                   Ok(("bc", 'a')));
        assert_eq!(ch('a').apply("cba"),
                   Err("cba"));
    }

    #[test]
    fn digits() {
        assert_eq!(digit_char().apply("42"),
                   Ok(("2", '4')));
        assert_eq!(digit_char().apply("dog"),
                   Err("dog"));
    }

    #[test]
    fn alphabetics() {
        assert_eq!(alphabetic_char().apply("abc"),
                   Ok(("bc", 'a')));
        assert_eq!(alphabetic_char().apply("123"),
                   Err("123"));
    }

    #[test]
    fn whitespaces() {
        assert_eq!(whitespace_char().apply(" bc"),
                   Ok(("bc", ' ')));
    }

    #[test]
    fn repeat_captures_while_satisfied() {

        // consume up to not satisfied
        assert_eq!(repeat(digit_char()).apply("123abc"),
                   Ok(("abc", vec!('1','2','3'))));

        // consume full input
        assert_eq!(repeat(digit_char()).apply("123"),
                   Ok(("", vec!('1','2','3'))));

        // zero is OK
        assert_eq!(repeat(digit_char()).apply("abc"),
                   Ok(("abc", vec!())));
    }

    #[test]
    fn repeat1_captures_while_satisfied() {

        // consume up to not satisfied
        assert_eq!(repeat1(digit_char()).apply("123abc"),
                   Ok(("abc", vec!('1','2','3'))));

        // consume full input
        assert_eq!(repeat1(digit_char()).apply("123"),
                   Ok(("", vec!('1','2','3'))));

        // zero is NOT OK
        assert_eq!(repeat1(digit_char()).apply("abc"),
                   Err("abc"));
    }

}