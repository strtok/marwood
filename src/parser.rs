use nom::InputTake;

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
            Some(ch) => {
                if f(ch) {
                    Ok((&input[1..], ch))
                } else {
                    Err(input)
                }
            },
            None => Err(input)
        }
    }
}

fn empty(input: &str) -> ParseResult<()> {
    Ok((input, ()))
}

fn any<'a>(input: &str) -> ParseResult<char> {
    match input.len() {
        0 => Err(input),
        _ => Ok((&input[1..], input.chars().next().unwrap()))
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
    fn satisfy_returns_ch() {
        assert_eq!(satisfy(|ch| ch == 'd').parse("dog"),
                   Ok(("og", 'd')));
        assert_eq!(satisfy(|ch| ch == 'o').parse("dog"),
                   Err("dog"));
    }
}