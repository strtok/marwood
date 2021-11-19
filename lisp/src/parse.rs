use crate::cell::Cell;
use crate::lexer::{Token, TokenType};
use std::iter::Peekable;

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {}

/// Parse one expression from the token stream.
///
/// # Arguments
/// *`cur` - An iterator over the token stream. The parser will only
///          advance the iterator enough to satisfy one expression.
/// *`text` - The text backed by the token spans.
pub fn parse<'a, T: Iterator<Item = &'a Token>>(
    text: &str,
    cur: &mut Peekable<T>,
) -> Result<Option<Cell>, ParseError> {
    let token = match cur.next() {
        Some(token) => token,
        None => return Ok(None),
    };
    let token_text = token.span_text(text);
    match token.token_type {
        TokenType::RightParen => Err(ParseError {}),
        TokenType::LeftParen => {
            let mut list = vec![];
            while let Some(&token) = cur.peek() {
                match token.token_type {
                    TokenType::RightParen => {
                        cur.next();
                        return if list.is_empty() {
                            Err(ParseError {})
                        } else {
                            Ok(Some(Cell::new_list(list)))
                        };
                    }
                    TokenType::Dot => {
                        cur.next();
                        return Ok(Some(Cell::Nil));
                    }
                    _ => match parse(text, cur) {
                        Ok(Some(cell)) => list.push(cell),
                        Ok(None) => {}
                        Err(e) => {
                            return Err(e);
                        }
                    },
                }
            }
            Err(ParseError {})
        }
        TokenType::True => Ok(Some(Cell::Bool(true))),
        TokenType::False => Ok(Some(Cell::Bool(false))),
        TokenType::Symbol => Ok(Some(Cell::new_symbol(token_text))),
        TokenType::Number => match token_text.parse::<i64>() {
            Ok(n) => Ok(Some(Cell::Number(n))),
            Err(_) => match token_text.parse::<f64>() {
                Ok(n) => Ok(Some(Cell::Number(n as i64))),
                Err(_) => Ok(Some(Cell::Symbol(token_text.to_string()))),
            },
        },
        _ => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell;
    use crate::lexer;
    use crate::list;

    macro_rules! parses {
        ($($lhs:expr => $rhs:expr),+) => {{
             $(
                assert_eq!(parse($lhs, &mut lexer::tokenize($lhs).unwrap().iter().peekable()), Ok(Some($rhs)));
             )+
        }};
    }

    macro_rules! fails {
        ($($lhs:expr),+) => {{
             $(
                assert!(matches!(parse($lhs, &mut lexer::tokenize($lhs).unwrap().iter().peekable()), Err(_)));
             )+
        }};
    }

    #[test]
    fn paren_mismatch() {
        fails!("(", ")");
    }

    #[test]
    fn variables() {
        parses!(
            "foo" => cell!["foo"],
            "bar" => cell!["bar"]
        );
    }

    #[test]
    fn consumes_one_expression_per_call() {
        let text = "foo bar baz";
        let tokens = lexer::tokenize(text).unwrap();
        let mut cur = (&tokens).iter().peekable();
        assert_eq!(parse(text, &mut cur), Ok(Some(cell!["foo"])));
        assert_eq!(parse(text, &mut cur), Ok(Some(cell!["bar"])));
        assert_eq!(parse(text, &mut cur), Ok(Some(cell!["baz"])));
        assert_eq!(parse(text, &mut cur), Ok(None));
    }

    #[test]
    fn lists_are_fully_consumed() {
        let text = "(foo bar)";
        let tokens = lexer::tokenize(text).unwrap();
        let mut cur = (&tokens).iter().peekable();
        assert_eq!(parse(text, &mut cur), Ok(Some(list!["foo", "bar"])));
        assert_eq!(parse(text, &mut cur), Ok(None));
    }

    #[test]
    fn procedures() {
        parses!(
            "(foo)" => list!["foo"],
            "( foo )" => list!["foo"],
            "(foo bar baz)" => list!["foo", "bar", "baz"]
        );

        fails!("()", "( )", "(  )");
    }

    #[test]
    fn numbers() {
        parses!(
            "42" => cell![42],
            "+42" => cell![42],
            "-42" => cell![-42],
            "42..1" => cell!["42..1"]
        );
    }

    #[test]
    fn expressions() {
        parses!(
            "foo" => cell!["foo"],
            "42" => cell![42],
            "-18" => cell![-18],
            "(foo)" => list!["foo"],
            "(foo (bar baz))" => list!["foo", list!["bar", "baz"]]
        );
    }
}
