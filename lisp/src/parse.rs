use crate::cell::Cell;
use crate::lex::{Token, TokenType};
use crate::list;
use std::iter::Peekable;

#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error("unexpected EOF")]
    Eof,
    #[error("unexpected token '{0}'")]
    UnexpectedToken(String),
    #[error("unexpected one token after .")]
    ExpectedOneTokenAfterDot,
    #[error("expected at least one token before .")]
    ExpectedTokenBeforeDot,
}

/// Parse one expression from the token stream.
///
/// # Arguments
/// *`cur` - An iterator over the token stream. The parser will only
///          advance the iterator enough to satisfy one expression.
/// *`text` - The text backed by the token spans.
pub fn parse<'a, T: Iterator<Item = &'a Token>>(
    text: &str,
    cur: &mut Peekable<T>,
) -> Result<Cell, Error> {
    let token = match cur.next() {
        Some(token) => token,
        None => return Err(Error::Eof),
    };
    match token.token_type {
        TokenType::SingleQuote => Ok(list!["quote", parse(text, cur)?]),
        TokenType::RightParen => Err(Error::UnexpectedToken(")".into())),
        TokenType::LeftParen => parse_list(text, cur),
        TokenType::True => Ok(Cell::Bool(true)),
        TokenType::False => Ok(Cell::Bool(false)),
        TokenType::Symbol => Ok(Cell::new_symbol(token.lexeme(text))),
        TokenType::Number => parse_number(text, token),
        TokenType::Dot | TokenType::WhiteSpace => {
            Err(Error::UnexpectedToken(token.lexeme(text).into()))
        }
    }
}

fn parse_list<'a, T: Iterator<Item = &'a Token>>(
    text: &str,
    cur: &mut Peekable<T>,
) -> Result<Cell, Error> {
    // If the parser has encountered a dot, then
    // dotted_form is set Some(false). If it has
    // encountered one value after the dot then the
    // value is set to Some(trueO
    let mut dotted_form: Option<bool> = None;

    let mut list = vec![];
    while let Some(&token) = cur.peek() {
        match token.token_type {
            TokenType::RightParen => {
                cur.next();
                return match dotted_form {
                    Some(true) => {
                        let last_cdr = list.pop().unwrap();
                        Ok(Cell::new_improper_list(list, last_cdr))
                    }
                    Some(false) => Err(Error::ExpectedOneTokenAfterDot),
                    None => Ok(Cell::new_list(list)),
                };
            }
            TokenType::Dot => {
                if list.is_empty() {
                    return Err(Error::ExpectedTokenBeforeDot);
                }
                cur.next();
                dotted_form = Some(false);
            }
            _ => {
                match dotted_form {
                    Some(true) => return Err(Error::ExpectedOneTokenAfterDot),
                    Some(ref mut val) => *val = true,
                    None => {}
                };
                match parse(text, cur) {
                    Ok(cell) => list.push(cell),
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
        }
    }

    Err(Error::Eof)
}

fn parse_number(text: &str, token: &Token) -> Result<Cell, Error> {
    let lexeme = token.lexeme(text);
    match lexeme.parse::<i64>() {
        Ok(n) => Ok(Cell::Number(n)),
        Err(_) => match lexeme.parse::<f64>() {
            Ok(n) => Ok(Cell::Number(n as i64)),
            Err(_) => Ok(Cell::Symbol(lexeme.into())),
        },
    }
}

/// Parse Macro
///
/// Given a single expression, tokenize and parse the
/// expression into a Cell.
///
/// This macro assumes the input is a single valid expression and
/// will panic!() if it encounters lex or parse errors.
///
/// # Arguments
/// `lhs` - The expression to parse
#[macro_export]
macro_rules! parse {
    ($lhs:expr) => {{
        let tokens = lex::scan($lhs).expect("lex failed");
        let mut cur = tokens.iter().peekable();
        parse::parse($lhs, &mut cur).expect("parse failed")
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell;
    use crate::cons;
    use crate::lex;
    use crate::list;

    macro_rules! parses {
        ($($lhs:expr => $rhs:expr),+) => {{
             $(
                assert_eq!(parse($lhs, &mut lex::scan($lhs).unwrap().iter().peekable()), Ok($rhs));
             )+
        }};
    }

    macro_rules! fails {
        ($($lhs:expr),+) => {{
             $(
                assert!(matches!(parse($lhs, &mut lex::scan($lhs).unwrap().iter().peekable()), Err(_)));
             )+
        }};
    }

    #[test]
    fn paren_mismatch() {
        fails!["(", ")"];
    }

    #[test]
    fn quote_sugar() {
        parses! {
            "'1" => list!["quote", 1],
            "'(1 2)" => list!["quote", list![1, 2]]
        };
    }

    #[test]
    fn variables() {
        parses! {
            "foo" => cell!["foo"],
            "bar" => cell!["bar"]
        };
    }

    #[test]
    fn consumes_one_expression_per_call() {
        let text = "foo bar baz";
        let tokens = lex::scan(text).unwrap();
        let mut cur = (&tokens).iter().peekable();
        assert_eq!(parse(text, &mut cur), Ok(cell!["foo"]));
        assert_eq!(parse(text, &mut cur), Ok(cell!["bar"]));
        assert_eq!(parse(text, &mut cur), Ok(cell!["baz"]));
        assert_eq!(parse(text, &mut cur), Err(Error::Eof));
    }

    #[test]
    fn lists_are_fully_consumed() {
        let text = "(foo bar)";
        let tokens = lex::scan(text).unwrap();
        let mut cur = (&tokens).iter().peekable();
        assert_eq!(parse(text, &mut cur), Ok(list!["foo", "bar"]));
        assert_eq!(parse(text, &mut cur), Err(Error::Eof));
    }

    #[test]
    fn procedures() {
        parses! {
            "(foo)" => list!["foo"],
            "( foo )" => list!["foo"],
            "(foo bar baz)" => list!["foo", "bar", "baz"],
            "()" => cell![],
            "( )" => cell![]
        };
    }

    #[test]
    fn dotted_form() {
        parses! {
            "(0 . 2)" => cons![0, 2],
            "(0 1 . 2)" => cons![0, cons![1, 2]],
            "(1 2 . (3 4))" => list![1, 2, 3, 4],
            "((1 . 2) . 3)" => cons![cons![1, 2], 3],
            "((().()).())" => cons![cons![cell![], cell![]], cell![]]
        };
        fails!["(.)", "(. 0)", "(0 .)", "(0 .)", "(0 1 .)"];
    }

    #[test]
    fn numbers() {
        parses! {
            "42" => cell![42],
            "+42" => cell![42],
            "-42" => cell![-42],
            "42..1" => cell!["42..1"]
        };
    }

    #[test]
    fn expressions() {
        parses! {
            "foo" => cell!["foo"],
            "42" => cell![42],
            "-18" => cell![-18],
            "(foo)" => list!["foo"],
            "(foo (bar baz))" => list!["foo", list!["bar", "baz"]]
        };
    }
}
