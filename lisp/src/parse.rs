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
/// *`cur` - an iterator over the token stream. The parser will only
///          advance the iterator enough to satisfy one expression.
/// *`text` - the text backed by the token spans.
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
        TokenType::Symbol => Ok(Cell::new_symbol(token.span(text))),
        TokenType::Number => parse_number(text, token),
        TokenType::Dot | TokenType::WhiteSpace => {
            Err(Error::UnexpectedToken(token.span(text).into()))
        }
    }
}

/// Parse List
///
/// This function is called by a parser that's encountered a '('.
/// It will recursively parse every value in the list until
/// it encounters a ')' or a '.', the later of which calls
/// parse_improper_list_tail() to finish parsing the dotted
/// form of a list.
///
/// # Arguments
/// *`cur` - an iterator over the token stream. The parser will only
///          advance the iterator enough to satisfy one expression.
/// *`text` - the text backed by the token spans.
fn parse_list<'a, T: Iterator<Item = &'a Token>>(
    text: &str,
    cur: &mut Peekable<T>,
) -> Result<Cell, Error> {
    let mut list = vec![];
    loop {
        match cur.peek().ok_or(Error::Eof)?.token_type {
            TokenType::RightParen => {
                cur.next();
                return Ok(Cell::new_list(list));
            }
            TokenType::Dot => {
                cur.next();
                return parse_improper_list_tail(list, text, cur);
            }
            _ => {
                list.push(parse(text, cur)?);
            }
        }
    }
}

/// Parse Improper List Tail
///
/// This function is called by a parser that has encounted a '.'
/// when parsing a list.
///
/// # Arguments
/// `list` - the constructed list up to the .
/// `text` - the text backed by the token spans.
/// `cur` - a cursor pointing to the position in the token stream
///         immediately after the encountered '.'
fn parse_improper_list_tail<'a, T: Iterator<Item = &'a Token>>(
    list: Vec<Cell>,
    text: &str,
    cur: &mut Peekable<T>,
) -> Result<Cell, Error> {
    // At least one value must be read before the dot
    if list.is_empty() {
        return Err(Error::ExpectedTokenBeforeDot);
    }

    // Read the next value and add it to the list
    let last_cdr = match cur.peek().ok_or(Error::Eof)?.token_type {
        TokenType::Dot | TokenType::RightParen => {
            return Err(Error::ExpectedOneTokenAfterDot);
        }
        _ => parse(text, cur)?,
    };

    // The next token must be a )
    match cur.next().ok_or(Error::Eof)?.token_type {
        TokenType::RightParen => Ok(Cell::new_improper_list(list, last_cdr)),
        _ => Err(Error::ExpectedOneTokenAfterDot),
    }
}

/// Parse Number
///
/// This function is called by a parser that has encountered a number
/// token.
///
/// If this function is unable to parse the number it is treated as
/// as symbol.
///
/// # Arguments
/// *`cur` - an iterator over the token stream. The parser will only
///          advance the iterator enough to satisfy one expression.
/// *`text` - the text backed by the token spans.
fn parse_number(text: &str, token: &Token) -> Result<Cell, Error> {
    let span = token.span(text);
    match span.parse::<i64>() {
        Ok(n) => Ok(Cell::Number(n)),
        Err(_) => match span.parse::<f64>() {
            Ok(n) => Ok(Cell::Number(n as i64)),
            Err(_) => Ok(Cell::Symbol(span.into())),
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
                assert_eq!(Ok($rhs), parse($lhs, &mut lex::scan($lhs).unwrap().iter().peekable()));
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
        fails!["(.)", "(. 0)", "(0 .)", "(0 .)", "(0 1 .)", "(1 . 2 . 3)"];
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
