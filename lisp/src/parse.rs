use crate::cell::Cell;
use crate::lex::{Token, TokenType};
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
    match token.token_type {
        TokenType::RightParen => Err(ParseError {}),
        TokenType::LeftParen => parse_list(text, cur),
        TokenType::True => Ok(Some(Cell::Bool(true))),
        TokenType::False => Ok(Some(Cell::Bool(false))),
        TokenType::Symbol => Ok(Some(Cell::new_symbol(token.lexeme(text)))),
        TokenType::Number => parse_number(text, token),
        _ => Ok(None),
    }
}

pub fn parse_list<'a, T: Iterator<Item = &'a Token>>(
    text: &str,
    cur: &mut Peekable<T>,
) -> Result<Option<Cell>, ParseError> {
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
                        Ok(Some(Cell::new_improper_list(list, last_cdr)))
                    }
                    Some(false) => Err(ParseError {}),
                    None => Ok(Some(Cell::new_list(list))),
                };
            }
            TokenType::Dot => {
                if list.is_empty() {
                    return Err(ParseError {});
                }
                cur.next();
                dotted_form = Some(false);
            }
            _ => {
                match dotted_form {
                    Some(true) => return Err(ParseError {}),
                    Some(ref mut val) => *val = true,
                    None => {}
                };
                match parse(text, cur) {
                    Ok(Some(cell)) => list.push(cell),
                    Ok(None) => {}
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
        }
    }
    Err(ParseError {})
}

pub fn parse_number(text: &str, token: &Token) -> Result<Option<Cell>, ParseError> {
    let lexeme = token.lexeme(text);
    match lexeme.parse::<i64>() {
        Ok(n) => Ok(Some(Cell::Number(n))),
        Err(_) => match lexeme.parse::<f64>() {
            Ok(n) => Ok(Some(Cell::Number(n as i64))),
            Err(_) => Ok(Some(Cell::Symbol(lexeme.to_string()))),
        },
    }
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
                assert_eq!(parse($lhs, &mut lex::scan($lhs).unwrap().iter().peekable()), Ok(Some($rhs)));
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
        let tokens = lex::scan(text).unwrap();
        let mut cur = (&tokens).iter().peekable();
        assert_eq!(parse(text, &mut cur), Ok(Some(cell!["foo"])));
        assert_eq!(parse(text, &mut cur), Ok(Some(cell!["bar"])));
        assert_eq!(parse(text, &mut cur), Ok(Some(cell!["baz"])));
        assert_eq!(parse(text, &mut cur), Ok(None));
    }

    #[test]
    fn lists_are_fully_consumed() {
        let text = "(foo bar)";
        let tokens = lex::scan(text).unwrap();
        let mut cur = (&tokens).iter().peekable();
        assert_eq!(parse(text, &mut cur), Ok(Some(list!["foo", "bar"])));
        assert_eq!(parse(text, &mut cur), Ok(None));
    }

    #[test]
    fn procedures() {
        parses!(
            "(foo)" => list!["foo"],
            "( foo )" => list!["foo"],
            "(foo bar baz)" => list!["foo", "bar", "baz"],
            "()" => cell![],
            "( )" => cell![]
        );
    }

    #[test]
    fn dotted_form() {
        parses!(
            "(0 . 2)" => cons![cell![0], cell![2]],
            "(0 1 . 2)" => cons![cell![0], cons![cell![1], cell![2]]]
        );
        fails!("(0 .)", "(0 1 .)");
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
