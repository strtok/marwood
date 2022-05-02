use crate::cell::Cell;
use crate::char::named_to_char;
use crate::lex::TokenType::NumberPrefix;
use crate::lex::{Token, TokenType};
use crate::number::{Exactness, Number};
use crate::parse::Error::{
    ExpectedListTerminator, ExpectedVectorTerminator, Incomplete, UnexpectedToken, UnknownChar,
};
use crate::{lex, list};
use std::iter::Peekable;

#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error("incomplete")]
    Incomplete,
    #[error("unexpected token '{0}'")]
    UnexpectedToken(String),
    #[error("unexpected one token after .")]
    ExpectedOneTokenAfterDot,
    #[error("expected at least one token before .")]
    ExpectedTokenBeforeDot,
    #[error("expected list terminator {0}, but encountered {1}")]
    ExpectedListTerminator(char, char),
    #[error("expected vector terminator ), but encountered {0}")]
    ExpectedVectorTerminator(char),
    #[error("syntax error: {0}")]
    SyntaxError(String),
    #[error("unknown character {0}")]
    UnknownChar(String),
    #[error(transparent)]
    LexError(#[from] lex::Error),
}

/// Parse text
///
/// Tokenize and parse one expression, returning the resulting Cell
/// and any remaining text, or Error if an error occurred.
///
/// # Arguments
/// *`text` - the text to parse
pub fn parse_text(text: &str) -> Result<(Cell, Option<&str>), Error> {
    let tokens = lex::scan(text)?;
    let mut cur = tokens.iter().peekable();
    let cell = parse(text, &mut cur)?;

    let remaining_text = cur.peek().map(|Token { span, .. }| &text[span.0..]);

    Ok((cell, remaining_text))
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
        None => return Err(Error::Incomplete),
    };
    match token.token_type {
        TokenType::SingleQuote => Ok(list!["quote", parse(text, cur)?]),
        TokenType::RightParen => Err(Error::UnexpectedToken(")".into())),
        TokenType::LeftParen => parse_list(text, cur, token),
        TokenType::HashParen => parse_vector(text, cur),
        TokenType::True => Ok(Cell::Bool(true)),
        TokenType::False => Ok(Cell::Bool(false)),
        TokenType::Char => parse_char(text, token),
        TokenType::String => parse_string(match token.span(text) {
            "\"\"" => "",
            span => &span[1..span.len() - 1],
        }),
        TokenType::Symbol => Ok(Cell::new_symbol(token.span(text))),
        TokenType::NumberPrefix | TokenType::Number => parse_number(text, cur, token),
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
/// * `start_token` - The start of list token, used to match the end of
///     list token.
fn parse_list<'a, T: Iterator<Item = &'a Token>>(
    text: &str,
    cur: &mut Peekable<T>,
    start_token: &Token,
) -> Result<Cell, Error> {
    let mut list = vec![];
    loop {
        match cur.peek().ok_or(Error::Incomplete)?.token_type {
            TokenType::RightParen => {
                let start_token = start_token.span(text).chars().next().unwrap();
                let end_token = cur.next().unwrap().span(text).chars().next().unwrap();
                if !match start_token {
                    '(' => end_token == ')',
                    '[' => end_token == ']',
                    '{' => end_token == '}',
                    _ => false,
                } {
                    return Err(ExpectedListTerminator(start_token, end_token));
                }
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

    // Exactly one value must be parsed after the dot
    let last_cdr = match cur.peek().ok_or(Error::Incomplete)?.token_type {
        TokenType::Dot | TokenType::RightParen => Err(Error::ExpectedOneTokenAfterDot),
        _ => Ok(parse(text, cur)?),
    }?;

    // The next token must be a ')'
    match cur.next().ok_or(Error::Incomplete)?.token_type {
        TokenType::RightParen => Ok(Cell::new_improper_list(list, last_cdr)),
        _ => Err(Error::ExpectedOneTokenAfterDot),
    }
}

/// Vector
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
/// * `start_token` - The start of list token, used to match the end of
///     list token.
fn parse_vector<'a, T: Iterator<Item = &'a Token>>(
    text: &str,
    cur: &mut Peekable<T>,
) -> Result<Cell, Error> {
    let mut vector = vec![];
    loop {
        match cur.peek().ok_or(Error::Incomplete)?.token_type {
            TokenType::RightParen => {
                let end_token = cur.next().unwrap().span(text).chars().next().unwrap();
                if end_token != ')' {
                    return Err(ExpectedVectorTerminator(end_token));
                }
                return Ok(Cell::Vector(vector));
            }
            TokenType::Dot => {
                return Err(UnexpectedToken(".".into()));
            }
            _ => {
                vector.push(parse(text, cur)?);
            }
        }
    }
}

/// Parse Char
///
/// Parse the character token, skipping the character prefix
/// #\. If only one character is present after the prefix then
/// this is a literal character. If not, then treat this as a
/// "named" character of which only 'space' and 'newline' are
/// recognized.
fn parse_char(text: &str, token: &Token) -> Result<Cell, Error> {
    let span = token.span(text);
    let span = &span[2..span.len()];
    if span.chars().count() == 1 {
        Ok(Cell::Char(span.chars().next().unwrap()))
    } else if span.starts_with('x') && span[1..span.len()].chars().all(|it| it.is_ascii_hexdigit())
    {
        let c =
            u32::from_str_radix(&span[1..span.len()], 16).map_err(|_| UnknownChar(span.into()))?;
        let c = char::from_u32(c).ok_or_else(|| UnknownChar(span.into()))?;
        Ok(Cell::Char(c))
    } else {
        named_to_char(span)
            .map(Cell::Char)
            .ok_or_else(|| Error::UnknownChar(span.into()))
    }
}

/// Parse String
///
/// Parse the literal string, processing any espace (e.g. \").
pub fn parse_string(span: &str) -> Result<Cell, Error> {
    let mut cur = span.chars().peekable();
    let mut output = String::new();
    while let Some(c) = cur.next() {
        if c == '\\' {
            output.push(match cur.peek() {
                Some('\\') => '\\',
                Some('a') => char::from_u32(0x7).unwrap(),
                Some('b') => char::from_u32(0x8).unwrap(),
                Some('e') => char::from_u32(0x1b).unwrap(),
                Some('t') => '\t',
                Some('n') => '\n',
                Some('r') => '\r',
                Some('v') => char::from_u32(0xb).unwrap(),
                Some('f') => char::from_u32(0xc).unwrap(),
                Some('x') => {
                    cur.next();
                    let mut unicode_value = 0_u32;
                    loop {
                        match cur.peek() {
                            Some(';') => break,
                            Some(c) if c.is_ascii_hexdigit() => {
                                unicode_value = unicode_value.checked_mul(16).ok_or_else(|| {
                                    Error::SyntaxError("overflow when parsing \\x".into())
                                })?;
                                unicode_value = unicode_value
                                    .checked_add(c.to_digit(16).unwrap())
                                    .ok_or_else(|| {
                                        Error::SyntaxError("overflow when parsing \\x".into())
                                    })?;
                            }
                            Some(c) => {
                                return Err(Error::SyntaxError(format!(
                                    "\\x expected hex digits but encountered '{}' in \"{}\"",
                                    c, span
                                )))
                            }
                            None => {
                                return Err(Error::SyntaxError(format!(
                                    "\\x must be terminated with ; in \"{}\"",
                                    span
                                )))
                            }
                        }
                        cur.next();
                    }
                    char::from_u32(unicode_value).ok_or_else(|| {
                        Error::SyntaxError(format!("\\#x{:x}; is not valid unicode", unicode_value))
                    })?
                }
                Some(c) => *c,
                None => return Err(Error::Incomplete),
            });
            cur.next();
        } else {
            output.push(c);
        }
    }

    Ok(Cell::String(output))
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
fn parse_number<'a, T: Iterator<Item = &'a Token>>(
    text: &str,
    cur: &mut Peekable<T>,
    mut token: &'a Token,
) -> Result<Cell, Error> {
    let mut exactness = Exactness::Unspecified;
    let mut radix = 10;

    while token.token_type == NumberPrefix {
        match token.span(text) {
            "#e" => exactness = Exactness::Exact,
            "#i" => exactness = Exactness::Inexact,
            "#d" => radix = 10,
            "#b" => radix = 2,
            "#o" => radix = 8,
            "#x" => radix = 16,
            _ => panic!("unexpected number prefix {}", token.span(text)),
        }
        token = cur.next().ok_or(Incomplete)?;
    }

    let span = token.span(text);
    match Number::parse_with_exactness(span, exactness, radix) {
        Some(num) => Ok(Cell::Number(num)),
        None => Ok(Cell::Symbol(span.to_string())),
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
    use crate::vector;

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
        assert_eq!(parse(text, &mut cur), Err(Error::Incomplete));
    }

    #[test]
    fn lists_are_fully_consumed() {
        let text = "(foo bar)";
        let tokens = lex::scan(text).unwrap();
        let mut cur = (&tokens).iter().peekable();
        assert_eq!(parse(text, &mut cur), Ok(list!["foo", "bar"]));
        assert_eq!(parse(text, &mut cur), Err(Error::Incomplete));
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
    fn vectors() {
        parses! {
            "#()" => vector![],
            "#(1)" => vector![1],
            "#(1 2 3)" => vector![1,2,3],
            "#(#(1 2 3) #(4 5 6))" => vector![vector![1,2,3], vector![4,5,6]],
            "#('foo 'bar)" => vector![list!["quote", "foo"], list!["quote", "bar"]]
        };

        fails!["#(1 2 3}", "#(1 2 3]", "#(1 2 . 3)"];
    }

    #[test]
    fn numbers() {
        parses! {
            "42" => cell![42],
            "+42" => cell![42],
            "-42" => cell![-42],
            "42..1" => cell!["42..1"]
        };
        parses! {
            "#xff" => cell![255],
            "#b11111111" => cell![255],
            "#o377" => cell![255]
        };
        parses! {
            "4/2" => cell![2],
            "1.0" => cell![1]
        }

        parses! {
            "#x7fffffff/1" => cell![0x7fffffff],
            "#xffffffff/1" => cell![0xffffffff]
        }
    }

    #[test]
    fn characters() {
        parses! {
            "#\\c" => cell!['c'],
            "#\\space" => cell![' '],
            "#\\newline" => cell!['\n'],
            "#\\x03bb" => cell!['λ'],
            "#\\x03BB" => cell!['λ'],
            "#\\x3bb" => cell!['λ']
        }
    }

    #[test]
    fn strings() {
        parses! {
            r#""""# => Cell::new_string(""),
            r#""foo""# => Cell::new_string("foo"),
            r#""foo \"bar\" baz""# => Cell::new_string("foo \"bar\" baz"),
            r#""foo \\ baz""# => Cell::new_string("foo \\ baz"),
            r#""\t""# => Cell::new_string("\t"),
            r#""\n""# => Cell::new_string("\n"),
            r#""\a""# => Cell::new_string(&String::from(char::from_u32(0x7).unwrap())),
            r#""\a""# => Cell::new_string(&String::from(char::from_u32(0x7).unwrap())),
            r#""\b""# => Cell::new_string(&String::from(char::from_u32(0x8).unwrap())),
            r#""\e""# => Cell::new_string(&String::from(char::from_u32(0x1b).unwrap())),
            r#""\x41;""# => Cell::new_string("A"),
            r#""\x1f436;""# => Cell::new_string("🐶")
        };

        fails![r#""\xffffffff;""#, r#""\x41""#, r#""\xzz;""#];
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

    #[test]
    fn alt_paren_chars() {
        parses! {
            "[1 2 3]" => list![1, 2, 3],
            "{1 2 3}" => list![1, 2, 3]
        };

        fails!["'(1 2 3]", "'(4 5 6}", "'{1 2 3]", "'[1 2 3}"];
    }
}
