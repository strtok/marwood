use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    Dot,
    False,
    LeftParen,
    Number,
    RightParen,
    SingleQuote,
    Symbol,
    True,
    WhiteSpace,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub span: (usize, usize),
    pub token_type: TokenType,
}

impl Token {
    pub fn new(span: (usize, usize), token_type: TokenType) -> Token {
        Token { span, token_type }
    }

    pub fn span_text<'a, 'b>(&'a self, text: &'b str) -> &'b str {
        &text[self.span.0..self.span.1]
    }
}

#[derive(Debug)]
pub struct Error {
    pub pos: usize,
    pub error_type: ErrorType,
}

impl Error {
    fn new(pos: usize, error_type: ErrorType) -> Error {
        Error { pos, error_type }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ErrorType {
    #[error("vectors are not supported")]
    VectorsNotSupported,
    #[error("unexpected token '{0}'")]
    UnexpectedToken(char),
}

pub fn tokenize(text: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = vec![];
    let mut cur = text.char_indices().peekable();

    while let Some(&(start, c)) = cur.peek() {
        tokens.push(match c {
            '(' | ')' | '\'' | '.' => {
                cur.next();
                let token_type = match c {
                    '(' => TokenType::LeftParen,
                    ')' => TokenType::RightParen,
                    '\'' => TokenType::SingleQuote,
                    '.' => TokenType::Dot,
                    _ => {
                        continue;
                    }
                };
                Token::new((start, start + c.len_utf8()), token_type)
            }
            '#' => {
                cur.next();
                match cur.next() {
                    Some((_, 't')) => Token::new((start, start + 2), TokenType::True),
                    Some((_, 'f')) => Token::new((start, start + 2), TokenType::False),
                    Some((_, '(')) => {
                        return Err(Error::new(start, ErrorType::VectorsNotSupported));
                    }
                    _ => {
                        return Err(Error::new(start, ErrorType::UnexpectedToken('#')));
                    }
                }
            }
            _ if is_initial_identifier(c) => scan_symbol(&mut cur)?,
            _ if is_initial_number(c) => scan_number(&mut cur)?,
            _ if c.is_whitespace() => {
                cur.next();
                continue;
            }
            _ => return Err(Error::new(start, ErrorType::UnexpectedToken(c))),
        });
    }

    Ok(tokens)
}

fn scan_symbol(cur: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    let start = cur.peek().unwrap().0;
    let mut end = start;
    while let Some(&(offset, c)) = cur.peek() {
        if !is_subsequent_identifier(c) && start != end {
            break;
        }
        end = offset + c.len_utf8();
        cur.next();
    }
    Ok(Token::new((start, end), TokenType::Symbol))
}

fn scan_number(cur: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    let start = cur.peek().unwrap().0;
    let mut end = start;
    while let Some(&(offset, c)) = cur.peek() {
        if !is_subsequent_number(c) && start != end {
            break;
        }
        end = offset + c.len_utf8();
        cur.next();
    }
    Ok(Token::new((start, end), TokenType::Number))
}

fn is_initial_number(c: char) -> bool {
    c.is_digit(10) || c == '+' || c == '-'
}

fn is_subsequent_number(c: char) -> bool {
    c.is_digit(10) || c == '.'
}

fn is_initial_identifier(c: char) -> bool {
    c.is_alphabetic()
        || c == '!'
        || c == '$'
        || c == '%'
        || c == '&'
        || c == '*'
        || c == '/'
        || c == ':'
        || c == '<'
        || c == '='
        || c == '>'
        || c == '?'
        || c == '^'
        || c == '_'
        || c == '~'
}

fn is_special_subsequent(c: char) -> bool {
    c == '+' || c == '-' || c == '.' || c == '@'
}

fn is_subsequent_identifier(c: char) -> bool {
    is_initial_identifier(c) || c.is_digit(10) || is_special_subsequent(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Map tokens to a vector (text, type) pairs.
    fn expand<T: IntoIterator<Item = Token>>(
        tokens: T,
        original_text: &str,
    ) -> Vec<(&str, TokenType)> {
        tokens
            .into_iter()
            .map(|token| (token.span_text(original_text), token.token_type.clone()))
            .collect()
    }

    macro_rules! lexes {
        ($lhs:expr => $(($token_text:expr, $token_type:expr)),+) => {{
            let mut v = vec![];
            $(v.push(($token_text, $token_type));)+
            assert_eq!(expand(tokenize($lhs).unwrap(), $lhs), v);
        }};
        ($($lhs:expr => $rhs:expr),+) => {{
             $(
                assert_eq!(expand(tokenize($lhs).unwrap(), $lhs).iter().next().unwrap(), &($lhs, $rhs));
             )+
        }};
    }

    #[test]
    fn parens() {
        lexes!(
            "(" => TokenType::LeftParen,
            ")" => TokenType::RightParen
        );
    }

    #[test]
    fn symbols() {
        lexes!(
            "foo" => TokenType::Symbol
        );
    }

    #[test]
    fn numbers() {
        lexes!(
            "5" => TokenType::Number,
            "10" => TokenType::Number,
            "10.5" => TokenType::Number,
            "10..5" => TokenType::Number,
            "-42" => TokenType::Number,
            "+42" => TokenType::Number,
            "-10.5" => TokenType::Number
        )
    }

    #[test]
    fn dot() {
        lexes!(
            "(foo . bar)" =>
            ("(", TokenType::LeftParen),
            ("foo", TokenType::Symbol),
            (".", TokenType::Dot),
            ("bar", TokenType::Symbol),
            (")", TokenType::RightParen)
        );
    }

    #[test]
    fn multiple_expressions() {
        lexes!(
            "42.0 '((1024)('baz))" =>
            ("42.0", TokenType::Number),
            ("'", TokenType::SingleQuote),
            ("(", TokenType::LeftParen),
            ("(", TokenType::LeftParen),
            ("1024", TokenType::Number),
            (")", TokenType::RightParen),
            ("(", TokenType::LeftParen),
            ("'", TokenType::SingleQuote),
            ("baz", TokenType::Symbol),
            (")", TokenType::RightParen),
            (")", TokenType::RightParen)
        );
    }

    #[test]
    fn boolean_tokens() {
        lexes!(
            "#t" => TokenType::True,
            "#f" => TokenType::False
        )
    }
}
