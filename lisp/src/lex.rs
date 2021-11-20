use std::iter::Peekable;
use std::str::CharIndices;

/// Token Type
///
/// [`TokenType`] represents the type of a lexeme as recognized
/// by the scanner.
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

/// Token
/// [`Token`] is the main unit of output of the scanner, and is
/// the pairing of a lexeme with its scanned type. The `lexeme`
/// field contains a start and end index from the original scanned
/// &str and may be used to extract the lexeme from the original
/// text (e.g. `text[token.lexeme.0..token.lexeme.1]`).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    /// (start, end) index of original lexeme in the source &str
    pub lexeme: (usize, usize),
    /// The type output by the scanner
    pub token_type: TokenType,
}

impl Token {
    pub fn new(span: (usize, usize), token_type: TokenType) -> Token {
        Token {
            lexeme: span,
            token_type,
        }
    }

    /// Lexeme
    ///
    /// Given the originally scanned &str, extract the lexeme from the
    /// &str given the (start, end) indexes stored in self.lexeme.
    ///
    /// # Arguments
    /// `text` - The originally scanned &str
    ///
    /// # Safety
    /// This method assumes the originally scanned &str be used, and
    /// may panic otherwise.
    pub fn lexeme<'a, 'b>(&'a self, text: &'b str) -> &'b str {
        &text[self.lexeme.0..self.lexeme.1]
    }
}

/// Error
///
/// An error returned by the scanner if the input text contains invalid
/// grammar.
#[derive(Debug, Eq, PartialEq)]
pub struct Error {
    /// The position of the start of the invalid grammar
    pub pos: usize,
    /// The type of error encountered
    pub error_type: ErrorType,
}

impl Error {
    fn new(pos: usize, error_type: ErrorType) -> Error {
        Error { pos, error_type }
    }
}

/// Error Type
///
/// The type of error encountered by the scanner.
#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum ErrorType {
    #[error("vectors are not supported")]
    VectorsNotSupported,
    #[error("unexpected token '{0}'")]
    UnexpectedToken(char),
}

/// Scan
///
/// [`scan`] scans the provided text and returns a vector of all
/// scanned [`Token`]s.
///
/// If `scan` is provided partial input it may or may not result
/// in an error depending on the final token's grammar. For examole,
/// if the user intended to input `quote` and instead input `quot`,
/// the lexer would return a valid token stream because `quot` is a
/// valid symbol.
///
/// # Examples
///
/// ```
///     use lisp::lex;
///     use lisp::lex::Token;
///     let text = "'(1 2 3)";
///     let tokens = lex::scan(text);
/// ```
///
/// # Arguments
/// `text` - the text to return tokens for
pub fn scan(text: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = vec![];
    let mut cur = text.char_indices().peekable();

    while let Some(&(start, c)) = cur.peek() {
        tokens.push(match c {
            '(' | ')' | '\'' | '.' => scan_simple_token(&mut cur)?,
            '#' => scan_hash_token(&mut cur)?,
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

fn scan_simple_token(cur: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    let (start, c) = cur.next().unwrap();
    Ok(Token::new(
        (start, start + c.len_utf8()),
        match c {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '\'' => TokenType::SingleQuote,
            '.' => TokenType::Dot,
            _ => {
                panic!();
            }
        },
    ))
}

fn scan_hash_token(cur: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    let (start, _) = cur.next().unwrap();
    let (_, c) = cur
        .next()
        .ok_or_else(|| Error::new(start, ErrorType::UnexpectedToken('#')))?;

    match c {
        't' => Ok(Token::new((start, start + 2), TokenType::True)),
        'f' => Ok(Token::new((start, start + 2), TokenType::False)),
        '(' => Err(Error::new(start, ErrorType::VectorsNotSupported)),
        _ => Err(Error::new(start, ErrorType::UnexpectedToken('#'))),
    }
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

    // Map tokens to a vector of (lexeme, type) pairs.
    fn expand<T: IntoIterator<Item = Token>>(tokens: T, text: &str) -> Vec<(&str, TokenType)> {
        tokens
            .into_iter()
            .map(|token| (token.lexeme(text), token.token_type.clone()))
            .collect()
    }

    macro_rules! lexes {
        ($lhs:expr => $(($token_text:expr, $token_type:expr)),+) => {{
            let mut v = vec![];
            $(v.push(($token_text, $token_type));)+
            assert_eq!(expand(scan($lhs).unwrap(), $lhs), v);
        }};
        ($($lhs:expr => $rhs:expr),+) => {{
             $(
                assert_eq!(expand(scan($lhs).unwrap(), $lhs).iter().next().unwrap(), &($lhs, $rhs));
             )+
        }};
    }

    macro_rules! fails {
        ($($lhs:expr => $rhs:expr),+) => {{
             $(
             assert_eq!(scan($lhs).unwrap_err(), $rhs);
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
    fn hash_prefixed_tokens() {
        lexes!(
            "#t" => TokenType::True,
            "#f" => TokenType::False
        );

        fails!(
            "#(vector)" => Error::new(0, ErrorType::VectorsNotSupported),
            "#b" => Error::new(0, ErrorType::UnexpectedToken('#'))
        );
    }
}
