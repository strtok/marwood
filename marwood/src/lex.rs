use std::iter::Peekable;
use std::str::CharIndices;

/// Token Type
///
/// [`TokenType`] represents the type of a span as recognized
/// by the scanner.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    Char,
    Dot,
    False,
    LeftParen,
    Number,
    NumberPrefix,
    RightParen,
    SingleQuote,
    String,
    Symbol,
    True,
    WhiteSpace,
    HashParen,
}

/// Token
///
/// [`Token`] is the main unit of output of the scanner, and is
/// the pairing of a span with its scanned type. The `span`
/// field contains a start and end index from the original scanned
/// &str and may be used to extract the span from the original
/// text (e.g. `text[token.span.0..token.span.1]`).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    /// (start, end) index of original span in the source &str
    pub span: (usize, usize),
    /// The type output by the scanner
    pub token_type: TokenType,
}

impl Token {
    pub fn new(span: (usize, usize), token_type: TokenType) -> Token {
        Token { span, token_type }
    }

    /// span
    ///
    /// Given the originally scanned &str, extract the span from the
    /// &str given the (start, end) indexes stored in self.span.
    ///
    /// # Arguments
    /// `text` - The originally scanned &str
    ///
    /// # Safety
    /// This method assumes the originally scanned &str be used, and
    /// may panic otherwise.
    pub fn span<'a, 'b>(&'a self, text: &'b str) -> &'b str {
        &text[self.span.0..self.span.1]
    }
}

/// Error Type
///
/// The type of error encountered by the scanner.
#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error("unexpected EOF")]
    Eof,
    #[error("unexpected character '{0}'")]
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
///     use marwood::lex;
///     use marwood::lex::Token;
///     let text = "'(1 2 3)";
///     let tokens = lex::scan(text);
/// ```
///
/// # Arguments
/// `text` - the text to return tokens for
pub fn scan(text: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = vec![];
    let mut cur = text.char_indices().peekable();

    while let Some(&(_, c)) = cur.peek() {
        tokens.push(match c {
            '(' | ')' | '[' | ']' | '{' | '}' | '\'' => scan_simple_token(&mut cur)?,
            '#' => scan_hash_token(&mut cur)?,
            '.' => scan_dot(&mut cur)?,
            '"' => scan_string(&mut cur)?,
            _ if is_initial_identifier(c) => scan_symbol(&mut cur)?,
            _ if is_initial_number(c) => scan_number(&mut cur)?,
            _ if c.is_whitespace() => {
                cur.next();
                continue;
            }
            _ => return Err(Error::UnexpectedToken(c)),
        });
    }

    Ok(tokens)
}

/// Scan Dot
///
/// Scan dot follows these rules:
///
/// * If a dot is not immediately followed by a subsequent number or
///   identifier character, then it is a lone dot.
/// * If a dot is followed by a subsequent number then it is parsed as
///   a number. This allows numbers such as .333 without the leading 0.
/// * If a number has any subsequent ., then it is downgraded to a symbol.
/// * Anything else is a symbol.
fn scan_dot(cur: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    let start = cur.next().unwrap().0;
    let mut end = start + '.'.len_utf8();

    let mut token_type = match cur.peek() {
        Some(c) if is_subsequent_number(c.1) => TokenType::Number,
        Some(c) if is_subsequent_identifier(c.1) => TokenType::Symbol,
        _ => TokenType::Dot,
    };

    while let Some(&(offset, c)) = cur.peek() {
        if c == '.' {
            token_type = TokenType::Symbol;
        }
        let check = match token_type {
            TokenType::Symbol => is_subsequent_identifier(c),
            TokenType::Number => is_subsequent_number(c),
            _ => false,
        };
        if !check && start != end {
            break;
        }
        end = offset + c.len_utf8();
        cur.next();
    }

    Ok(Token::new((start, end), token_type))
}

fn scan_simple_token(cur: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    let (start, c) = cur.next().unwrap();
    Ok(Token::new(
        (start, start + c.len_utf8()),
        match c {
            '(' | '[' | '{' => TokenType::LeftParen,
            ')' | ']' | '}' => TokenType::RightParen,
            '\'' => TokenType::SingleQuote,
            _ => {
                panic!();
            }
        },
    ))
}

fn scan_hash_token(cur: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    let (start, _) = cur.next().unwrap();
    let (_, c) = cur.next().ok_or(Error::UnexpectedToken('#'))?;

    match c {
        't' => Ok(Token::new((start, start + 2), TokenType::True)),
        'f' => Ok(Token::new((start, start + 2), TokenType::False)),
        '(' => Ok(Token::new((start, start + 2), TokenType::HashParen)),
        'e' | 'i' | 'b' | 'o' | 'd' | 'x' => {
            Ok(Token::new((start, start + 2), TokenType::NumberPrefix))
        }
        '\\' => scan_char(cur, start),
        _ => Err(Error::UnexpectedToken('#')),
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

fn scan_string(cur: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    // Advance past opening "
    cur.next();

    let start = cur.peek().ok_or(Error::Eof)?.0;
    let mut end = start;
    let mut escape_next = false;
    let mut terminated = false;
    while let Some(&(offset, c)) = cur.peek() {
        let escaping = escape_next;
        if c == '"' && start != end && !escaping {
            terminated = true;
            break;
        }
        if c == '\\' && !escaping {
            escape_next = true;
        } else {
            escape_next = false;
        }
        end = offset + c.len_utf8();
        cur.next();
    }
    if !terminated {
        return Err(Error::Eof);
    }
    cur.next();
    Ok(Token::new((start, end), TokenType::String))
}

/// Scan Char
///
/// Scan char is called when the scanner has encountered the char prefix
/// #\. It will have called scan_char after incrementing cursor past the
/// character prefix, both start and end signifying the start and end of
/// the prefix token, where end = start+2.
///
/// In the example #\space, cur.next() should be 's' and start points to
/// the the very first character of the prefix '#'.
///
/// # Arguments
/// `cur` - The cursor, pointing at the first character of the
///   actual character.
/// `start` - The start of the token.
fn scan_char(cur: &mut Peekable<CharIndices>, start: usize) -> Result<Token, Error> {
    let mut end;
    if let Some((offset, c)) = cur.next() {
        end = offset + c.len_utf8();
        if !c.is_ascii_alphabetic() {
            return Ok(Token::new((start, end), TokenType::Char));
        }
    } else {
        return Err(Error::Eof);
    }

    while let Some(&(offset, c)) = cur.peek() {
        if !c.is_ascii_alphanumeric() {
            break;
        }
        end = offset + c.len_utf8();
        cur.next();
    }

    Ok(Token::new((start, end), TokenType::Char))
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
    c.is_digit(10) || c.is_digit(16) || c == '.' || c == '/'
}

fn is_initial_identifier(c: char) -> bool {
    c.is_alphabetic()
        || c as u32 > 0xFF
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

    // Map tokens to a vector of (span, type) pairs.
    fn expand<T: IntoIterator<Item = Token>>(tokens: T, text: &str) -> Vec<(&str, TokenType)> {
        tokens
            .into_iter()
            .map(|token| (token.span(text), token.token_type.clone()))
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
        lexes! {
            "(" => TokenType::LeftParen,
            ")" => TokenType::RightParen,
            "#(" => TokenType::HashParen
        };
    }

    #[test]
    fn symbols() {
        lexes! {
            "foo" => TokenType::Symbol
        };
    }

    #[test]
    fn chars() {
        lexes! {
            "#\\a" => TokenType::Char,
            "#\\space" => TokenType::Char,
            "#\\newline" => TokenType::Char
        };
    }

    #[test]
    fn strings() {
        assert_eq!(
            expand(scan(r#""foo bar baz""#).unwrap(), r#""foo bar baz""#),
            [("foo bar baz", TokenType::String)]
        );

        assert_eq!(
            expand(
                scan(r#""The word \"recursion\" has many meanings.""#).unwrap(),
                r#""The word \"recursion\" has many meanings.""#
            ),
            [(
                r#"The word \"recursion\" has many meanings."#,
                TokenType::String
            )]
        );

        assert_eq!(
            expand(scan(r#""\\\\""#).unwrap(), r#""\\\\""#),
            [((r#"\\\\"#), TokenType::String)]
        );
    }

    #[test]
    fn numbers() {
        lexes! {
            "5" => TokenType::Number,
            "10" => TokenType::Number,
            "10.5" => TokenType::Number,
            "10..5" => TokenType::Number,
            "-42" => TokenType::Number,
            "+42" => TokenType::Number,
            "-10.5" => TokenType::Number
        };
    }

    #[test]
    fn dot() {
        lexes! {
            "(foo . bar)" =>
            ("(", TokenType::LeftParen),
            ("foo", TokenType::Symbol),
            (".", TokenType::Dot),
            ("bar", TokenType::Symbol),
            (")", TokenType::RightParen)
        };
        lexes! {
            "." => TokenType::Dot,
            ".." => TokenType::Symbol,
            "..." => TokenType::Symbol
        };
        lexes! {
            ".100" => TokenType::Number
        };
    }

    #[test]
    fn multiple_expressions() {
        lexes! {
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
        };
    }

    #[test]
    fn hash_prefixed_tokens() {
        lexes! {
            "#t" => TokenType::True,
            "#f" => TokenType::False
        };

        fails! {
            "#p" => Error::UnexpectedToken('#')
        };
    }
}
