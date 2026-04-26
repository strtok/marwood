use crate::lex;
use crate::lex::{Token, TokenType};
use std::borrow::Cow::{Borrowed, Owned};

pub struct ReplHighlighter {}

impl ReplHighlighter {
    pub fn new() -> ReplHighlighter {
        ReplHighlighter {}
    }
}

impl Default for ReplHighlighter {
    fn default() -> Self {
        ReplHighlighter::new()
    }
}

impl ReplHighlighter {
    pub fn highlight<'a>(&self, text: &'a str, index: usize) -> std::borrow::Cow<'a, str> {
        const ON: &str = "\x1b[1;33m";
        const OFF: &str = "\x1b[0m";

        let tokens = match lex::scan(text) {
            Ok(tokens) => tokens,
            Err(_) => {
                return Borrowed(text);
            }
        };

        // Case 1: the cursor sits on a bracket. Highlight the matching
        // partner only — the cursor itself marks the bracket the user
        // is on.
        if let Some(cur) = find_token_at_cursor(&tokens, index) {
            if matches!(
                cur.1.token_type,
                TokenType::LeftParen | TokenType::RightParen
            ) {
                if let Some(matched) = find_matching_bracket(&tokens, cur) {
                    let span = matched.span;
                    return Owned(format!(
                        "{}{}{}{}{}",
                        &text[..span.0],
                        ON,
                        &text[span.0..span.1],
                        OFF,
                        &text[span.1..]
                    ));
                }
                return Borrowed(text);
            }
        }

        // Case 2: cursor is between brackets. Highlight the innermost
        // enclosing pair so the user always sees their depth.
        if let Some((open, close)) = find_enclosing_pair(&tokens, index) {
            return Owned(format!(
                "{}{}{}{}{}{}{}{}{}",
                &text[..open.span.0],
                ON,
                &text[open.span.0..open.span.1],
                OFF,
                &text[open.span.1..close.span.0],
                ON,
                &text[close.span.0..close.span.1],
                OFF,
                &text[close.span.1..]
            ));
        }

        Borrowed(text)
    }

    pub fn highlight_check(&self, text: &str, mut index: usize) -> bool {
        let tokens = match lex::scan(text) {
            Ok(tokens) => tokens,
            Err(_) => {
                return false;
            }
        };
        index = index.saturating_sub(1);
        matches!(
            find_token_at_cursor(&tokens, index),
            Some((
                _,
                &Token {
                    token_type: TokenType::LeftParen | TokenType::RightParen,
                    ..
                },
            ))
        )
    }
}

fn find_matching_bracket<'a>(
    tokens: &'a [Token],
    bracket: (usize, &'a Token),
) -> Option<&'a Token> {
    let (have, want, mut iter): (TokenType, TokenType, Box<dyn Iterator<Item = &Token>>) =
        match bracket.1.token_type {
            TokenType::RightParen => (
                TokenType::RightParen,
                TokenType::LeftParen,
                Box::new(tokens[..(bracket.0)].iter().rev()),
            ),
            TokenType::LeftParen => (
                TokenType::LeftParen,
                TokenType::RightParen,
                Box::new(tokens[(bracket.0 + 1)..].iter()),
            ),
            _ => return None,
        };

    let mut stack = 0;
    for it in &mut *iter {
        if it.token_type == have {
            stack += 1;
        }

        if it.token_type == want {
            if stack == 0 {
                return Some(it);
            } else {
                stack -= 1;
            }
        }
    }

    None
}

fn find_token_at_cursor(tokens: &[Token], index: usize) -> Option<(usize, &Token)> {
    match find_token_at_index(tokens, index) {
        Some(token) => Some(token),
        _ => {
            if index > 0 {
                find_token_at_index(tokens, index - 1)
            } else {
                None
            }
        }
    }
}

fn find_token_at_index(tokens: &[Token], index: usize) -> Option<(usize, &Token)> {
    tokens
        .iter()
        .enumerate()
        .find(|(_, it)| index >= it.span.0 && index < it.span.1)
}

/// Find the innermost `( ... )` pair that encloses `cursor`. Returns
/// (open, close) tokens, or None when the cursor isn't inside a
/// closed parenthesised form.
fn find_enclosing_pair(tokens: &[Token], cursor: usize) -> Option<(&Token, &Token)> {
    let mut stack: Vec<usize> = Vec::new();
    for (i, tok) in tokens.iter().enumerate() {
        if tok.span.0 >= cursor {
            break;
        }
        match tok.token_type {
            TokenType::LeftParen => stack.push(i),
            TokenType::RightParen => {
                stack.pop();
            }
            _ => {}
        }
    }
    let open_idx = *stack.last()?;
    let open = &tokens[open_idx];
    let close = find_matching_bracket(tokens, (open_idx, open))?;
    if close.span.1 > cursor {
        Some((open, close))
    } else {
        None
    }
}
