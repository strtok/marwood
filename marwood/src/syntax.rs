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
        let tokens = match lex::scan(text) {
            Ok(tokens) => tokens,
            Err(_) => {
                return Borrowed(text);
            }
        };

        let cur_token = match find_token_at_cursor(&tokens, index) {
            Some(token) => token,
            None => {
                return Borrowed(text);
            }
        };

        let highlight_bracket = match find_matching_bracket(&tokens, cur_token) {
            Some(token) => token,
            None => {
                return Borrowed(text);
            }
        };

        let span = highlight_bracket.span;
        let bracket_text = &text[(span.0)..(span.1)];
        let replace = format!(
            "{}\x1b[4m{}\x1b[0m{}",
            &text[0..(span.0)],
            bracket_text,
            &text[(span.1)..]
        );
        Owned(replace)
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
