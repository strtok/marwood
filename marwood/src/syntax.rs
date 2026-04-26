use crate::lex;
use crate::lex::{Token, TokenType};
use std::borrow::Cow::{Borrowed, Owned};
use std::cell::RefCell;

#[derive(Clone, PartialEq, Eq)]
enum HighlightSpans {
    /// Cursor is on a bracket; highlight just its match.
    Match((usize, usize)),
    /// Cursor is between brackets; highlight the enclosing pair.
    Pair((usize, usize), (usize, usize)),
}

pub struct ReplHighlighter {
    /// Cache of the last (text, computed-spans) pair. `highlight_check`
    /// returns true only when the *highlight* differs from the cache,
    /// so xterm-readline's "skip refresh when nothing changed" path
    /// is preserved when the cursor moves inside the same enclosing
    /// pair.
    cache: RefCell<Option<(String, Option<HighlightSpans>)>>,
}

impl ReplHighlighter {
    pub fn new() -> ReplHighlighter {
        ReplHighlighter {
            cache: RefCell::new(None),
        }
    }
}

impl Default for ReplHighlighter {
    fn default() -> Self {
        ReplHighlighter::new()
    }
}

impl ReplHighlighter {
    /// Compute which spans, if any, would be highlighted for the given
    /// text and cursor position. Pure function shared by highlight()
    /// and highlight_check() so they can never disagree.
    fn compute_spans(text: &str, index: usize) -> Option<HighlightSpans> {
        let tokens = lex::scan(text).ok()?;

        // Case 1: cursor is on a bracket — highlight the match only.
        if let Some(cur) = find_token_at_cursor(&tokens, index) {
            if matches!(
                cur.1.token_type,
                TokenType::LeftParen | TokenType::RightParen
            ) {
                return find_matching_bracket(&tokens, cur).map(|m| HighlightSpans::Match(m.span));
            }
        }

        // Case 2: cursor is between brackets — highlight enclosing pair.
        find_enclosing_pair(&tokens, index)
            .map(|(open, close)| HighlightSpans::Pair(open.span, close.span))
    }

    pub fn highlight<'a>(&self, text: &'a str, index: usize) -> std::borrow::Cow<'a, str> {
        const ON: &str = "\x1b[1;33m";
        const OFF: &str = "\x1b[0m";

        let spans = Self::compute_spans(text, index);
        // Keep the cache in sync with what's been rendered, so the
        // next highlight_check has an accurate baseline.
        *self.cache.borrow_mut() = Some((text.to_string(), spans.clone()));

        match spans {
            None => Borrowed(text),
            Some(HighlightSpans::Match(span)) => Owned(format!(
                "{}{}{}{}{}",
                &text[..span.0],
                ON,
                &text[span.0..span.1],
                OFF,
                &text[span.1..]
            )),
            Some(HighlightSpans::Pair(open, close)) => Owned(format!(
                "{}{}{}{}{}{}{}{}{}",
                &text[..open.0],
                ON,
                &text[open.0..open.1],
                OFF,
                &text[open.1..close.0],
                ON,
                &text[close.0..close.1],
                OFF,
                &text[close.1..]
            )),
        }
    }

    pub fn highlight_check(&self, text: &str, index: usize) -> bool {
        let current = Self::compute_spans(text, index);
        let mut cache = self.cache.borrow_mut();
        let stale = match &*cache {
            Some((cached_text, cached_spans)) => {
                cached_text != text || cached_spans != &current
            }
            None => current.is_some(),
        };
        if stale {
            *cache = Some((text.to_string(), current));
        }
        stale
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
