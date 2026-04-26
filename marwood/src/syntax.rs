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
    /// Cache of the last computed spans. `highlight_check` returns
    /// true only when the *highlight result* differs from the cache,
    /// so xterm-readline's "skip refresh when nothing changed" path
    /// works for any keystroke that doesn't change which bracket pair
    /// is being highlighted (including text-shifting inserts inside
    /// the same enclosing pair).
    cache: RefCell<Option<Option<HighlightSpans>>>,
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
        *self.cache.borrow_mut() = Some(spans.clone());

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
            Some(cached) => cached != &current,
            None => current.is_some(),
        };
        if stale {
            *cache = Some(current);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_returns_true_then_false_when_unchanged() {
        let hl = ReplHighlighter::new();
        // Cursor on '(' of "(foo)" → matches ')'.
        assert!(hl.highlight_check("(foo)", 0));
        // Same call → cache hit, no refresh needed.
        assert!(!hl.highlight_check("(foo)", 0));
    }

    #[test]
    fn check_stays_false_when_typing_inside_paren_keeps_match_span() {
        let hl = ReplHighlighter::new();
        // Cursor on ')' of "(foo)" — match-bracket span is (0,1).
        assert!(hl.highlight_check("(foo)", 4));
        // Insert a char before ')'; cursor moved with it. Match-bracket
        // span is still (0,1), so the highlight doesn't change.
        assert!(!hl.highlight_check("(foob)", 5));
    }

    #[test]
    fn check_returns_true_when_pair_balance_changes() {
        let hl = ReplHighlighter::new();
        assert!(hl.highlight_check("(foo)", 4));
        // Typing '(' makes the buffer unbalanced: ')' at pos 5 no longer
        // has a match, so the cursor-on-bracket result changes.
        assert!(hl.highlight_check("(foo()", 5));
    }

    #[test]
    fn check_returns_false_when_cursor_moves_inside_same_pair() {
        let hl = ReplHighlighter::new();
        // Cursor between 'o' and 'o' of "(foo)" — enclosing pair (0,1)/(4,5).
        assert!(hl.highlight_check("(foo)", 2));
        // Move cursor one to the right; still inside the same pair.
        assert!(!hl.highlight_check("(foo)", 3));
    }
}
