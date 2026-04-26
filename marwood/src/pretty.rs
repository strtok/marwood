//! Indent-aware Lisp pretty-printer.
//!
//! Renders a [`Cell`] within a target column width. If the value fits
//! on a single line within `width` it's emitted verbatim via the
//! existing [`Display`](crate::cell::Cell) impl. Otherwise the printer
//! breaks the form across lines using a small special-form table so
//! `define` / `let` / `cond` etc. indent the way a Lisp reader expects.
//!
//! Atoms are unbreakable: a single symbol or string longer than
//! `width` overflows rather than being truncated.

use crate::cell::Cell;

/// Render `cell` as a string targeting `width` columns. Lines longer
/// than `width` are only produced when no break is possible (atoms
/// that can't fit, list heads forced onto an already-indented line).
pub fn format(cell: &Cell, width: usize) -> String {
    let mut p = Printer {
        width,
        out: String::new(),
        col: 0,
    };
    p.print(cell);
    p.out
}

struct Printer {
    width: usize,
    out: String,
    col: usize,
}

impl Printer {
    fn raw(&mut self, s: &str) {
        for ch in s.chars() {
            if ch == '\n' {
                self.col = 0;
            } else {
                self.col += 1;
            }
        }
        self.out.push_str(s);
    }

    fn newline(&mut self, indent: usize) {
        self.out.push('\n');
        for _ in 0..indent {
            self.out.push(' ');
        }
        self.col = indent;
    }

    fn fits_single_line(&self, single: &str) -> bool {
        // Single-line renderings never contain '\n' for any Cell variant,
        // so character count == column advance.
        self.col + single.chars().count() <= self.width
    }

    fn print(&mut self, cell: &Cell) {
        let single = format!("{:#}", cell);
        if self.fits_single_line(&single) {
            self.raw(&single);
            return;
        }
        match cell {
            Cell::Pair(_, _) => self.print_pair(cell),
            Cell::Vector(items) => self.print_vector(items),
            Cell::DatumDef(label, value) => {
                self.raw(&format!("#{}=", label));
                self.print(value);
            }
            _ => self.raw(&single),
        }
    }

    fn print_pair(&mut self, cell: &Cell) {
        // Sugar quote-like forms back to their reader prefix.
        if let Some(prefix) = quote_prefix(cell) {
            self.raw(prefix);
            self.print(cell.cadr().unwrap());
            return;
        }
        if cell.is_list() {
            let elts: Vec<&Cell> = cell.iter().collect();
            if elts.is_empty() {
                self.raw("()");
                return;
            }
            match lookup_rule(elts[0]) {
                Rule::Body(n) => self.print_body_form(&elts, n),
                Rule::AlignArgs => self.print_align_args(&elts),
            }
        } else if cell.is_improper_list() {
            self.print_improper(cell);
        } else {
            // A bare dotted pair (a . b). Cell::Display already handles
            // this on a single line; no useful break point.
            self.raw(&format!("{:#}", cell));
        }
    }

    /// `(head arg1 .. arg_special body...)` — the head plus
    /// `special_count` arguments stay on the first line, remaining
    /// elements indent two columns from the open paren.
    fn print_body_form(&mut self, elts: &[&Cell], special_count: usize) {
        let open_col = self.col;
        self.raw("(");
        let body_indent = open_col + 2;
        self.print(elts[0]);
        let limit = (1 + special_count).min(elts.len());
        for elt in &elts[1..limit] {
            self.raw(" ");
            self.print(elt);
        }
        for elt in &elts[limit..] {
            self.newline(body_indent);
            self.print(elt);
        }
        self.raw(")");
    }

    /// `(head arg1 arg2 ...)` — when head is a symbol (a procedure
    /// call) the first arg stays on the open-paren line and subsequent
    /// args align under its column. When head isn't a symbol the form
    /// is treated as data: every element gets its own line aligned
    /// under the first element.
    fn print_align_args(&mut self, elts: &[&Cell]) {
        self.raw("(");
        let first_col = self.col;
        self.print(elts[0]);
        if elts.len() == 1 {
            self.raw(")");
            return;
        }
        if elts[0].as_symbol().is_some() {
            self.raw(" ");
            let arg_col = self.col;
            self.print(elts[1]);
            for elt in &elts[2..] {
                self.newline(arg_col);
                self.print(elt);
            }
        } else {
            for elt in &elts[1..] {
                self.newline(first_col);
                self.print(elt);
            }
        }
        self.raw(")");
    }

    fn print_improper(&mut self, cell: &Cell) {
        let mut elts: Vec<&Cell> = Vec::new();
        let mut tail: Option<&Cell> = None;
        let mut current = cell;
        loop {
            match current {
                Cell::Pair(car, cdr) => {
                    elts.push(car.as_ref());
                    current = cdr.as_ref();
                }
                Cell::Nil => break,
                other => {
                    tail = Some(other);
                    break;
                }
            }
        }
        self.raw("(");
        if elts.is_empty() {
            if let Some(t) = tail {
                self.print(t);
            }
            self.raw(")");
            return;
        }
        self.print(elts[0]);
        if elts.len() == 1 && tail.is_none() {
            self.raw(")");
            return;
        }
        self.raw(" ");
        let align_col = self.col;
        if elts.len() > 1 {
            self.print(elts[1]);
            for elt in &elts[2..] {
                self.newline(align_col);
                self.print(elt);
            }
        }
        if let Some(t) = tail {
            self.newline(align_col);
            self.raw(". ");
            self.print(t);
        }
        self.raw(")");
    }

    fn print_vector(&mut self, items: &[Cell]) {
        self.raw("#(");
        if items.is_empty() {
            self.raw(")");
            return;
        }
        let elt_col = self.col;
        self.print(&items[0]);
        for item in &items[1..] {
            self.newline(elt_col);
            self.print(item);
        }
        self.raw(")");
    }
}

fn quote_prefix(cell: &Cell) -> Option<&'static str> {
    let car = cell.car()?;
    let cdr = cell.cdr()?;
    if !cdr.is_pair() {
        return None;
    }
    if !cdr.cdr()?.is_nil() {
        return None;
    }
    if car.is_quote() {
        Some("'")
    } else if car.is_quasiquote() {
        Some("`")
    } else if car.is_unquote() {
        Some(",")
    } else {
        None
    }
}

enum Rule {
    /// `Body(n)`: head + the next `n` elements stay on the open-paren
    /// line; the rest indent +2 from the open paren.
    Body(usize),
    /// Head + first arg on open-paren line; remaining args align
    /// under the first arg's column.
    AlignArgs,
}

fn lookup_rule(head: &Cell) -> Rule {
    match head.as_symbol() {
        Some(s) => match s {
            // `Body(1)` — first non-head element is the "special" form
            // (binding list, parameter list, condition, etc.)
            "define" | "define-syntax" | "let" | "let*" | "letrec" | "letrec*"
            | "let-values" | "let*-values" | "lambda" | "if" | "do" => Rule::Body(1),
            // `Body(0)` — every body element indents under the head.
            "begin" | "when" | "unless" | "cond" | "case" | "and" | "or"
            | "syntax-rules" => Rule::Body(0),
            _ => Rule::AlignArgs,
        },
        None => Rule::AlignArgs,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    fn p(s: &str) -> Cell {
        parse::parse_text(s).expect("parse failed").0
    }

    #[test]
    fn atom_fits_on_one_line() {
        assert_eq!(format(&p("42"), 80), "42");
        assert_eq!(format(&p("'foo"), 80), "'foo");
        assert_eq!(format(&p("#t"), 80), "#t");
    }

    #[test]
    fn atom_overflows_when_longer_than_width() {
        // Symbols, strings, etc. are unbreakable; they overflow rather
        // than get truncated or wrapped.
        let long_sym = "abcdefghijklmnop";
        assert_eq!(format(&p(long_sym), 8), long_sym);
    }

    #[test]
    fn short_list_stays_on_one_line() {
        assert_eq!(format(&p("(+ 1 2)"), 80), "(+ 1 2)");
        assert_eq!(format(&p("(define x 1)"), 80), "(define x 1)");
    }

    #[test]
    fn list_breaks_when_overflowing() {
        let out = format(&p("(foo a b c d)"), 6);
        assert_eq!(out, "(foo a\n     b\n     c\n     d)");
    }

    #[test]
    fn define_form_indents_body_plus_two() {
        let out = format(&p("(define (f x) (+ x 1) (* x 2))"), 20);
        assert_eq!(out, "(define (f x)\n  (+ x 1)\n  (* x 2))");
    }

    #[test]
    fn lambda_form_indents_body_plus_two() {
        let out = format(&p("(lambda (x y) (+ x y) (- x y))"), 20);
        assert_eq!(out, "(lambda (x y)\n  (+ x y)\n  (- x y))");
    }

    #[test]
    fn let_form_indents_body_plus_two() {
        let out = format(&p("(let ((a 1) (b 2)) (+ a b))"), 18);
        assert_eq!(out, "(let ((a 1) (b 2))\n  (+ a b))");
    }

    #[test]
    fn let_form_breaks_bindings_when_too_wide() {
        let out = format(&p("(let ((aaaaa 1) (bbbbb 2)) body)"), 16);
        assert!(
            out.starts_with("(let ((aaaaa 1)\n"),
            "expected bindings to break, got:\n{}",
            out
        );
        assert!(
            out.contains("\n  body)"),
            "expected body indented +2, got:\n{}",
            out
        );
    }

    #[test]
    fn if_form_indents_branches() {
        let out = format(&p("(if (> x 0) 'positive 'non-positive)"), 16);
        assert_eq!(out, "(if (> x 0)\n  'positive\n  'non-positive)");
    }

    #[test]
    fn cond_clauses_each_on_own_line() {
        // cond is Body(0): every element after the head indents +2.
        let out = format(
            &p("(cond ((= x 1) 'one) ((= x 2) 'two) (else 'other))"),
            24,
        );
        assert_eq!(
            out,
            "(cond\n  ((= x 1) 'one)\n  ((= x 2) 'two)\n  (else 'other))"
        );
    }

    #[test]
    fn begin_body_indents_two() {
        let out = format(&p("(begin (display \"hi\") (newline) 42)"), 16);
        assert_eq!(out, "(begin\n  (display \"hi\")\n  (newline)\n  42)");
    }

    #[test]
    fn quoted_data_keeps_sugar() {
        // 'X stays as 'X, not (quote X).
        assert_eq!(format(&p("'(1 2 3)"), 80), "'(1 2 3)");
        let out = format(&p("'(aaaa bbbb cccc dddd)"), 12);
        assert!(out.starts_with("'("), "expected quote sugar: {}", out);
    }

    #[test]
    fn nested_breaks_propagate() {
        let out = format(
            &p("(define (greet name) (display \"hello, \") (display name) (newline))"),
            30,
        );
        assert_eq!(
            out,
            "(define (greet name)\n  (display \"hello, \")\n  (display name)\n  (newline))"
        );
    }

    #[test]
    fn empty_list_renders_as_paren_pair() {
        assert_eq!(format(&Cell::Nil, 80), "()");
        assert_eq!(format(&p("'()"), 80), "'()");
    }

    #[test]
    fn dotted_pair_single_line() {
        assert_eq!(format(&p("(1 . 2)"), 80), "(1 . 2)");
    }

    #[test]
    fn improper_list_breaks_with_dotted_tail() {
        let out = format(&p("(aaa bbb ccc . ddd)"), 8);
        assert_eq!(out, "(aaa bbb\n     ccc\n     . ddd)");
    }

    #[test]
    fn vector_breaks_into_aligned_elements() {
        let v = Cell::Vector(vec![
            Cell::Symbol("aaaa".into()),
            Cell::Symbol("bbbb".into()),
            Cell::Symbol("cccc".into()),
        ]);
        let out = format(&v, 8);
        assert_eq!(out, "#(aaaa\n  bbbb\n  cccc)");
    }

    #[test]
    fn empty_vector_single_line() {
        assert_eq!(format(&Cell::Vector(vec![]), 80), "#()");
    }

    #[test]
    fn nested_let_in_define() {
        // At width 22 the inner let bindings still fit on one line.
        let out = format(&p("(define (sum a b) (let ((x a) (y b)) (+ x y)))"), 22);
        assert_eq!(out, "(define (sum a b)\n  (let ((x a) (y b))\n    (+ x y)))");
    }

    #[test]
    fn nested_let_breaks_bindings_when_indent_pushes_overflow() {
        // At width 18 the body-indented let bindings don't fit, so
        // the bindings list itself breaks.
        let out = format(&p("(define (sum a b) (let ((x a) (y b)) (+ x y)))"), 18);
        assert_eq!(
            out,
            "(define (sum a b)\n  (let ((x a)\n        (y b))\n    (+ x y)))"
        );
    }

    #[test]
    fn function_call_aligns_args_under_first() {
        let out = format(&p("(very-long-name aaa bbb ccc)"), 22);
        assert_eq!(out, "(very-long-name aaa\n                bbb\n                ccc)");
    }

    #[test]
    fn cycles_render_via_datum_labels() {
        let out = format(&p("#0=(1 2 . #0#)"), 80);
        assert!(out.contains("#0=") && out.contains("#0#"), "got: {}", out);
    }

    #[test]
    fn long_atom_inside_breaking_form_overflows_locally() {
        // The `define` rule still breaks the body; the long atom stays
        // on its own line and overflows width.
        let long = "verylongnamethatdoesntfit";
        let src = format!("(define x {})", long);
        let out = format(&p(&src), 12);
        assert!(out.contains("\n  verylongnamethatdoesntfit"), "got: {}", out);
    }

    #[test]
    fn formatted_output_round_trips_through_parser() {
        // Whatever shape the printer produces, the result must lex/parse
        // back to the same Cell.
        let original = p("(define (f x) (let ((a 1) (b 2)) (+ a b x)))");
        let formatted = format(&original, 12);
        let reparsed = p(&formatted);
        assert_eq!(reparsed, original);
    }
}
