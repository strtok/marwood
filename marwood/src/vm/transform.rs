use crate::cell;
use crate::cell::Cell;
use crate::error::Error;
use crate::error::Error::InvalidSyntax;
use std::collections::HashSet;
use std::sync::atomic::{AtomicU32, Ordering};

/// Mint a fresh hygiene scope. Scope 0 is reserved for unscoped
/// (`Cell::Symbol`) identifiers. Two kinds of scope are minted from
/// this single counter:
///
/// * A *use scope* — one per macro-expansion invocation, stamped onto
///   identifiers introduced by the template (binders like `tmp`).
/// * A *definition scope* — one per macro at `define-syntax` time,
///   stamped onto template identifiers whose binding was captured from
///   the macro's definition environment (e.g. references to `+`).
pub(crate) fn mint_scope() -> u32 {
    static COUNTER: AtomicU32 = AtomicU32::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

macro_rules! car {
    ($cell:expr) => {{
        $cell
            .car()
            .ok_or(Error::ExpectedPairButFound($cell.clone()))?
    }};
}

macro_rules! cdr {
    ($cell:expr) => {{
        $cell
            .cdr()
            .ok_or(Error::ExpectedPairButFound($cell.clone()))?
    }};
}

/// Pattern
///
/// Pattern represents a single syntax-rules pattern. `variables` is
/// the set of pattern variables encountered in the pattern, each
/// paired with its ellipsis nesting depth (0 = not under any
/// ellipsis, 1 = under one ellipsis, etc.).
#[derive(Debug, Eq, PartialEq)]
pub struct Pattern {
    expr: Cell,
    variables: Vec<(Cell, usize)>,
    ellipsis: Cell,
    literals: Vec<Cell>,
    underscore: Cell,
}

impl Pattern {
    pub fn try_new(expr: &Cell, ellipsis: &Cell, literals: &[Cell]) -> Result<Pattern, Error> {
        if !expr.is_pair() {
            return Err(InvalidSyntax("pattern must be a ()".into()));
        }

        let mut pattern = Pattern {
            expr: expr.clone(),
            variables: vec![],
            ellipsis: ellipsis.clone(),
            literals: literals.to_vec(),
            underscore: cell!["_"],
        };

        Self::build(cdr!(expr), &mut pattern, 0)?;
        Ok(pattern)
    }

    pub fn is_ellipsis(&self, cell: &Cell) -> bool {
        *cell == self.ellipsis
    }

    pub fn is_literal(&self, cell: &Cell) -> bool {
        self.literals.iter().any(|it| it == cell)
    }

    pub fn is_variable(&self, cell: &Cell) -> bool {
        self.variables.iter().any(|(v, _)| v == cell)
    }

    pub fn variable_depth(&self, cell: &Cell) -> Option<usize> {
        self.variables
            .iter()
            .find(|(v, _)| v == cell)
            .map(|(_, d)| *d)
    }

    fn is_variable_candidate(&self, cell: &Cell) -> bool {
        cell.is_symbol()
            && !self.is_literal(cell)
            && !self.is_ellipsis(cell)
            && *cell != self.underscore
    }

    fn build(expr: &Cell, pattern: &mut Pattern, depth: usize) -> Result<(), Error> {
        let improper = expr.is_improper_list();
        let len = expr.len();
        let mut iter = expr.iter().enumerate().peekable();
        let mut ellipsis_ct = 0;
        while let Some((idx, it)) = iter.next() {
            let ellipsis_next = match iter.peek() {
                Some((_, cell)) => pattern.is_ellipsis(cell),
                _ => false,
            };
            let effective_depth = depth + if ellipsis_next { 1 } else { 0 };
            match it {
                Cell::Symbol(_) => {
                    if pattern.is_ellipsis(it) {
                        if idx == 0 || (idx == len - 1 && improper) {
                            return Err(InvalidSyntax(format!(
                                "invalid ellipsis placement in {:#}",
                                expr
                            )));
                        }
                        ellipsis_ct += 1;
                        if ellipsis_ct > 1 {
                            return Err(InvalidSyntax(format!("duplicate ellipsis in {:#}", expr)));
                        }
                        continue;
                    }

                    if pattern.is_variable_candidate(it) {
                        if pattern.is_variable(it) {
                            return Err(InvalidSyntax(format!(
                                "duplicate pattern variable {:#}",
                                it
                            )));
                        }
                        pattern.variables.push((it.clone(), effective_depth));
                    } else if ellipsis_next {
                        return Err(InvalidSyntax(
                            "ellipsis must follow pattern variable".into(),
                        ));
                    }
                }
                Cell::Pair(_, _) => {
                    Self::build(it, pattern, effective_depth)?;
                }
                _ => {}
            }
        }
        Ok(())
    }
}

/// Result of a successful pattern match. Shape follows the pattern's
/// ellipsis nesting: a variable of depth `d` is stored as `d` nested
/// `Seq`s with `Leaf`s at the bottom.
#[derive(Debug, Clone)]
enum MatchValue<'a> {
    Leaf(&'a Cell),
    Seq(Vec<MatchValue<'a>>),
}

fn lookup_at<'a, 'b>(
    mv: &'b MatchValue<'a>,
    indices: &[usize],
) -> Option<&'b MatchValue<'a>> {
    let mut cur = mv;
    for &i in indices {
        match cur {
            MatchValue::Seq(v) => {
                cur = v.get(i)?;
            }
            MatchValue::Leaf(_) => return None,
        }
    }
    Some(cur)
}

/// Names that appear *free* in a (sub-)template — i.e. not pattern
/// variables, not the ellipsis marker, and not buried inside a
/// `(quote ...)` form. Symbols inside quotes are data, not identifier
/// references, so they don't participate in definition-environment
/// capture.
fn collect_free_names(
    template: &Cell,
    pattern: &Pattern,
    ellipsis: &Cell,
    quoted: bool,
    out: &mut HashSet<String>,
) {
    match template {
        Cell::Symbol(name) => {
            if quoted {
                return;
            }
            if pattern.is_variable(template) {
                return;
            }
            if template == ellipsis {
                return;
            }
            out.insert(name.clone());
        }
        Cell::Pair(_, _) => {
            let body: Vec<&Cell> = template.iter().collect();
            let body_quoted =
                quoted || (!body.is_empty() && body[0].as_symbol() == Some("quote"));
            for c in body {
                collect_free_names(c, pattern, ellipsis, body_quoted, out);
            }
        }
        _ => {}
    }
}

/// Pattern variables that appear in a (sub-)template.
fn collect_template_vars(template: &Cell, pattern: &Pattern, out: &mut Vec<Cell>) {
    match template {
        Cell::Symbol(_) => {
            if pattern.is_variable(template) && !out.iter().any(|v| v == template) {
                out.push(template.clone());
            }
        }
        Cell::Pair(_, _) => {
            for it in template {
                collect_template_vars(it, pattern, out);
            }
        }
        _ => {}
    }
}

/// Transform
///
/// Transform is a runtime representation of a set of syntax-rules.
#[derive(Debug, Eq, PartialEq)]
pub struct Transform {
    keyword: Cell,
    ellipsis: Cell,
    syntax_rules: Vec<(Pattern, Cell)>,
    literals: Vec<Cell>,
    /// Definition scope minted at `define-syntax` time. Free template
    /// identifiers whose names are in `captured` are stamped with this
    /// scope so they intern to env slots populated at definition time
    /// from the macro's enclosing environment, rather than to the
    /// user's current (possibly redefined) globals.
    def_scope: u32,
    /// Names that were resolvable in the global environment when this
    /// macro was defined. The compiler populates this set after
    /// `try_new` and before installing the macro on the heap; see
    /// `Compile::compile_define_syntax`.
    captured: HashSet<String>,
}

impl Transform {
    /// Try New
    ///
    /// Given a (define-syntax ...) expression, build a Transformer
    /// object or return an error.
    ///
    /// # Arguments
    /// `expr` - the full (define-syntax ...) expression
    pub fn try_new(expr: &Cell) -> Result<Transform, Error> {
        let expr = expr.collect_vec();
        let (keyword, mut syntax_rules) = match expr.as_slice() {
            [_, keyword, syntax_rules] => (*keyword, *syntax_rules),
            _ => return Err(InvalidSyntax("expected keyword and syntax-rules".into())),
        };

        // keyword must be a symbol
        if !keyword.is_symbol() {
            return Err(InvalidSyntax("keyword must be an identifier".into()));
        }

        // Skip past "syntax-rules"
        if car!(syntax_rules) != &cell!["syntax-rules"] {
            return Err(InvalidSyntax("expected syntax-rules".into()));
        }
        syntax_rules = cdr!(syntax_rules);

        // ellipsis
        let ellipsis = match car!(syntax_rules) {
            Cell::Symbol(_) => {
                let ellipsis = car!(syntax_rules).clone();
                syntax_rules = cdr!(syntax_rules);
                ellipsis
            }
            _ => cell!["..."],
        };

        // literals must be a list of symbols
        let literals = car!(syntax_rules)
            .collect_vec()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        if literals.iter().any(|it| !it.is_symbol()) {
            return Err(InvalidSyntax("literals must be identifiers".into()));
        }
        syntax_rules = cdr!(syntax_rules);

        let syntax_rules = syntax_rules.collect_vec();
        let mut syntax_rules_vec = vec![];
        for it in syntax_rules {
            let pattern = car!(it).clone();
            let template = car!(cdr!(it)).clone();
            let pattern = Pattern::try_new(&pattern, &ellipsis, &literals)?;
            Self::check_template_syntax(&template, &pattern, &ellipsis)?;
            syntax_rules_vec.push((pattern, template));
        }

        Ok(Transform {
            keyword: keyword.clone(),
            ellipsis,
            syntax_rules: syntax_rules_vec,
            literals,
            def_scope: mint_scope(),
            captured: HashSet::new(),
        })
    }

    /// Definition-time scope for this macro. Free template identifiers
    /// captured from the definition environment are stamped with this
    /// scope.
    pub fn def_scope(&self) -> u32 {
        self.def_scope
    }

    /// Collect names that appear free in any template — i.e. symbols
    /// that aren't pattern variables, the ellipsis, or buried inside a
    /// `(quote ...)`. These are the candidates to capture from the
    /// macro's definition environment at `define-syntax` time.
    pub fn collect_free_template_names(&self) -> HashSet<String> {
        let mut out = HashSet::new();
        for (pattern, template) in &self.syntax_rules {
            collect_free_names(template, pattern, &self.ellipsis, false, &mut out);
        }
        out
    }

    /// Mark a template-free name as having been captured from the
    /// definition environment. Names not marked are stamped with a
    /// fresh per-expansion scope as before.
    pub fn mark_captured(&mut self, name: String) {
        self.captured.insert(name);
    }

    /// Is Literal
    ///
    /// Is cell in the set of literals?
    pub fn is_literal(&self, cell: &Cell) -> bool {
        self.literals.iter().any(|it| it == cell)
    }

    pub fn keyword(&self) -> &Cell {
        &self.keyword
    }

    /// Check Template Syntax
    ///
    /// * Any symbol preceding an ellipsis must be a pattern variable
    /// * Like patterns, ellipsis must not be in the tail position of an
    ///   improper list.
    fn check_template_syntax(
        template: &Cell,
        pattern: &Pattern,
        ellipsis: &Cell,
    ) -> Result<(), Error> {
        let improper = template.is_improper_list();
        let mut ellipsis_in_pattern = false;
        let mut iter = template.iter().peekable();

        if template.is_pair() && car!(template) == ellipsis {
            return Err(InvalidSyntax("ellipsis out of place".into()));
        }

        while let Some(template) = iter.next() {
            match template {
                Cell::Pair(_, _) => Self::check_template_syntax(template, pattern, ellipsis)?,
                Cell::Symbol(_) => {
                    if !pattern.is_variable(template) && iter.peek() == Some(&ellipsis) {
                        return Err(InvalidSyntax(
                            "ellipses must follow a pattern variable".into(),
                        ));
                    }
                    if template == ellipsis {
                        if ellipsis_in_pattern || (improper && iter.peek().is_none()) {
                            return Err(InvalidSyntax("ellipses out of place".into()));
                        }
                        ellipsis_in_pattern = true;
                        continue;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Transform
    ///
    /// Transform the input expression given the syntax-rules defined in
    /// this transformer. An error is returned if none of the input expressions
    /// match the patterns specified in the syntax-rules of this transformer.
    ///
    /// # Arguments
    /// `expr` - The expression to transform
    pub fn transform(&self, expr: &Cell) -> Result<Cell, Error> {
        let invalid_syntax = || Err(InvalidSyntax(format!("{:#}", self.keyword)));
        if !expr.is_pair() {
            invalid_syntax()?;
        }

        for rule in &self.syntax_rules {
            let mut env: Vec<(Cell, MatchValue)> = vec![];
            if self.match_list(cdr!(&rule.0.expr), cdr!(expr), &mut env) {
                let scope = mint_scope();
                return self
                    .expand(&rule.1, &rule.0, &env, &mut vec![], scope, false)
                    .ok_or_else(|| InvalidSyntax(format!("{:#}", self.keyword)));
            }
        }

        Err(InvalidSyntax(format!(
            "no matching syntax for {}",
            self.keyword
        )))
    }

    /// Match a single pattern cell against an expression, pushing
    /// pattern-variable bindings into `env`.
    fn match_cell<'a>(
        &self,
        pattern: &Cell,
        expr: &'a Cell,
        env: &mut Vec<(Cell, MatchValue<'a>)>,
    ) -> bool {
        match pattern {
            Cell::Symbol(_) => {
                if self.is_literal(pattern) {
                    pattern == expr
                } else if pattern == &cell!["_"] {
                    true
                } else {
                    env.push((pattern.clone(), MatchValue::Leaf(expr)));
                    true
                }
            }
            Cell::Pair(_, _) => {
                if !(expr.is_pair() || expr.is_nil()) {
                    return false;
                }
                self.match_list(pattern, expr, env)
            }
            _ => pattern == expr,
        }
    }

    /// Match a list-shaped pattern (proper or improper) against a
    /// list-shaped expression. Handles at most one ellipsis segment.
    fn match_list<'a>(
        &self,
        pattern: &Cell,
        expr: &'a Cell,
        env: &mut Vec<(Cell, MatchValue<'a>)>,
    ) -> bool {
        let p_improper = pattern.is_improper_list();
        let e_improper = expr.is_improper_list();

        let p_all: Vec<&Cell> = pattern.iter().collect();
        let e_all: Vec<&Cell> = expr.iter().collect();

        let (p_elems, p_tail): (&[&Cell], Option<&Cell>) = if p_improper {
            let (b, t) = p_all.split_at(p_all.len() - 1);
            (b, Some(t[0]))
        } else {
            (&p_all[..], None)
        };
        let (e_elems, e_tail): (&[&Cell], Option<&Cell>) = if e_improper {
            let (b, t) = e_all.split_at(e_all.len() - 1);
            (b, Some(t[0]))
        } else {
            (&e_all[..], None)
        };

        let ellipsis_at = p_elems
            .iter()
            .position(|c| self.is_ellipsis_cell(c));

        match ellipsis_at {
            None => {
                if p_elems.len() != e_elems.len() {
                    return false;
                }
                if p_improper != e_improper {
                    return false;
                }
                for (pe, ee) in p_elems.iter().zip(e_elems.iter()) {
                    if !self.match_cell(pe, ee, env) {
                        return false;
                    }
                }
                if let (Some(pt), Some(et)) = (p_tail, e_tail)
                    && !self.match_cell(pt, et, env) {
                        return false;
                    }
                true
            }
            Some(i) => {
                // pattern layout: [pre...] sub ... [post...] [. tail]?
                // ellipsis is at p_elems[i]; sub is p_elems[i-1]; post is p_elems[i+1..]
                if i == 0 {
                    return false;
                }
                let sub = p_elems[i - 1];
                let pre = &p_elems[..i - 1];
                let post = &p_elems[i + 1..];

                if e_elems.len() < pre.len() + post.len() {
                    return false;
                }
                if p_improper != e_improper {
                    return false;
                }
                let repeat_count = e_elems.len() - pre.len() - post.len();

                // pre
                for (pe, ee) in pre.iter().zip(e_elems.iter()) {
                    if !self.match_cell(pe, ee, env) {
                        return false;
                    }
                }

                let mut sub_vars: Vec<Cell> = vec![];
                self.collect_pattern_vars(sub, &mut sub_vars);

                let mut seqs: Vec<Vec<MatchValue<'a>>> =
                    sub_vars.iter().map(|_| Vec::with_capacity(repeat_count)).collect();

                for k in 0..repeat_count {
                    let mut inner: Vec<(Cell, MatchValue<'a>)> = vec![];
                    if !self.match_cell(sub, e_elems[pre.len() + k], &mut inner) {
                        return false;
                    }
                    for (vi, v) in sub_vars.iter().enumerate() {
                        match inner.iter().find(|(n, _)| n == v) {
                            Some((_, mv)) => seqs[vi].push(mv.clone()),
                            None => seqs[vi].push(MatchValue::Seq(vec![])),
                        }
                    }
                }
                for (v, s) in sub_vars.into_iter().zip(seqs.into_iter()) {
                    env.push((v, MatchValue::Seq(s)));
                }

                // post
                let post_start = pre.len() + repeat_count;
                for (pe, ee) in post.iter().zip(e_elems[post_start..].iter()) {
                    if !self.match_cell(pe, ee, env) {
                        return false;
                    }
                }

                if let (Some(pt), Some(et)) = (p_tail, e_tail)
                    && !self.match_cell(pt, et, env) {
                        return false;
                    }
                true
            }
        }
    }

    fn is_ellipsis_cell(&self, cell: &Cell) -> bool {
        *cell == self.ellipsis
    }

    /// Route helper: collect pattern variables appearing in a sub-pattern.
    /// A pattern variable is any symbol that isn't a literal, the ellipsis,
    /// or `_`.
    fn collect_pattern_vars(&self, expr: &Cell, out: &mut Vec<Cell>) {
        match expr {
            Cell::Symbol(_) => {
                if expr.is_symbol()
                    && !self.is_literal(expr)
                    && !self.is_ellipsis_cell(expr)
                    && *expr != cell!["_"]
                    && !out.iter().any(|v| v == expr)
                {
                    out.push(expr.clone());
                }
            }
            Cell::Pair(_, _) => {
                for it in expr {
                    self.collect_pattern_vars(it, out);
                }
            }
            _ => {}
        }
    }

    /// Expand a template using the matched environment. `indices` tracks
    /// the current position within each enclosing ellipsis level.
    fn expand<'a>(
        &self,
        template: &Cell,
        pattern: &Pattern,
        env: &[(Cell, MatchValue<'a>)],
        indices: &mut Vec<usize>,
        scope: u32,
        quoted: bool,
    ) -> Option<Cell> {
        match template {
            Cell::Symbol(name) => {
                if pattern.is_variable(template) {
                    let mv = env.iter().find(|(v, _)| v == template).map(|(_, m)| m)?;
                    let depth = pattern.variable_depth(template).unwrap_or(0);
                    if indices.len() < depth {
                        return None;
                    }
                    let path = &indices[indices.len() - depth..];
                    match lookup_at(mv, path)? {
                        MatchValue::Leaf(c) => Some((*c).clone()),
                        MatchValue::Seq(_) => None,
                    }
                } else if quoted {
                    // Inside a (quote ...) form, template symbols are
                    // data, not identifier references — so they stay
                    // as plain `Cell::Symbol`. This keeps `(eq? 'foo
                    // (macro-that-returns-quoted-foo))` true, since
                    // marwood interns symbols by name.
                    Some(Cell::Symbol(name.clone()))
                } else {
                    // Template-literal identifier. If it was resolved
                    // in the macro's definition environment at
                    // `define-syntax` time, stamp it with the macro's
                    // *definition* scope so it interns to the env slot
                    // pre-populated by the compiler with the captured
                    // value. Otherwise stamp it with the per-expansion
                    // *use* scope, giving fresh hygienic identity to
                    // binders introduced by the template (`tmp`, etc.)
                    // and falling through to the user's live globals
                    // for anything not captured at def time.
                    let stamp = if self.captured.contains(name) {
                        self.def_scope
                    } else {
                        scope
                    };
                    Some(Cell::Identifier {
                        name: name.clone(),
                        scope: stamp,
                    })
                }
            }
            Cell::Pair(_, _) => {
                let improper = template.is_improper_list();
                let all: Vec<&Cell> = template.iter().collect();
                let (body, tail): (&[&Cell], Option<&Cell>) = if improper {
                    let (b, t) = all.split_at(all.len() - 1);
                    (b, Some(t[0]))
                } else {
                    (&all[..], None)
                };

                // If this pair is a `(quote ...)` form, expand its
                // body in quoted mode so symbols within are emitted
                // as data rather than scope-stamped identifiers.
                // Pattern-variable substitution still happens normally.
                let body_quoted = quoted
                    || (!body.is_empty() && body[0].as_symbol() == Some("quote"));

                let mut out: Vec<Cell> = vec![];
                let mut i = 0;
                while i < body.len() {
                    let sub = body[i];
                    let ellipsis_next =
                        i + 1 < body.len() && self.is_ellipsis_cell(body[i + 1]);
                    if ellipsis_next {
                        let n = self.driver_length(sub, pattern, env, indices)?;
                        for k in 0..n {
                            indices.push(k);
                            let r = self.expand(sub, pattern, env, indices, scope, body_quoted);
                            indices.pop();
                            out.push(r?);
                        }
                        i += 2;
                    } else {
                        out.push(self.expand(sub, pattern, env, indices, scope, body_quoted)?);
                        i += 1;
                    }
                }

                match tail {
                    Some(t) => {
                        let tail_expanded =
                            self.expand(t, pattern, env, indices, scope, body_quoted)?;
                        if matches!(tail_expanded, Cell::Nil) {
                            Some(Cell::new_list(out))
                        } else {
                            Some(Cell::new_improper_list(out, tail_expanded))
                        }
                    }
                    None => Some(Cell::new_list(out)),
                }
            }
            cell => Some(cell.clone()),
        }
    }

    /// Count of repetitions for an ellipsis expansion. Every pattern
    /// variable with depth >= 1 appearing in `sub` is a potential
    /// driver; its Seq at the fixed-prefix indices determines one
    /// candidate count. The ellipsis iterates the minimum across all
    /// candidates (marwood's chosen zip-truncation semantics).
    fn driver_length<'a>(
        &self,
        sub: &Cell,
        pattern: &Pattern,
        env: &[(Cell, MatchValue<'a>)],
        indices: &[usize],
    ) -> Option<usize> {
        let mut vars: Vec<Cell> = vec![];
        collect_template_vars(sub, pattern, &mut vars);
        let mut result: Option<usize> = None;
        for v in &vars {
            let depth = pattern.variable_depth(v).unwrap_or(0);
            if depth == 0 {
                continue;
            }
            // After this ellipsis is pushed, the var's binding window
            // is the last `depth` entries of indices. The new index
            // fills the innermost slot; the preceding `depth - 1`
            // entries come from the tail of `indices`.
            let new_len = indices.len() + 1;
            let start = new_len.saturating_sub(depth);
            if start > indices.len() {
                continue;
            }
            let prefix = &indices[start..];
            let mv = env.iter().find(|(n, _)| n == v).map(|(_, m)| m)?;
            let node = lookup_at(mv, prefix)?;
            let n = match node {
                MatchValue::Seq(v) => v.len(),
                MatchValue::Leaf(_) => continue,
            };
            result = Some(match result {
                Some(prev) => prev.min(n),
                None => n,
            });
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use crate::{cell, lex};

    /// Replace every `Cell::Identifier` in the result with the
    /// equivalent `Cell::Symbol`, so expansion-result assertions can
    /// compare against `parse!(...)` literals (which only ever produce
    /// `Cell::Symbol`). The scope tag is an internal hygiene marker
    /// and isn't user-observable.
    fn strip_scopes(c: Cell) -> Cell {
        match c {
            Cell::Identifier { name, .. } => Cell::Symbol(name),
            Cell::Pair(car, cdr) => Cell::Pair(
                Box::new(strip_scopes(*car)),
                Box::new(strip_scopes(*cdr)),
            ),
            Cell::Vector(v) => Cell::Vector(v.into_iter().map(strip_scopes).collect()),
            other => other,
        }
    }

    #[allow(dead_code)]
    fn expand_result(t: &Transform, src: &str) -> Result<Cell, Error> {
        t.transform(&parse!(src)).map(strip_scopes)
    }

    #[test]
    fn bad_patterns() {
        assert!(Pattern::try_new(&parse!("#t"), &cell!["..."], &[]).is_err());
        assert!(Pattern::try_new(&parse!("(_ ... a)"), &cell!["..."], &[]).is_err());
        assert!(Pattern::try_new(&parse!("(_ a . ...)"), &cell!["..."], &[]).is_err());
        assert!(Pattern::try_new(&parse!("(_ a a)"), &cell!["..."], &[]).is_err());
    }

    #[test]
    fn pattern_variable() {
        assert_eq!(
            Pattern::try_new(&parse!("(_ a b c)"), &cell!["..."], &[])
                .unwrap()
                .variables,
            vec![(cell!["a"], 0), (cell!["b"], 0), (cell!["c"], 0)]
        );
        assert_eq!(
            Pattern::try_new(&parse!("(_ a b . c)"), &cell!["..."], &[])
                .unwrap()
                .variables,
            vec![(cell!["a"], 0), (cell!["b"], 0), (cell!["c"], 0)]
        );
        assert_eq!(
            Pattern::try_new(&parse!("(_ a* ...)"), &cell!["..."], &[])
                .unwrap()
                .variables,
            vec![(cell!["a*"], 1)]
        );
        assert_eq!(
            Pattern::try_new(&parse!("(_ (a* b*) ...)"), &cell!["..."], &[])
                .unwrap()
                .variables,
            vec![(cell!["a*"], 1), (cell!["b*"], 1)]
        );
    }

    #[test]
    fn nested_ellipsis_variable_depths() {
        let pattern =
            Pattern::try_new(&parse!("(_ a (b (c ...)) ...)"), &cell!["..."], &[]).unwrap();
        assert_eq!(
            pattern.variables,
            vec![(cell!["a"], 0), (cell!["b"], 1), (cell!["c"], 2)]
        );
    }

    #[test]
    fn error_on_bad_form() {
        assert!(Transform::try_new(&parse!("(define-syntax)")).is_err());
        assert!(Transform::try_new(&parse!("(define-syntax 100)")).is_err());
        assert!(Transform::try_new(&parse!("(define-syntax '())")).is_err());
        assert!(Transform::try_new(&parse!("(define-syntax let not-a-list)")).is_err());
        assert!(
            Transform::try_new(&parse!("(define-syntax let (syntax-rules (1 2 3) ()))")).is_err()
        );
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax begin
              (not-expected-rules ()
                [(begin exp ...)
                 ((lambda () exp ...))]))"#
            ))
            .is_err()
        );
    }

    #[test]
    fn bad_pattern_syntax() {
        // Variable reuse
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ exp exp) ()]))
        "#
            ))
            .is_err()
        );

        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ exp . exp) ()]))
        "#
            ))
            .is_err()
        );

        // nested variable reuse
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ (exp) exp) ()]))
        "#
            ))
            .is_err()
        );

        // double ellipsis
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ foo ... bar ...) ()]))
        "#
            ))
            .is_err()
        );

        // ellipses out of place
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ (... foo)) ()]))
        "#
            ))
            .is_err()
        );

        // ellipsis in head position
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ (...)) ()]))
        "#
            ))
            .is_err()
        );

        // Ellipsis in improper list tail
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ foo . ...) ()]))
        "#
            ))
            .is_err()
        );

        // Ellipsis matching the keyword
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ ...) ()]))
        "#
            ))
            .is_err()
        );

        // Ellipsis matching _
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ _ ...) ()]))
        "#
            ))
            .is_err()
        );

        // Ellipsis matching a literal
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules (literal)
                [(_ literal ...) ()]))
        "#
            ))
            .is_err()
        );
    }

    #[test]
    fn bad_template_syntax() {
        // Expansion of a non-pattern variavle
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ a ...) (b ...)]))
        "#
            ))
            .is_err()
        );
        // Invalid ellipsis position
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ a ...) (...)]))
        "#
            ))
            .is_err()
        );
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ a ...) (a ... ...)]))
        "#
            ))
            .is_err()
        );
        assert!(
            Transform::try_new(&parse!(
                r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ a ...) (a . ...)]))
        "#
            ))
            .is_err()
        );
    }

    #[test]
    fn alternative_ellipsis_form() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax begin
              (syntax-rules ___ (bar baz)
                [(begin exp ...)
                 ((lambda () exp ...))]))
        "#
        ))
        .unwrap();
        assert_eq!(transform.ellipsis, cell!["___"]);
        assert_eq!(transform.literals, vec![cell!["bar"], cell!["baz"]]);
    }

    #[test]
    fn literals() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax begin
              (syntax-rules (bar baz)
                [(begin exp ...)
                 ((lambda () exp ...))]))
        "#
        ))
        .unwrap();
        assert_eq!(transform.keyword, cell!["begin"]);
        assert_eq!(transform.literals, vec![cell!["bar"], cell!["baz"]]);
        assert!(transform.is_literal(&cell!["bar"]));
        assert!(transform.is_literal(&cell!["baz"]));
        assert!(
            Transform::try_new(&parse!("(define-syntax let (syntax-rules (1 2 3) ()))")).is_err()
        );
    }

    #[test]
    fn single_pattern_variable() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax bind-zero
              (syntax-rules ()
                [(_ a) (define a 0)]
        ))
        "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(bind-zero b)"),
            Ok(parse!("(define b 0)"))
        );
    }

    #[test]
    fn nested_pattern_variables() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax add-nested
              (syntax-rules ()
                [(_ (x) (y)) (+ x y)]
        ))
        "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(add-nested (10) (20))"),
            Ok(parse!("(+ 10 20)"))
        );
    }

    #[test]
    fn single_variable_expansion() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax sum
              (syntax-rules ()
                [(sum a* ...) (+ a* ...)]
        ))
        "#
        ))
        .unwrap();
        assert_eq!(expand_result(&transform, "(sum)"), Ok(parse!("(+)")));
        assert_eq!(
            expand_result(&transform, "(sum 10)"),
            Ok(parse!("(+ 10)"))
        );
        assert_eq!(
            expand_result(&transform, "(sum 10 20)"),
            Ok(parse!("(+ 10 20)"))
        );
    }

    #[test]
    fn expansion_edge_cases() {
        {
            let transform = Transform::try_new(&parse!(
                r#"
            (define-syntax sum
                  (syntax-rules ()
                    [(sum a1 a* ... a2) (+ a1 a* ... a2)]
            ))
            "#
            ))
            .unwrap();
            assert_eq!(
                expand_result(&transform, "(sum 10 20)"),
                Ok(parse!("(+ 10 20)"))
            );
            assert_eq!(
                expand_result(&transform, "(sum 10 20 30)"),
                Ok(parse!("(+ 10 20 30)"))
            );
        }
        {
            let transform = Transform::try_new(&parse!(
                r#"
            (define-syntax sum
                  (syntax-rules ()
                    [(sum a1 a* ...) (+ a1 a* ...)]
            ))
            "#
            ))
            .unwrap();
            assert_eq!(
                expand_result(&transform, "(sum 10)"),
                Ok(parse!("(+ 10)"))
            );
            assert_eq!(
                expand_result(&transform, "(sum 10 20)"),
                Ok(parse!("(+ 10 20)"))
            );
            assert_eq!(
                expand_result(&transform, "(sum 10 20 30)"),
                Ok(parse!("(+ 10 20 30)"))
            );
        }
        {
            let transform = Transform::try_new(&parse!(
                r#"
            (define-syntax square
                  (syntax-rules ()
                    [(_ a) (* a a)]
            ))
            "#
            ))
            .unwrap();
            assert_eq!(
                expand_result(&transform, "(square 10)"),
                Ok(parse!("(* 10 10)"))
            );
        }
        {
            let transform = Transform::try_new(&parse!(
                r#"
            (define-syntax square-of-sums
                  (syntax-rules ()
                    [(_ a1 a* ...)
                     (* (+ a1 a* ...) (+ a1 a* ...))]
            ))
            "#
            ))
            .unwrap();
            assert_eq!(
                expand_result(&transform, "(square-of-sums 10)"),
                Ok(parse!("(* (+ 10) (+ 10))"))
            );
            assert_eq!(
                expand_result(&transform, "(square-of-sums 10 20)"),
                Ok(parse!("(* (+ 10 20) (+ 10 20))"))
            );
            assert_eq!(
                expand_result(&transform, "(square-of-sums 10 20 30)"),
                Ok(parse!("(* (+ 10 20 30) (+ 10 20 30))"))
            );
        }
    }

    #[test]
    fn literal() {
        let transform = Transform::try_new(&parse!(
            r#"
            (define-syntax sum
                  (syntax-rules (add sub)
                    [(math add a1 a2) (+ a1 a2)]
                    [(math sub a1 a2) (- a1 a2)]
            ))
            "#
        ))
        .unwrap();
        assert!(
            transform
                .transform(&parse!("(math multiply 10 10)"))
                .is_err()
        );
        assert_eq!(
            expand_result(&transform, "(math add 10 20)"),
            Ok(parse!("(+ 10 20)"))
        );
        assert_eq!(
            expand_result(&transform, "(math sub 10 20)"),
            Ok(parse!("(- 10 20)"))
        );
    }

    #[test]
    fn alternative_ellipsis() {
        let transform = Transform::try_new(&parse!(
            r#"
            (define-syntax sum
                  (syntax-rules * ()
                    [(sum a* *) (+ a* *)]
            ))
            "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(sum 10 20)"),
            Ok(parse!("(+ 10 20)"))
        );
    }

    #[test]
    fn underscore() {
        let transform = Transform::try_new(&parse!(
            r#"
            (define-syntax sum
                  (syntax-rules ()
                    [(sum _ a1 _ a2) (+ a1 a2)]
            ))
            "#
        ))
        .unwrap();
        assert!(expand_result(&transform, "(sum)").is_err());
        assert!(expand_result(&transform, "(sum 10)").is_err());
        assert!(expand_result(&transform, "(sum 10 20)").is_err());
        assert!(expand_result(&transform, "(sum 10 20 30 )").is_err());
        assert_eq!(
            expand_result(&transform, "(sum 10 20 30 40)"),
            Ok(parse!("(+ 20 40)"))
        );
    }

    #[test]
    fn underscore_as_literal() {
        let transform = Transform::try_new(&parse!(
            r#"
            (define-syntax sum
                  (syntax-rules (_)
                    [(sum _ a1 _ a2) (+ a1 a2)]
            ))
            "#
        ))
        .unwrap();
        assert!(expand_result(&transform, "(sum)").is_err());
        assert!(expand_result(&transform, "(sum 10)").is_err());
        assert!(expand_result(&transform, "(sum 10 20)").is_err());
        assert!(expand_result(&transform, "(sum 10 20 30 )").is_err());
        assert!(expand_result(&transform, "(sum 10 20 30 40)").is_err());
        assert_eq!(
            expand_result(&transform, "(sum _ 20 _ 40)"),
            Ok(parse!("(+ 20 40)"))
        );
    }

    #[test]
    fn zip_multi() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax zip-mult (syntax-rules ()
            [(_ (x x* ...) (y y* ...))
             (+ (* x y) (* x* y*) ...)]))
        "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(zip-multi (10) (10))"),
            Ok(parse!("(+ (* 10 10))"))
        );
        assert_eq!(
            expand_result(&transform, "(zip-mult (10 20 30) (10 20 30))"),
            Ok(parse!("(+ (* 10 10) (* 20 20) (* 30 30))"))
        );
        assert_eq!(
            expand_result(&transform, "(zip-mult (10 20 30 40) (10 20 30))"),
            Ok(parse!("(+ (* 10 10) (* 20 20) (* 30 30))"))
        );
        assert_eq!(
            expand_result(&transform, "(zip-mult (10 20 30) (10 20 30 40))"),
            Ok(parse!("(+ (* 10 10) (* 20 20) (* 30 30))"))
        );
    }

    #[test]
    fn begin_macro() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax begin
              (syntax-rules ()
                [(begin exp ...)
                 ((lambda () exp ...))]))
        "#
        ))
        .unwrap();
        assert_eq!(transform.keyword, cell!["begin"]);
        assert_eq!(
            expand_result(&transform, "(begin)"),
            Ok(parse!("((lambda ()))"))
        );
        assert_eq!(
            expand_result(&transform, "(begin 1 2 3)"),
            Ok(parse!("((lambda () 1 2 3))"))
        );
    }

    #[test]
    fn when_macro() {
        let transform = Transform::try_new(&parse!(
            r#"
       (define-syntax when
          (syntax-rules ()
            [(when test result1 result2 ...)
             (if test
                 (begin result1 result2 ...))]))
        "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(when #t 1)"),
            Ok(parse!("(if #t (begin 1))"))
        );
        assert_eq!(
            expand_result(&transform, "(when (< x 10) 'a 'b 'c)"),
            Ok(parse!("(if (< x 10) (begin 'a 'b 'c))"))
        );
        // missing required result1 -> no matching rule
        assert!(expand_result(&transform, "(when #t)").is_err());
    }

    #[test]
    fn and_macro() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax and
          (syntax-rules ()
            [(and) #t]
            [(and test) test]
            [(and test1 test2 ...)
             (if test1 (and test2 ...) #f)]))
        "#
        ))
        .unwrap();
        assert_eq!(expand_result(&transform, "(and)"), Ok(parse!("#t")));
        assert_eq!(expand_result(&transform, "(and 5)"), Ok(parse!("5")));
        // Only one expansion step happens here; the recursive (and 2 3)
        // is re-expanded by the compiler, not by transform itself.
        assert_eq!(
            expand_result(&transform, "(and 1 2 3)"),
            Ok(parse!("(if 1 (and 2 3) #f)"))
        );
    }

    #[test]
    fn or_macro() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax or
          (syntax-rules ()
            [(or) #f]
            [(or test) test]
            [(or test1 test2 ...)
             (let ((x test1))
               (if x x (or test2 ...)))]))
        "#
        ))
        .unwrap();
        assert_eq!(expand_result(&transform, "(or)"), Ok(parse!("#f")));
        assert_eq!(expand_result(&transform, "(or 5)"), Ok(parse!("5")));
        assert_eq!(
            expand_result(&transform, "(or 1 2)"),
            Ok(parse!("(let ((x 1)) (if x x (or 2)))"))
        );
        assert_eq!(
            expand_result(&transform, "(or 1 2 3)"),
            Ok(parse!("(let ((x 1)) (if x x (or 2 3)))"))
        );
    }

    #[test]
    fn trivial_let_macro() {
        let transform = Transform::try_new(&parse!(
            r#"
        (define-syntax let
            (syntax-rules ()
            [(let ((name val) ...) body1 body2 ...)
                ((lambda (name ...) body1 body2 ...) val ...)]))
        "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(let () 42)"),
            Ok(parse!("((lambda () 42))"))
        );
        assert_eq!(
            expand_result(&transform, "(let ((x 10)) (* x x))"),
            Ok(parse!("((lambda (x) (* x x)) 10)"))
        );
        assert_eq!(
            expand_result(&transform, "(let ((x 10) (y 20)) (+ x y))"),
            Ok(parse!("((lambda (x y) (+ x y)) 10 20)"))
        );
        // body1 is required -> no matching rule
        assert!(expand_result(&transform, "(let ())").is_err());
    }

    /// Originally removed in commit 5d1f263 "remove failing tests".
    /// A later fix (fe76def "allow captured values to be reused") made
    /// this work, so re-add it to lock in the behaviour.
    #[test]
    fn pattern_variable_used_twice_in_template() {
        let transform = Transform::try_new(&parse!(
            r#"
                (define-syntax foo (syntax-rules ()
                   [(_ a b* ...)
                    '((a b*) ...)]))
            "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(foo bar 1 2 3)"),
            Ok(parse!("'((bar 1) (bar 2) (bar 3))"))
        );
    }

    /// Nested ellipsis where a pattern variable is used at a deeper
    /// ellipsis nesting in the template than in the pattern. `a*` is
    /// bound at depth 1 and broadcast under the outer `...`.
    #[test]
    fn nested_expansion() {
        let transform = Transform::try_new(&parse!(
            r#"
            (define-syntax foo (syntax-rules ()
                [(_ (a* ...))
                 '(((a* (a* ...)) ... ))]))
            "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(foo (1 2 3))"),
            Ok(parse!("'(((1 (1 2 3)) (2 (1 2 3)) (3 (1 2 3))))"))
        );
    }

    /// Nested ellipsis must keep the per-iteration grouping of inner
    /// bindings: the `b ...` inside the outer `...` expands only to
    /// the `b`s captured during the matching outer `a` iteration.
    #[test]
    fn nested_ellipsis_preserves_grouping() {
        let transform = Transform::try_new(&parse!(
            r#"
            (define-syntax foo (syntax-rules ()
                [(_ ((a b ...) ...))
                 (list (list a (list b ...)) ...)]))
            "#
        ))
        .unwrap();
        assert_eq!(
            expand_result(&transform, "(foo ((1 2 3) (10 20 30 40) (100)))"),
            Ok(parse!("(list (list 1 (list 2 3)) (list 10 (list 20 30 40)) (list 100 (list)))"))
        );
    }
}
