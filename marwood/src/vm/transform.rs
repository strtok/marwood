use crate::cell;
use crate::cell::Cell;
use crate::error::Error;
use crate::error::Error::InvalidSyntax;

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
/// Pattern represents a single syntax-rules pattern, where
/// `pattern` contains the pattern expression and `variables` is
/// the set of pattern variables encountered in the pattern.

#[derive(Debug, Eq, PartialEq)]
pub struct Pattern {
    expr: Cell,
    variables: Vec<Cell>,
    expanded_variables: Vec<Cell>,
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
            expanded_variables: vec![],
            ellipsis: ellipsis.clone(),
            literals: literals.to_vec(),
            underscore: cell!["_"],
        };

        Self::build(cdr!(expr), &mut pattern)?;
        Ok(pattern)
    }

    pub fn is_ellipsis(&self, cell: &Cell) -> bool {
        *cell == self.ellipsis
    }

    pub fn is_literal(&self, cell: &Cell) -> bool {
        self.literals.iter().any(|it| it == cell)
    }

    pub fn is_variable(&self, cell: &Cell) -> bool {
        self.variables.iter().any(|it| it == cell)
    }

    pub fn is_expanded_variable(&self, cell: &Cell) -> bool {
        self.expanded_variables.iter().any(|it| it == cell)
    }

    fn is_variable_candidate(&self, cell: &Cell) -> bool {
        cell.is_symbol()
            && !self.is_literal(cell)
            && !self.is_ellipsis(cell)
            && *cell != self.underscore
    }

    fn build(expr: &Cell, pattern: &mut Pattern) -> Result<(), Error> {
        let improper = expr.is_improper_list();
        let len = expr.len();
        let mut iter = expr.iter().enumerate().peekable();
        let mut ellipsis_ct = 0;
        while let Some((idx, it)) = iter.next() {
            let ellipsis_next = match iter.peek() {
                Some((_, cell)) => pattern.is_ellipsis(cell),
                _ => false,
            };
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
                        pattern.variables.push(it.clone())
                    } else if ellipsis_next {
                        return Err(InvalidSyntax(
                            "ellipsis must follow pattern variable".into(),
                        ));
                    }

                    if ellipsis_next {
                        Self::find_expanded_variables(it, pattern);
                    }
                }
                Cell::Pair(_, _) => {
                    if ellipsis_next {
                        Self::find_expanded_variables(it, pattern);
                    }
                    Self::build(it, pattern)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn find_expanded_variables(expr: &Cell, pattern: &mut Pattern) {
        match expr {
            Cell::Symbol(_) => {
                if pattern.is_variable_candidate(expr)
                    && !pattern.expanded_variables.iter().any(|it| it == expr)
                {
                    pattern.expanded_variables.push(expr.clone());
                }
            }
            Cell::Pair(_, _) => {
                for it in expr {
                    Self::find_expanded_variables(it, pattern);
                }
            }
            _ => {}
        }
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
        })
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
    /// * Pattern variables may not be expanded multiple times (TODO)
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
            let mut env = PatternEnvironment::new(&rule.0);
            if self.pattern_match(cdr!(&rule.0.expr), cdr!(expr), &mut env) {
                return self
                    .expand(&rule.1, &rule.0, &mut env)
                    .ok_or_else(|| InvalidSyntax(format!("{:#}", self.keyword)));
            }
        }

        return Err(InvalidSyntax(format!(
            "no matching syntax for {}",
            self.keyword
        )));
    }

    /// Pattern Match
    ///
    /// Attempt to match the input expression against one of the syntax-rules pattern,
    /// returning a pattern environment if successful.
    ///
    /// # Arguments
    /// `pattern` - The pattern to attempt to apply
    /// `expr` - The expression to match
    /// `bindings` - The set of matched variable bindings
    fn pattern_match<'a, 'b>(
        &self,
        pattern: &'a Cell,
        expr: &'a Cell,
        env: &'b mut PatternEnvironment<'a>,
    ) -> bool {
        // expr and pattern must either both be lists or improper lists
        if (pattern.is_pair() || pattern.is_nil()) && !(expr.is_pair() || expr.is_nil()) {
            return false;
        }
        if expr.is_pair() && pattern.is_pair() && (expr.is_list() != pattern.is_list()) {
            return false;
        }

        let mut expr_iter = expr.iter().peekable();
        let mut pattern_iter = pattern.iter().peekable();

        let mut expr;
        let mut pattern = &Cell::Nil;

        let mut in_ellipsis = false;

        loop {
            // Get the next expression
            // If there is no next expression, then:
            //   * If we are in an ellipsis expansion, move the pattern iterator
            //     past it so we can see if more pattern remains.
            //   * If any pattern remains, there is no match.
            //   * If pattern was exhausted, then the match is complete.
            expr = match expr_iter.next() {
                Some(expr) => expr,
                None => {
                    if in_ellipsis {
                        pattern_iter.next();
                    }
                    return match pattern_iter.next() {
                        Some(_) => {
                            if pattern_iter.peek() == Some(&&self.ellipsis) {
                                pattern_iter.next();
                                pattern_iter.peek().is_none()
                            } else {
                                false
                            }
                        }
                        None => true,
                    };
                }
            };

            // Get the next pattern.
            // * Reuse the same pattern if we're in an ellipsis expansion
            //   and based on the expression length there's more to capture.
            pattern = match in_ellipsis {
                true => {
                    if pattern_iter.len() == expr_iter.len() + 2 {
                        pattern_iter.next();
                        match pattern_iter.next() {
                            Some(pattern) => pattern,
                            None => return expr_iter.peek().is_none(),
                        }
                    } else {
                        pattern
                    }
                }
                false => match pattern_iter.next() {
                    Some(pattern) => pattern,
                    None => {
                        return false;
                    }
                },
            };

            in_ellipsis = pattern_iter.peek() == Some(&&self.ellipsis);

            match pattern {
                Cell::Symbol(_) => {
                    if self.is_literal(pattern) {
                        if pattern != expr {
                            return false;
                        }
                    } else if pattern != &cell!["_"] {
                        env.add_binding(pattern, expr);
                    }
                }
                Cell::Pair(_, _) => {
                    if !self.pattern_match(pattern, expr, env) {
                        return false;
                    }
                }
                pattern => {
                    if pattern != expr {
                        return false;
                    }
                }
            }
        }
    }

    /// Expand
    ///
    /// Given a list of bindings created from a pattern match, and a template, expand
    /// the template with the bindings.
    ///
    /// # Arguments
    /// `template` - The template to use for expansion
    /// `pattern` - The pattern associated with the template being expanded.
    /// `bindings` The matched bindings from the pattern
    fn expand(
        &self,
        template: &Cell,
        pattern: &Pattern,
        env: &mut PatternEnvironment,
    ) -> Option<Cell> {
        match template {
            Cell::Symbol(_) => {
                return if pattern.is_variable(template) {
                    env.get_binding(template).cloned()
                } else {
                    Some(template.clone())
                }
            }
            Cell::Pair(_, _) => {
                let mut v = vec![];
                let mut template_iter = template.iter().peekable();
                let mut template = template_iter.next().unwrap();

                loop {
                    let in_ellipsis = template_iter.peek() == Some(&&self.ellipsis);
                    match self.expand(template, pattern, env) {
                        Some(cell) => {
                            v.push(cell);
                            if in_ellipsis {
                                continue;
                            }
                        }
                        None => {
                            if !in_ellipsis {
                                return None;
                            }
                            template_iter.next();
                        }
                    }

                    template = match template_iter.next() {
                        Some(template) => template,
                        None => {
                            break;
                        }
                    };
                }
                Some(Cell::new_list(v))
            }
            cell => Some(cell.clone()),
        }
    }
}

/// Pattern Environment
///
/// Pattern environment is the result of a successful pattern,
/// containing all of the information needed to apply the template
/// portion of the pattern rule.
#[derive(Debug, Clone)]
struct PatternEnvironment<'a> {
    /// Bindings are pairs of matched (pattern expr)
    bindings: Vec<(&'a Cell, &'a Cell)>,

    /// A copy of the pattern, which is used to determine what
    /// type of bindings a variable is
    pattern: &'a Pattern,

    /// Position of expanded bindings (bindings that were captured
    /// and expanded with the ellipsis)
    iters: Vec<(&'a Cell, Option<usize>)>,
}

impl<'a> PatternEnvironment<'a> {
    fn new(pattern: &'a Pattern) -> PatternEnvironment<'a> {
        PatternEnvironment {
            bindings: vec![],
            pattern,
            iters: pattern
                .expanded_variables
                .iter()
                .map(|it| (it, None))
                .collect(),
        }
    }

    fn add_binding(&mut self, pattern: &'a Cell, expr: &'a Cell) {
        self.bindings.push((pattern, expr));
    }

    fn get_binding(&mut self, symbol: &Cell) -> Option<&'a Cell> {
        if !self.pattern.is_variable(symbol) {
            None
        } else if self.pattern.is_expanded_variable(symbol) {
            self.get_expanded_binding(symbol)
        } else {
            self.bindings
                .iter()
                .find(|it| it.0 == symbol)
                .map(|it| it.1)
        }
    }

    fn get_expanded_binding(&mut self, symbol: &Cell) -> Option<&'a Cell> {
        let iter = match self.iters.iter_mut().find(|it| it.0 == symbol) {
            Some((_, iter)) => iter,
            None => {
                return None;
            }
        };

        let start = match iter {
            Some(position) => *position,
            None => 0_usize,
        };

        match self.bindings[start..self.bindings.len()]
            .iter()
            .enumerate()
            .find(|it| it.1 .0 == symbol)
        {
            Some(binding) => {
                *iter = Some(start + binding.0 + 1);
                Some(binding.1 .1)
            }
            None => {
                *iter = None;
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use crate::{cell, lex};

    #[test]
    fn bad_patterns() {
        assert!(Pattern::try_new(&parse!("#t"), &cell!["..."], &vec![]).is_err());
        assert!(Pattern::try_new(&parse!("(_ ... a)"), &cell!["..."], &vec![]).is_err());
        assert!(Pattern::try_new(&parse!("(_ a . ...)"), &cell!["..."], &vec![]).is_err());
        assert!(Pattern::try_new(&parse!("(_ a a)"), &cell!["..."], &vec![]).is_err());
    }

    #[test]
    fn pattern_variable() {
        assert_eq!(
            Pattern::try_new(&parse!("(_ a b c)"), &cell!["..."], &vec![])
                .unwrap()
                .variables,
            vec![cell!["a"], cell!["b"], cell!["c"]]
        );
        assert_eq!(
            Pattern::try_new(&parse!("(_ a b . c)"), &cell!["..."], &vec![])
                .unwrap()
                .variables,
            vec![cell!["a"], cell!["b"], cell!["c"]]
        );
        assert_eq!(
            Pattern::try_new(&parse!("(_ a* ...)"), &cell!["..."], &vec![])
                .unwrap()
                .variables,
            vec![cell!["a*"]]
        );
        assert_eq!(
            Pattern::try_new(&parse!("(_ (a* b*) ...)"), &cell!["..."], &vec![])
                .unwrap()
                .variables,
            vec![cell!["a*"], cell!["b*"]]
        );
    }

    #[test]
    fn expanded_pattern_variables() {
        let pattern =
            Pattern::try_new(&parse!("(_ a (b (c ...)) ...)"), &cell!["..."], &vec![]).unwrap();
        assert_eq!(pattern.variables, vec![cell!["a"], cell!["b"], cell!["c"]]);
        assert_eq!(pattern.expanded_variables, vec![cell!["b"], cell!["c"]]);
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
        assert!(Transform::try_new(&parse!(
            r#"        
        (define-syntax begin
              (not-expected-rules ()
                [(begin exp ...)
                 ((lambda () exp ...))]))"#
        ))
        .is_err());
    }

    #[test]
    fn bad_pattern_syntax() {
        // Variable reuse
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ exp exp) ()]))
        "#
        ))
        .is_err());

        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ exp . exp) ()]))
        "#
        ))
        .is_err());

        // // nested variable reuse
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ (exp) exp) ()]))
        "#
        ))
        .is_err());
        //
        // // double ellipsis
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ foo ... bar ...) ()]))
        "#
        ))
        .is_err());

        // ellipses out of place
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ (... foo)) ()]))
        "#
        ))
        .is_err());

        // ellipsis in head position
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ (...)) ()]))
        "#
        ))
        .is_err());

        // Ellipsis in improper list tail
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ foo . ...) ()]))
        "#
        ))
        .is_err());

        // Ellipsis matching the keyword
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ ...) ()]))
        "#
        ))
        .is_err());

        // Ellipsis matching _
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ _ ...) ()]))
        "#
        ))
        .is_err());

        // Ellipsis matching a literal
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules (literal)
                [(_ literal ...) ()]))
        "#
        ))
        .is_err());
    }

    #[test]
    fn bad_template_syntax() {
        // Expansion of a non-pattern variavle
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ a ...) (b ...)]))
        "#
        ))
        .is_err());
        // Invalid ellipsis position
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ a ...) (...)]))
        "#
        ))
        .is_err());
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ a ...) (a ... ...)]))
        "#
        ))
        .is_err());
        assert!(Transform::try_new(&parse!(
            r#"
        (define-syntax bad
              (syntax-rules ()
                [(_ a ...) (a . ...)]))
        "#
        ))
        .is_err());
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
            transform.transform(&parse!("(bind-zero b)")),
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
            transform.transform(&parse!("(add-nested (10) (20))")),
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
        assert_eq!(transform.transform(&parse!("(sum)")), Ok(parse!("(+)")));
        assert_eq!(
            transform.transform(&parse!("(sum 10)")),
            Ok(parse!("(+ 10)"))
        );
        assert_eq!(
            transform.transform(&parse!("(sum 10 20)")),
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
            assert!(transform.transform(&parse!("(sum 10 20)")).is_err());
            assert_eq!(
                transform.transform(&parse!("(sum 10 20 30)")),
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
                transform.transform(&parse!("(sum 10)")),
                Ok(parse!("(+ 10)"))
            );
            assert_eq!(
                transform.transform(&parse!("(sum 10 20)")),
                Ok(parse!("(+ 10 20)"))
            );
            assert_eq!(
                transform.transform(&parse!("(sum 10 20 30)")),
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
                transform.transform(&parse!("(square 10)")),
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
                transform.transform(&parse!("(square-of-sums 10)")),
                Ok(parse!("(* (+ 10) (+ 10))"))
            );
            assert_eq!(
                transform.transform(&parse!("(square-of-sums 10 20)")),
                Ok(parse!("(* (+ 10 20) (+ 10 20))"))
            );
            assert_eq!(
                transform.transform(&parse!("(square-of-sums 10 20 30)")),
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
        assert!(transform
            .transform(&parse!("(math multiply 10 10)"))
            .is_err());
        assert_eq!(
            transform.transform(&parse!("(math add 10 20)")),
            Ok(parse!("(+ 10 20)"))
        );
        assert_eq!(
            transform.transform(&parse!("(math sub 10 20)")),
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
            transform.transform(&parse!("(sum 10 20)")),
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
        assert!(transform.transform(&parse!("(sum)")).is_err());
        assert!(transform.transform(&parse!("(sum 10)")).is_err());
        assert!(transform.transform(&parse!("(sum 10 20)")).is_err());
        assert!(transform.transform(&parse!("(sum 10 20 30 )")).is_err());
        assert_eq!(
            transform.transform(&parse!("(sum 10 20 30 40)")),
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
        assert!(transform.transform(&parse!("(sum)")).is_err());
        assert!(transform.transform(&parse!("(sum 10)")).is_err());
        assert!(transform.transform(&parse!("(sum 10 20)")).is_err());
        assert!(transform.transform(&parse!("(sum 10 20 30 )")).is_err());
        assert!(transform.transform(&parse!("(sum 10 20 30 40)")).is_err());
        assert_eq!(
            transform.transform(&parse!("(sum _ 20 _ 40)")),
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
            transform.transform(&parse!("(zip-multi (10) (10))")),
            Ok(parse!("(+ (* 10 10))"))
        );
        assert_eq!(
            transform.transform(&parse!("(zip-mult (10 20 30) (10 20 30))")),
            Ok(parse!("(+ (* 10 10) (* 20 20) (* 30 30))"))
        );
        assert_eq!(
            transform.transform(&parse!("(zip-mult (10 20 30 40) (10 20 30))")),
            Ok(parse!("(+ (* 10 10) (* 20 20) (* 30 30))"))
        );
        assert_eq!(
            transform.transform(&parse!("(zip-mult (10 20 30) (10 20 30 40))")),
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
        ));
        assert!(transform.is_ok());
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
        ));
        assert!(transform.is_ok());
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
        ));
        assert!(transform.is_ok());
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
        ));
        assert!(transform.is_ok());
    }
}
