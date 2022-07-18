use crate::char::write_escaped_char;
use crate::number::Number;
use ::lazy_static::lazy_static;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use std::ops::DerefMut;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Cell {
    Bool(bool),
    Char(char),
    Nil,
    Number(Number),
    Pair(Box<Cell>, Box<Cell>),
    String(String),
    Symbol(String),
    Vector(Vec<Cell>),

    // Types that exist in VCell, but need Cell representation for
    // printing purposes. These are never created by the lexer/parser.
    Continuation,
    Macro,
    Procedure(Option<String>),
    Undefined,
    Void,
}

impl Cell {
    pub fn new_symbol(val: &str) -> Cell {
        Cell::Symbol(val.into())
    }

    pub fn new_string(val: &str) -> Cell {
        Cell::String(val.into())
    }

    pub fn new_list<T: IntoIterator<Item = Cell>>(iter: T) -> Cell {
        Cell::construct_list(iter, None)
    }

    pub fn new_improper_list<T: IntoIterator<Item = Cell>>(iter: T, cdr: Cell) -> Cell {
        Cell::construct_list(iter, Some(cdr))
    }

    /// Construct List
    ///
    /// This constructs a list composed of cons cells, where each value of the list
    /// is stored in car, and the remainder of the list is stored in cdr. The very
    /// last cons cell's cdr is set to '() (i.e. Cell::Nil).
    ///
    ///```text
    /// [car][cdr]
    ///        `--[car][cdr]
    ///                  `---[car][nil]
    /// ```
    /// If `last_cdr` is Some, then the very last cell is set to the value of last_cdr
    /// instead of '():
    ///
    ///```text
    /// [car][cdr]
    ///        `--[car][cdr]
    ///                  `---[car][last_cdr]
    /// ```
    /// An improper list uses the dotted notation form, for example: `(1 2 3 . 4)`
    ///
    /// # Arguments
    /// `iter` - An iterator over Cell used to construct the list
    /// `last_cdr` - If Some(cell), then set the last cell in the list's cdr to last_cdr
    ///              instead of Cell::Nil
    fn construct_list<T: IntoIterator<Item = Cell>>(iter: T, mut last_cdr: Option<Cell>) -> Cell {
        let mut head = Cell::Nil;
        let mut tail = &mut head;
        for cell in iter {
            match tail {
                Cell::Pair(_, next) => {
                    *next = Box::new(Cell::Pair(Box::new(cell), Box::new(Cell::Nil)));
                    tail = &mut (**next);
                }
                _ => {
                    *tail = Cell::Pair(Box::new(cell), Box::new(Cell::Nil));
                }
            }
        }

        if last_cdr.is_some() {
            if let Cell::Pair(_, ref mut cdr) = *tail.deref_mut() {
                *cdr = Box::new(last_cdr.take().unwrap());
            }
        }

        head
    }

    pub fn new_pair(car: Cell, cdr: Cell) -> Cell {
        Cell::Pair(Box::new(car), Box::new(cdr))
    }

    pub fn iter(&self) -> IntoIter {
        IntoIter { next: self }
    }

    pub fn iter_improper(&self) -> IntoIter {
        IntoIter { next: self }
    }

    pub fn collect_vec(&self) -> Vec<&Cell> {
        self.iter().collect::<Vec<_>>()
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Cell::Nil)
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, Cell::Pair(_, _))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, Cell::Symbol(_))
    }

    pub fn is_list(&self) -> bool {
        if self.is_pair() {
            let mut rest = self.cdr().unwrap();
            loop {
                if !rest.is_pair() {
                    return rest.is_nil();
                } else {
                    rest = rest.cdr().unwrap();
                }
            }
        } else {
            false
        }
    }

    pub fn is_improper_list(&self) -> bool {
        if self.is_pair() {
            let mut rest = self.cdr().unwrap();
            loop {
                if !rest.is_pair() {
                    return !rest.is_nil();
                } else {
                    rest = rest.cdr().unwrap();
                }
            }
        } else {
            false
        }
    }

    pub fn len(&self) -> usize {
        self.iter().count()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_quote(&self) -> bool {
        self.is_symbol_str("quote")
    }

    pub fn is_quasiquote(&self) -> bool {
        self.is_symbol_str("quasiquote")
    }

    pub fn is_unquote(&self) -> bool {
        self.is_symbol_str("unquote")
    }

    pub fn is_define(&self) -> bool {
        self.is_symbol_str("define")
    }

    pub fn is_lambda(&self) -> bool {
        self.is_symbol_str("lambda")
    }

    pub fn is_symbol_str(&self, s: &'static str) -> bool {
        match self.as_symbol() {
            Some(sym) => sym == s,
            _ => false,
        }
    }

    /// Is Primitive Symbol
    ///
    /// Return true if the given cell is a primitive symbol (e.g. a built-in
    /// procedure)
    ///
    /// # Arguments
    /// `cell`
    pub fn is_primitive_symbol(&self) -> bool {
        lazy_static! {
            static ref PRIMITIVE_SYMBOLS: HashSet<&'static str> = HashSet::from([
                "define",
                "lambda",
                "if",
                "quasiquote",
                "quote",
                "set!",
                "unquote"
            ]);
        }
        match self {
            Cell::Symbol(sym) => PRIMITIVE_SYMBOLS.contains(sym.as_str()),
            _ => false,
        }
    }

    pub fn car(&self) -> Option<&Cell> {
        match self {
            Cell::Pair(car, _) => Some(car),
            _ => None,
        }
    }

    pub fn cdr(&self) -> Option<&Cell> {
        match self {
            Cell::Pair(_, cdr) => Some(cdr),
            _ => None,
        }
    }

    pub fn cadr(&self) -> Option<&Cell> {
        match self.cdr() {
            Some(cell) => cell.car(),
            None => None,
        }
    }

    pub fn cddr(&self) -> Option<&Cell> {
        match self.cdr() {
            Some(cell) => cell.cdr(),
            None => None,
        }
    }

    pub fn as_number(&self) -> Option<Number> {
        match self {
            Cell::Number(val) => Some(val.clone()),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Cell::Symbol(val) => Some(val),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Cell::Bool(val) => Some(*val),
            _ => None,
        }
    }
}

impl From<bool> for Cell {
    fn from(val: bool) -> Self {
        Cell::Bool(val)
    }
}

impl From<&str> for Cell {
    fn from(val: &str) -> Self {
        Cell::Symbol(val.into())
    }
}

impl From<i64> for Cell {
    fn from(val: i64) -> Self {
        Cell::Number(Number::Fixnum(val))
    }
}

impl From<char> for Cell {
    fn from(val: char) -> Self {
        Cell::Char(val)
    }
}

impl From<Vec<Cell>> for Cell {
    fn from(val: Vec<Cell>) -> Self {
        Cell::new_list(val)
    }
}

pub struct IntoIter<'a> {
    next: &'a Cell,
}

impl<'a> ExactSizeIterator for IntoIter<'a> {}

impl<'a> Iterator for IntoIter<'a> {
    type Item = &'a Cell;

    fn next(&mut self) -> Option<Self::Item> {
        const NIL: Cell = Cell::Nil;
        match self.next {
            Cell::Pair(car, cdr) => {
                self.next = cdr.borrow();
                Some(car.borrow())
            }
            Cell::Nil => None,
            _ => {
                let cell = self.next;
                self.next = &NIL;
                Some(cell)
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.next.len();
        (len, Some(len))
    }
}

impl<'a> IntoIterator for &'a Cell {
    type Item = &'a Cell;
    type IntoIter = IntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { next: self }
    }
}

pub struct Iter {
    next: Option<Cell>,
}

impl ExactSizeIterator for Iter {}

impl Iterator for Iter {
    type Item = Cell;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next.take() {
            Some(Cell::Pair(car, cdr)) => {
                self.next = Some(*cdr);
                Some(*car)
            }
            Some(Cell::Nil) => None,
            cell => {
                self.next = Some(Cell::Nil);
                cell
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = match &self.next {
            Some(cell) => cell.len(),
            None => 0,
        };
        (len, Some(len))
    }
}

impl IntoIterator for Cell {
    type Item = Cell;
    type IntoIter = Iter;

    fn into_iter(self) -> Self::IntoIter {
        Iter { next: Some(self) }
    }
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Cell::Pair(car, cdr) => {
                // sugar quote any list in the exact form (quote x)
                if car.is_quote() && (*cdr).is_pair() && (*cdr).cdr().unwrap().is_nil() {
                    write!(f, "'")?;
                    return std::fmt::Display::fmt(cdr.car().unwrap(), f);
                }
                write!(f, "(")?;
                let mut car = car;
                let mut cdr = cdr;
                loop {
                    match (*cdr).as_ref() {
                        Cell::Nil => {
                            if f.alternate() {
                                write!(f, "{:#})", car)?;
                            } else {
                                write!(f, "{})", car)?;
                            }
                            return Ok(());
                        }
                        Cell::Pair(ncar, ncdr) => {
                            if f.alternate() {
                                write!(f, "{:#} ", car)?;
                            } else {
                                write!(f, "{} ", car)?;
                            }
                            car = ncar;
                            cdr = ncdr;
                        }
                        _ => {
                            if f.alternate() {
                                write!(f, "{:#} . {:#})", car, cdr)?;
                            } else {
                                write!(f, "{} . {})", car, cdr)?;
                            }
                            return Ok(());
                        }
                    }
                }
            }
            Cell::Bool(val) => {
                write!(f, "{}", if *val { "#t" } else { "#f" })
            }
            Cell::Char(c) => match f.alternate() {
                false => write!(f, "{}", c),
                true => write_escaped_char(*c, f),
            },
            Cell::Number(val) => {
                write!(f, "{}", val)
            }
            Cell::String(val) => match f.alternate() {
                false => write!(f, "{}", val),
                true => {
                    write!(f, "\"")?;
                    for it in val.chars() {
                        match it {
                            '"' | '\\' => write!(f, "\\{}", it)?,
                            '\t' => write!(f, "\\t")?,
                            '\n' => write!(f, "\\n")?,
                            '\r' => write!(f, "\\r")?,
                            _ if it as u32 == 0x1b => write!(f, "\\e")?,
                            _ if it as u32 == 0x7 => write!(f, "\\a")?,
                            _ if it as u32 == 0x8 => write!(f, "\\b")?,
                            _ if it as u32 == 0xb => write!(f, "\\v")?,
                            _ if it as u32 == 0xc => write!(f, "\\f")?,
                            _ if it.is_control() => write!(f, "\\x{:x};", it as u32)?,
                            it => write!(f, "{}", it)?,
                        };
                    }
                    write!(f, "\"")
                }
            },
            Cell::Symbol(val) => {
                write!(f, "{}", val)
            }
            Cell::Nil => {
                write!(f, "()")
            }
            Cell::Vector(vector) => {
                write!(f, "#(")?;
                for (idx, cell) in vector.iter().enumerate() {
                    if idx == vector.len() - 1 {
                        if f.alternate() {
                            write!(f, "{:#}", cell)?;
                        } else {
                            write!(f, "{}", cell)?;
                        }
                    } else if f.alternate() {
                        write!(f, "{:#} ", cell)?;
                    } else {
                        write!(f, "{} ", cell)?;
                    }
                }
                write!(f, ")")
            }
            Cell::Continuation => {
                write!(f, "#<continuation>")
            }
            Cell::Macro => {
                write!(f, "#<macro>")
            }
            Cell::Procedure(desc) => match desc {
                Some(desc) => {
                    write!(f, "#<procedure:{}>", desc)
                }
                None => {
                    write!(f, "#<procedure>")
                }
            },
            Cell::Undefined => {
                write!(f, "#<undefined>")
            }
            Cell::Void => {
                write!(f, "#<void>")
            }
        }
    }
}

#[macro_export]
macro_rules! cell {
    () => {
        Cell::Nil
    };
    ($elt:expr) => {
        Cell::from($elt)
    };
    ($($elt:expr),+) => {{
        let mut v = vec![];
        $(v.push(Cell::from($elt));)+
        Cell::from(v)
    }};
}

#[macro_export]
macro_rules! void {
    () => {
        Cell::Void
    };
}

#[macro_export]
macro_rules! cons {
    () => {
        Cell::new_pair(Cell::Nil, Cell::Nil)
    };
    ($car:expr) => {
        Cell::new_pair(Cell::from($car), Cell::Nil)
    };
    ($car:expr, $cdr:expr) => {
        Cell::new_pair(Cell::from($car), Cell::from($cdr))
    };
}

#[macro_export]
macro_rules! list {
    () => {
        Cell::new_list(vec!())
    };
    ($($elt:expr),+) => {{
        let v = vec![$(Cell::from($elt),)+];
        Cell::from(v)
    }};
}

#[macro_export]
macro_rules! vector {
    () => {
        Cell::Vector(vec![])
    };
    ($($elt:expr),+) => {{
        let v = vec![$(Cell::from($elt),)+];
        Cell::Vector(v)
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq() {
        assert_eq!(
            Cell::Number(Number::Fixnum(16)),
            Cell::Number(Number::Fixnum(16))
        );
        assert_eq!(Cell::new_symbol("foo"), Cell::new_symbol("foo"));
        assert_eq!(
            Cell::new_list(vec!(Cell::new_symbol("foo"), Cell::new_symbol("bar"))),
            Cell::new_list(vec!(Cell::new_symbol("foo"), Cell::new_symbol("bar")))
        );
        assert_eq!(Cell::Nil, Cell::Nil);
    }

    #[test]
    fn cell_macro() {
        assert_eq!(cell![], Cell::Nil);
        assert_eq!(cell!["foo"], Cell::Symbol("foo".into()));
        assert_eq!(cell![42], Cell::Number(Number::Fixnum(42)));
        assert_eq!(cell![-42], Cell::Number(Number::Fixnum(-42)));
        assert_eq!(
            cell![0, 1, 2],
            Cell::new_list(vec!(
                Cell::Number(Number::Fixnum(0)),
                Cell::Number(Number::Fixnum(1)),
                Cell::Number(Number::Fixnum(2))
            ))
        );
        assert_eq!(
            cell!["foo", 42],
            Cell::new_list(vec!(
                Cell::new_symbol("foo"),
                Cell::Number(Number::Fixnum(42))
            ))
        );
        assert_eq!(
            cell!["foo", cell![0, 1, 2]],
            Cell::new_list(vec!(
                Cell::new_symbol("foo"),
                Cell::new_list(vec!(
                    Cell::Number(Number::Fixnum(0)),
                    Cell::Number(Number::Fixnum(1)),
                    Cell::Number(Number::Fixnum(2))
                ))
            ))
        );
        assert_eq!(list![], Cell::new_list(vec!()));
        assert_eq!(list!["foo"], Cell::new_list(vec!(Cell::new_symbol("foo"))));
    }

    #[test]
    fn vector_macr() {
        assert_eq!(
            vector![1, 2, 3],
            Cell::Vector(vec![cell![1], cell![2], cell![3]])
        );
    }

    #[test]
    fn proper_list() {
        assert!(list![1, 2, 3].is_list());
    }

    #[test]
    fn improper_list() {
        let improper_list = Cell::new_improper_list(vec![cell![1], cell![2]].into_iter(), cell![3]);
        assert!(!improper_list.is_list());
        assert!(improper_list.is_improper_list());
        assert_eq!(improper_list, cons!(cell!(1), cons!(cell!(2), cell!(3))));
        assert_eq!(format!("{}", improper_list), "(1 2 . 3)");
    }

    #[test]
    fn iter() {
        assert_eq!(
            list![1, 2, 3].iter().cloned().collect::<Vec<Cell>>(),
            vec![cell![1], cell![2], cell![3]]
        );
        assert_eq!(
            cell![1].iter().cloned().collect::<Vec<Cell>>(),
            vec![cell![1]]
        );
    }

    #[test]
    fn into_iter() {
        assert_eq!(
            list![1, 2, 3].into_iter().collect::<Vec<Cell>>(),
            vec![cell![1], cell![2], cell![3]]
        );
        assert_eq!(cell![1].into_iter().collect::<Vec<Cell>>(), vec![cell![1]]);
    }

    #[test]
    fn iter_improper() {
        let improper_list = Cell::new_improper_list(vec![cell![1], cell![2]].into_iter(), cell![3]);
        assert_eq!(
            improper_list.iter_improper().collect::<Vec<&Cell>>(),
            vec![&cell![1], &cell![2], &cell![3]]
        );
    }

    #[test]
    fn flatten() {
        assert_eq!(
            list![list![1, 2, 3], list![4, 5, 6]]
                .into_iter()
                .flatten()
                .collect::<Vec<Cell>>(),
            vec![cell![1], cell![2], cell![3], cell![4], cell![5], cell![6]]
        );
    }

    #[test]
    fn display() {
        assert_eq!(format!("{}", Cell::Nil), "()");
        assert_eq!(format!("{}", cell![true]), "#t");
        assert_eq!(format!("{}", cell![false]), "#f");
        assert_eq!(format!("{}", cell![42]), "42");
        assert_eq!(format!("{}", cell!["foo"]), "foo");
        assert_eq!(format!("{}", list![1, 2, 3]), "(1 2 3)");
        assert_eq!(
            format!("{}", list![1, 2, 3, list![5, 6, 7]]),
            "(1 2 3 (5 6 7))"
        );
        assert_eq!(format!("{}", cons!("foo")), "(foo)");
        assert_eq!(format!("{}", cons!("foo", "bar")), "(foo . bar)");
        assert_eq!(format!("{}", cons!(1, cons!(2, 3))), "(1 2 . 3)");
        assert_eq!(format!("{}", cons!(cell!(), 42)), "(() . 42)");
        assert_eq!(format!("{}", list!["quote", list![1, 2]]), "'(1 2)");
        assert_eq!(
            format!("{}", list!["quote", cons!["quote", 1]]),
            "'(quote . 1)"
        );
    }

    #[test]
    fn display_quote() {
        assert_eq!(format!("{}", list!["quote", list![1, 2]]), "'(1 2)");
        assert_eq!(
            format!("{}", list!["quote", cons!["quote", 1]]),
            "'(quote . 1)"
        );
        assert_eq!(format!("{}", list!["quote"]), "(quote)");
        assert_eq!(format!("{}", list!["quote", "quote"]), "'quote");
        assert_eq!(
            format!("{}", list!["quote", "quote", "quote"]),
            "(quote quote quote)"
        );
    }

    #[test]
    fn car_and_cdr() {
        assert_eq!(list![1, 2, 3].car(), Some(&cell![1]));
        assert_eq!(list![1, 2, 3].cdr(), Some(&list![2, 3]));
    }
}
