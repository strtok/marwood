use ::lazy_static::lazy_static;
use std::borrow::Borrow;
use std::fmt::{Debug, Display, Formatter};
use std::ops::DerefMut;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Cell {
    Bool(bool),
    Number(i64),
    Symbol(String),
    Cons(Box<Cell>, Box<Cell>),
    Nil,
}

impl Cell {
    pub fn new_symbol(val: &str) -> Cell {
        Cell::Symbol(val.into())
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
                Cell::Cons(_, next) => {
                    *next = Box::new(Cell::Cons(Box::new(cell), Box::new(Cell::Nil)));
                    tail = &mut (**next);
                }
                _ => {
                    *tail = Cell::Cons(Box::new(cell), Box::new(Cell::Nil));
                }
            }
        }

        if last_cdr.is_some() {
            if let Cell::Cons(_, ref mut cdr) = *tail.deref_mut() {
                *cdr = Box::new(last_cdr.take().unwrap());
            }
        }

        head
    }

    pub fn new_cons(car: Cell, cdr: Cell) -> Cell {
        Cell::Cons(Box::new(car), Box::new(cdr))
    }

    pub fn iter(&self) -> IntoIter {
        IntoIter { next: self }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Cell::Nil)
    }

    pub fn is_cons(&self) -> bool {
        matches!(self, Cell::Cons(_, _))
    }

    pub fn car(&self) -> Option<&Cell> {
        match self {
            Cell::Cons(car, _) => Some(car),
            _ => None,
        }
    }

    pub fn cdr(&self) -> Option<&Cell> {
        match self {
            Cell::Cons(_, cdr) => Some(cdr),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<i64> {
        match self {
            Cell::Number(val) => Some(*val),
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
        Cell::Number(val)
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

impl<'a> Iterator for IntoIter<'a> {
    type Item = &'a Cell;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next {
            Cell::Cons(car, cdr) => {
                self.next = cdr.borrow();
                Some(car.borrow())
            }
            _ => None,
        }
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

impl Iterator for Iter {
    type Item = Cell;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next.take() {
            Some(Cell::Cons(car, cdr)) => {
                self.next = Some(*cdr);
                Some(*car)
            }
            _ => None,
        }
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
        lazy_static! {
            static ref QUOTE: Cell = Cell::Symbol("quote".into());
        }

        match self {
            Cell::Cons(car, cdr) => {
                // sugar quote any list in the exact form (quote x)
                if **car == *QUOTE && (*cdr).is_cons() && (*cdr).cdr().unwrap().is_nil() {
                    write!(f, "'")?;
                    return std::fmt::Display::fmt(cdr.car().unwrap(), f);
                }
                write!(f, "(")?;
                let mut car = car;
                let mut cdr = cdr;
                loop {
                    match (*cdr).as_ref() {
                        Cell::Nil => {
                            write!(f, "{})", car)?;
                            return Ok(());
                        }
                        Cell::Cons(ncar, ncdr) => {
                            write!(f, "{} ", car)?;
                            car = ncar;
                            cdr = ncdr;
                        }
                        _ => {
                            write!(f, "{} . {})", car, cdr)?;
                            return Ok(());
                        }
                    }
                }
            }
            Cell::Bool(val) => {
                write!(f, "{}", if *val { "#t" } else { "#f" })
            }
            Cell::Number(val) => {
                write!(f, "{}", val)
            }
            Cell::Symbol(val) => {
                write!(f, "{}", val)
            }
            Cell::Nil => {
                write!(f, "()")
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
macro_rules! cons {
    () => {
        Cell::new_cons(Cell::Nil, Cell::Nil)
    };
    ($car:expr) => {
        Cell::new_cons(Cell::from($car), Cell::Nil)
    };
    ($car:expr, $cdr:expr) => {
        Cell::new_cons(Cell::from($car), Cell::from($cdr))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq() {
        assert_eq!(Cell::Number(16), Cell::Number(16));
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
        assert_eq!(cell![42], Cell::Number(42));
        assert_eq!(cell![-42], Cell::Number(-42));
        assert_eq!(
            cell![0, 1, 2],
            Cell::new_list(vec!(Cell::Number(0), Cell::Number(1), Cell::Number(2)))
        );
        assert_eq!(
            cell!["foo", 42],
            Cell::new_list(vec!(Cell::new_symbol("foo"), Cell::Number(42)))
        );
        assert_eq!(
            cell!["foo", cell![0, 1, 2]],
            Cell::new_list(vec!(
                Cell::new_symbol("foo"),
                Cell::new_list(vec!(Cell::Number(0), Cell::Number(1), Cell::Number(2)))
            ))
        );
        assert_eq!(list![], Cell::new_list(vec!()));
        assert_eq!(list!["foo"], Cell::new_list(vec!(Cell::new_symbol("foo"))));
    }

    #[test]
    fn improper_list() {
        let improper_list = Cell::new_improper_list(vec![cell![1], cell![2]].into_iter(), cell![3]);
        assert_eq!(improper_list, cons!(cell!(1), cons!(cell!(2), cell!(3))));
        assert_eq!(format!("{}", improper_list), "(1 2 . 3)");
    }

    #[test]
    fn into_iter() {
        assert_eq!(
            list![1, 2, 3].iter().cloned().collect::<Vec<Cell>>(),
            vec![cell![1], cell![2], cell![3]]
        );
        assert_eq!(cell![1].iter().cloned().collect::<Vec<Cell>>(), vec![]);
    }

    #[test]
    fn iter() {
        assert_eq!(
            list![1, 2, 3].into_iter().collect::<Vec<Cell>>(),
            vec![cell![1], cell![2], cell![3]]
        );
        assert_eq!(cell![1].into_iter().collect::<Vec<Cell>>(), vec![]);
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
        assert_eq!(
            format!("{}", list!["quote", list!["quote", "quote", "quote"]]),
            "(quote quote quote)"
        );
    }

    #[test]
    fn car_and_cdr() {
        assert_eq!(list![1, 2, 3].car(), Some(&cell![1]));
        assert_eq!(list![1, 2, 3].cdr(), Some(&list![2, 3]));
    }
}
