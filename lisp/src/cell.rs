use std::borrow::Borrow;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Cell {
    Number(i64),
    Symbol(String),
    Cons(Box<Cell>, Box<Cell>),
    Quote(Box<Cell>),
    Nil,
}

impl Cell {
    pub fn symbol(val: &str) -> Cell {
        Cell::Symbol(val.to_string())
    }
    pub fn list<T: IntoIterator<Item = Cell>>(iter: T) -> Cell {
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
        head
    }

    pub fn quote(cell: Cell) -> Cell {
        Cell::Quote(Box::new(cell))
    }

    pub fn iter(&self) -> IntoIter {
        IntoIter { next: self }
    }
}

impl From<&str> for Cell {
    fn from(val: &str) -> Self {
        Cell::Symbol(val.to_string())
    }
}

impl From<i64> for Cell {
    fn from(val: i64) -> Self {
        Cell::Number(val)
    }
}

impl From<Vec<Cell>> for Cell {
    fn from(val: Vec<Cell>) -> Self {
        Cell::list(val)
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
macro_rules! list {
    () => {
        Cell::list(vec!())
    };
    ($($elt:expr),+) => {{
        let mut v = vec![];
        $(v.push(Cell::from($elt));)+
        Cell::from(v)
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq() {
        assert_eq!(Cell::Number(16), Cell::Number(16));
        assert_eq!(Cell::symbol("foo"), Cell::symbol("foo"));
        assert_eq!(
            Cell::list(vec!(Cell::symbol("foo"), Cell::symbol("bar"))),
            Cell::list(vec!(Cell::symbol("foo"), Cell::symbol("bar")))
        );
        assert_eq!(
            Cell::quote(Cell::symbol("foo")),
            Cell::quote(Cell::symbol("foo")),
        );
        assert_eq!(Cell::Nil, Cell::Nil);
    }

    #[test]
    fn cell_macro() {
        assert_eq!(cell![], Cell::Nil);
        assert_eq!(cell!["foo"], Cell::Symbol("foo".to_string()));
        assert_eq!(cell![42], Cell::Number(42));
        assert_eq!(cell![-42], Cell::Number(-42));
        assert_eq!(
            cell![0, 1, 2],
            Cell::list(vec!(Cell::Number(0), Cell::Number(1), Cell::Number(2)))
        );
        assert_eq!(
            cell!["foo", 42],
            Cell::list(vec!(Cell::symbol("foo"), Cell::Number(42)))
        );
        assert_eq!(
            cell!["foo", cell![0, 1, 2]],
            Cell::list(vec!(
                Cell::symbol("foo"),
                Cell::list(vec!(Cell::Number(0), Cell::Number(1), Cell::Number(2)))
            ))
        );
        assert_eq!(list![], Cell::list(vec!()));
        assert_eq!(list!["foo"], Cell::list(vec!(Cell::symbol("foo"))));
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
}
