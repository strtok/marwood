#[derive(Debug, Eq, PartialEq)]
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
}
