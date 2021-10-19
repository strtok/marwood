#[derive(Debug, Eq, PartialEq)]
pub enum Cell {
    Number(i64),
    Symbol(String),
    Cons(Box<Cell>, Box<Cell>),
    Quote(Box<Cell>),
    Nil,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq() {
        assert_eq!(Cell::Number(16), Cell::Number(16));
        assert_eq!(
            Cell::Symbol("foo".to_owned()),
            Cell::Symbol("foo".to_owned())
        );
        assert_eq!(
            Cell::Cons(
                Box::new(Cell::Symbol("foo".to_owned())),
                Box::new(Cell::Symbol("foo".to_owned()))
            ),
            Cell::Cons(
                Box::new(Cell::Symbol("foo".to_owned())),
                Box::new(Cell::Symbol("foo".to_owned()))
            )
        );
        assert_eq!(
            Cell::Quote(Box::new(Cell::Symbol("foo".to_owned()))),
            Cell::Quote(Box::new(Cell::Symbol("foo".to_owned())))
        );
        assert_eq!(Cell::Nil, Cell::Nil);
    }
}
