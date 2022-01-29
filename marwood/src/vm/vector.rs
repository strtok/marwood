use crate::vm::vcell::VCell;
use std::cell::RefCell;

#[derive(Debug, Eq, PartialEq)]
pub struct Vector {
    vector: RefCell<Vec<VCell>>,
}

impl Vector {
    pub fn new(vector: Vec<VCell>) -> Vector {
        Vector {
            vector: RefCell::new(vector),
        }
    }

    pub fn len(&self) -> usize {
        self.vector.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, index: usize) -> Option<VCell> {
        self.vector.borrow().get(index).cloned()
    }
}
