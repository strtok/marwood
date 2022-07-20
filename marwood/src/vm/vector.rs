use crate::vm::vcell::VCell;
use std::cell::RefCell;

#[derive(Clone, Debug, Eq, PartialEq)]
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

    pub fn put(&self, index: usize, value: VCell) {
        let mut vector = self.vector.borrow_mut();
        if index < vector.len() {
            *vector.get_mut(index).unwrap() = value;
        }
    }

    pub fn push(&self, value: VCell) {
        let mut vector = self.vector.borrow_mut();
        vector.push(value);
    }

    pub fn clone_vector(&self, start: Option<usize>, end: Option<usize>) -> Vec<VCell> {
        let v = self.vector.borrow();
        let mut start = start.unwrap_or(0);
        if start > v.len() {
            start = v.len();
        }

        let mut end = end.unwrap_or(v.len() - 1);
        if end >= v.len() {
            end = v.len() - 1;
        }

        Vec::from(&v[start..=end])
    }
}
