use crate::vm::vcell::VCell;
use crate::vm::{Error, Vm};

impl Vm {
    /// eqv
    ///
    /// This function backs the scheme eqv? predicate, which returns
    /// true if two objects should normally be regarded as the same
    /// object. It has a slightly confusing set of rules:
    ///
    /// It returns #t if:
    /// * both symbols and are the same symbol
    /// * both boolean, and both #t or both #f
    /// * both numbers, are numerically equal and are either both inexact or both exact.
    /// * both are the empty list
    /// * both are pairs, vectors or strings that denote the same locations in the store
    /// * both are procedures whose location tags are equal
    ///
    /// It returns #f if:
    /// * both are different types
    /// * both are the same number, but one is exact and the other is inexact
    /// * both are numbers for which the = procedure returns false
    /// * one is the empty list and the other is not
    /// * both are procedures that would behave differently
    pub fn eqv(&self, left: &VCell, right: &VCell) -> Result<bool, Error> {
        // If both are the same object on the heap, they -must- be the same.
        // This covers all symbol cases, because symbols are interned in Marwood
        if left.is_ptr() && right.is_ptr() && (left == right) {
            return Ok(true);
        }

        // Deference both so that their types may be compared
        let left = match left {
            VCell::Ptr(ptr) => self.heap.get_at_index(*ptr),
            _ => left,
        };
        let right = match right {
            VCell::Ptr(ptr) => self.heap.get_at_index(*ptr),
            _ => right,
        };
        match (left, right) {
            (VCell::Bool(left), VCell::Bool(right)) => Ok(left == right),
            (VCell::FixedNum(left), VCell::FixedNum(right)) => Ok(left == right),
            (VCell::Nil, VCell::Nil) => Ok(true),
            (VCell::Pair(_, _), VCell::Pair(_, _)) => Ok(left == right),
            _ => Ok(false),
        }
    }

    /// equal
    ///
    /// This function backs the scheme equal? predicate.
    ///
    /// When applied to pairs, vectors and strings it recursively compares them.
    /// If applied to any other type, it compares with eqv?.
    pub fn equal(&self, left: &VCell, right: &VCell) -> Result<bool, Error> {
        let mut left = left.clone();
        let mut right = right.clone();
        if self.eqv(&left, &right)? {
            return Ok(true);
        }
        left = match left {
            VCell::Ptr(ptr) => self.heap.get_at_index(ptr).clone(),
            _ => left.clone(),
        };
        right = match right {
            VCell::Ptr(ptr) => self.heap.get_at_index(ptr).clone(),
            _ => right.clone(),
        };
        if left.is_pair() && right.is_pair() {
            return self.compare_pair(left, right);
        }
        if left.is_vector() && right.is_vector() {
            return self.compare_vector(left, right);
        }
        self.eqv(&left, &right)
    }

    pub fn compare_pair(&self, mut left: VCell, mut right: VCell) -> Result<bool, Error> {
        loop {
            if !left.is_pair() || !right.is_pair() {
                return self.eqv(&left, &right);
            }
            let lcar = left.as_car()?;
            let rcar = right.as_car()?;
            if !self.equal(&lcar, &rcar)? {
                return Ok(false);
            }
            left = self.heap.get(&left.as_cdr()?);
            right = self.heap.get(&right.as_cdr()?);
        }
    }

    pub fn compare_vector(&self, left: VCell, right: VCell) -> Result<bool, Error> {
        let left = left.as_vector()?;
        let right = right.as_vector()?;
        if left.len() != right.len() {
            return Ok(false);
        }
        for idx in 0..left.len() {
            if !self.equal(&left.get(idx).unwrap(), &right.get(idx).unwrap())? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

#[cfg(test)]
mod tests {}
