use crate::error::Error;
use crate::vm::Vm;
use crate::vm::vcell::VCell;
use std::collections::HashSet;

/// Recursion / iteration depth at which `equal?` upgrades from
/// allocation-free comparison to cycle-tracking mode. Acyclic input
/// pays nothing; pathological depths or actual cycles trigger the
/// upgrade and then terminate co-inductively.
const CYCLE_DETECT_THRESHOLD: usize = 10_000;

/// Lazily-allocated visited set. While `visited` is None, comparison
/// runs without any auxiliary memory or hashing. Once a depth or
/// loop-iteration counter trips the threshold, the set is created and
/// every subsequent pair/vector comparison records `(left_ptr, right_ptr)`
/// before recursing. A repeat lookup is treated as equal, terminating
/// the recursion on cyclic structure.
struct CycleState {
    visited: Option<HashSet<(usize, usize)>>,
}

impl CycleState {
    fn new() -> Self {
        Self { visited: None }
    }

    fn upgrade(&mut self) {
        if self.visited.is_none() {
            self.visited = Some(HashSet::new());
        }
    }

    /// Returns true if (l, r) is new or tracking is inactive — the
    /// caller should proceed with the comparison. Returns false if
    /// (l, r) was already in flight, in which case the caller should
    /// treat the two as equal.
    fn check_or_insert(&mut self, l: usize, r: usize) -> bool {
        match &mut self.visited {
            None => true,
            Some(set) => set.insert((l, r)),
        }
    }
}

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
            (VCell::Number(left), VCell::Number(right)) => Ok(left == right),
            (VCell::Nil, VCell::Nil) => Ok(true),
            (VCell::Pair(_, _), VCell::Pair(_, _)) => Ok(left == right),
            (VCell::Char(left), VCell::Char(right)) => Ok(left == right),
            (VCell::String(left), VCell::String(right)) => Ok(left == right),
            _ => Ok(false),
        }
    }

    /// equal
    ///
    /// This function backs the scheme equal? predicate.
    ///
    /// When applied to pairs, vectors and strings it recursively compares them.
    /// If applied to any other type, it compares with eqv?.
    ///
    /// Acyclic comparisons run without auxiliary memory. If recursion
    /// depth or cdr-loop iteration count exceeds
    /// `CYCLE_DETECT_THRESHOLD`, a visited set is allocated and any
    /// repeat `(left_ptr, right_ptr)` comparison is treated as equal,
    /// terminating recursion on cyclic structure.
    pub fn equal(&self, left: &VCell, right: &VCell) -> Result<bool, Error> {
        let mut state = CycleState::new();
        self.equal_inner(left, right, &mut state, 0)
    }

    fn equal_inner(
        &self,
        left: &VCell,
        right: &VCell,
        state: &mut CycleState,
        depth: usize,
    ) -> Result<bool, Error> {
        if depth > CYCLE_DETECT_THRESHOLD {
            state.upgrade();
        }
        if self.eqv(left, right)? {
            return Ok(true);
        }

        let lptr = left.as_ptr().ok();
        let rptr = right.as_ptr().ok();

        let left = match left {
            VCell::Ptr(ptr) => self.heap.get_at_index(*ptr).clone(),
            _ => left.clone(),
        };
        let right = match right {
            VCell::Ptr(ptr) => self.heap.get_at_index(*ptr).clone(),
            _ => right.clone(),
        };
        if left.is_pair() && right.is_pair() {
            if let (Some(l), Some(r)) = (lptr, rptr)
                && !state.check_or_insert(l, r)
            {
                return Ok(true);
            }
            return self.compare_pair(left, right, state, depth + 1);
        }
        if left.is_vector() && right.is_vector() {
            if let (Some(l), Some(r)) = (lptr, rptr)
                && !state.check_or_insert(l, r)
            {
                return Ok(true);
            }
            return self.compare_vector(left, right, state, depth + 1);
        }
        if left.is_string() && right.is_string() {
            return Ok(left.as_string()?.borrow().as_str() == right.as_string()?.borrow().as_str());
        }
        self.eqv(&left, &right)
    }

    fn compare_pair(
        &self,
        mut left: VCell,
        mut right: VCell,
        state: &mut CycleState,
        depth: usize,
    ) -> Result<bool, Error> {
        let mut iters: usize = 0;
        loop {
            if !left.is_pair() || !right.is_pair() {
                return self.eqv(&left, &right);
            }
            let lcar = left.as_car()?;
            let rcar = right.as_car()?;
            if !self.equal_inner(&lcar, &rcar, state, depth + 1)? {
                return Ok(false);
            }
            let lcdr_ptr = left.as_cdr()?.as_ptr()?;
            let rcdr_ptr = right.as_cdr()?.as_ptr()?;
            let next_left = self.heap.get_at_index(lcdr_ptr).clone();
            let next_right = self.heap.get_at_index(rcdr_ptr).clone();
            iters += 1;
            if iters > CYCLE_DETECT_THRESHOLD {
                state.upgrade();
            }
            if next_left.is_pair()
                && next_right.is_pair()
                && !state.check_or_insert(lcdr_ptr, rcdr_ptr)
            {
                return Ok(true);
            }
            left = next_left;
            right = next_right;
        }
    }

    fn compare_vector(
        &self,
        left: VCell,
        right: VCell,
        state: &mut CycleState,
        depth: usize,
    ) -> Result<bool, Error> {
        let left = left.as_vector()?;
        let right = right.as_vector()?;
        if left.len() != right.len() {
            return Ok(false);
        }
        for idx in 0..left.len() {
            if !self.equal_inner(
                &left.get(idx).unwrap(),
                &right.get(idx).unwrap(),
                state,
                depth + 1,
            )? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

#[cfg(test)]
mod tests {}
