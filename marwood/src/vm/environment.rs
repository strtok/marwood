use crate::cell::Cell;
use crate::vm::vcell::VCell;
use crate::vm::Error;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};

/// Given a cell, recursively collect a list of free (unbound) symbols.
///
/// A symbol is 'free' if at the time it is encountered it meets the
/// following conditions:
///
/// * It is a symbol
/// * It does not yet exist in 'env'
/// * It is not a primitive procedure (+, if, cons, etc.)
/// * It is not currently being defined (e.g. first argument of a define procedure)
///
/// While recursing, any formal arguments to lambda procedures will be added
/// to 'environment' and no longer represent free symbols.
///
/// # Arguments
/// `cell` - The cell to recurse
/// `env` - The set of symbols bound by encountered formal arguments
/// while recursing.
pub fn free_symbols<'a>(
    cell: &'a Cell,
    env: &mut HashSet<&'a Cell>,
) -> Result<HashSet<&'a Cell>, Error> {
    match cell {
        Cell::Symbol(_) => match env.contains(&cell) {
            true => Ok(HashSet::new()),
            false => Ok(HashSet::from([cell])),
        },
        Cell::Pair(car, cdr) => {
            let car = car.as_ref();
            let cdr = cdr.as_ref();
            if car.is_quote() {
                return Ok(HashSet::new());
            }
            let mut union = HashSet::new();
            if !car.is_primitive_symbol() && !env.contains(car) {
                union.insert(car);
            }

            let mut lat = match car {
                Cell::Symbol(sym) => match sym.as_str() {
                    "define" => cdr
                        .cdr()
                        .ok_or_else(|| Error::InvalidNumArgs("define".into()))?,
                    "lambda" => {
                        let mut args = cdr
                            .car()
                            .ok_or_else(|| Error::InvalidNumArgs("lambda".into()))?;
                        while args.is_pair() {
                            let sym = args.car().unwrap();
                            if !sym.is_symbol() {
                                return Err(Error::InvalidArgs(
                                    "lambda".into(),
                                    "argument".into(),
                                    sym.to_string(),
                                ));
                            }
                            env.insert(sym);
                            args = args.cdr().unwrap();
                        }
                        cdr.cdr()
                            .ok_or_else(|| Error::InvalidNumArgs("lambda".into()))?
                    }
                    _ => cdr,
                },
                _ => cdr,
            };

            while lat.is_pair() {
                union.extend(free_symbols(lat.car().unwrap(), env)?.iter());
                lat = lat.cdr().unwrap();
            }

            // improper list
            if !lat.is_nil() {
                union.extend(free_symbols(lat, env)?.iter());
            }

            Ok(union)
        }
        _ => Ok(HashSet::new()),
    }
}

/// Is Primitive Symbol
///
/// Return true if the given cell is a primitive symbol (e.g. a built-in
/// procedure)
///
/// # Arguments
/// `cell`
impl Cell {
    pub fn is_quote(&self) -> bool {
        self.is_symbol_str("quote")
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

    pub fn is_primitive_symbol(&self) -> bool {
        lazy_static! {
            static ref PRIMITIVE_SYMBOLS: HashSet<&'static str> = HashSet::from([
                "car", "cdr", "cons", "define", "eq?", "eqv?", "lambda", "quote", "+", "-", "*"
            ]);
        }
        match self {
            Cell::Symbol(sym) => PRIMITIVE_SYMBOLS.contains(sym.as_str()),
            _ => false,
        }
    }
}

/// Environment
///
/// Environment represents a binding of a symbol to a value in the heap.
/// The environment tracks both deep bindings (sym -> slot), and also a
/// vector of shallow bindings (slot -> vcell).
#[derive(Debug)]
pub struct Environment {
    /// Deep bindings is a map of symbol ptr -> slot, and
    /// is used by the compiler to aassociate a symbol at compilation
    /// time with the environment slot.
    bindings: HashMap<usize, usize>,

    /// Environment slots. The compiler produces shallow bindings as
    /// ptr into this vector at compile time.
    slots: Vec<VCell>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            bindings: HashMap::new(),
            slots: vec![],
        }
    }

    pub fn iter_bindings(&self) -> std::collections::hash_map::Keys<usize, usize> {
        self.bindings.keys()
    }

    pub fn iter_slots(&self) -> std::slice::Iter<VCell> {
        self.slots.iter()
    }

    /// Get binding
    ///
    /// Get binding provides a deep binding lookup of sym -> slot. If the
    /// binding does not already exist, get_binding() will crates a new
    /// binding and return the newly bound slot.
    ///
    /// # Arguments
    /// `sym` - The symbol to provide a binding for
    pub fn get_binding<T: Into<usize>>(&mut self, sym: T) -> usize {
        let sym: usize = sym.into();
        match self.bindings.get(&sym) {
            Some(slot) => *slot,
            None => {
                self.slots.push(VCell::undefined());
                let slot = self.slots.len() - 1;
                self.bindings.insert(sym, slot);
                slot
            }
        }
    }

    /// Get Symbol
    ///
    /// Get the symbol bound to an environment slot. This is a reverse lookup
    /// of a binding created with get_binding().
    ///
    /// # Arguments
    /// `slot` - The slot to find the symbol for.
    pub fn get_symbol<T: Into<usize>>(&self, slot: T) -> Option<usize> {
        let slot = slot.into();
        self.bindings
            .iter()
            .find(|it| *(it.1) == slot)
            .map(|it| *it.0)
    }

    /// Get slot
    ///
    /// Get slot provides a shallow binding interface. Given the environment
    /// slot, return the bound value.
    ///
    /// Any slot accessed by the runtime must have been valid at compilation
    /// time, so any reference to an unknown slot is considered a critical error
    /// and will cause a panic of the runtime.
    ///
    /// # Arguments
    /// `slot` - The slot to return a value for
    pub fn get_slot(&self, slot: usize) -> VCell {
        self.slots
            .get(slot)
            .expect("invalid environment slot")
            .clone()
    }

    /// Put slot
    ///
    /// Get slot provides a shallow binding interface. Given the environment
    /// slot, return the bound value.
    ///
    /// Any slot accessed by the runtime must have been valid at compilation
    /// time, so any reference to an unknown slot is considered a critical error
    /// and will cause a panic of the runtime.
    ///
    /// Any value other than a ptr or undefined is considered a runtime error
    /// and will result in a panic.
    ///
    /// # Arguments
    /// `slot` - The slot to return a value for
    pub fn put_slot(&mut self, slot: usize, vcell: VCell) {
        assert!(
            matches!(vcell, VCell::Ptr(_) | VCell::Undefined),
            "unexpected put_slot() of {:?}",
            vcell
        );
        *self.slots.get_mut(slot).expect("invalid environment slot") = vcell;
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{cell, lex, parse};

    #[test]
    fn same_binding_same_slot() {
        let mut env = Environment::new();
        assert_eq!(env.get_binding(50_usize), 0);
        assert_eq!(env.get_binding(100_usize), 1);
        assert_eq!(env.get_binding(50_usize), 0);
        assert_eq!(env.get_symbol(0_usize), Some(50_usize));
        assert_eq!(env.get_symbol(1_usize), Some(100_usize));
        assert_eq!(env.get_slot(0), VCell::undefined());
        env.put_slot(0, VCell::ptr(42));
        assert_eq!(env.get_slot(0), VCell::ptr(42));
        env.put_slot(0, VCell::undefined());
    }

    #[test]
    #[should_panic]
    fn put_slot_panics_if_non_ptr() {
        let mut env = Environment::new();
        assert_eq!(env.get_binding(50_usize), 0);
        env.put_slot(0, VCell::nil());
    }

    #[test]
    fn primitive_symbol_check() {
        assert!(cell!["+"].is_primitive_symbol());
        assert!(!cell!["foo"].is_primitive_symbol());
        assert!(!cell![100].is_primitive_symbol());
    }

    #[test]
    fn free_syms() {
        // Single symbol not in the environment
        assert_eq!(
            free_symbols(&parse!["a"], &mut HashSet::new()),
            Ok(HashSet::from([&cell!["a"]]))
        );

        // A single symbol in the environment
        assert_eq!(
            free_symbols(&parse!["a"], &mut HashSet::from([&cell!["a"]])),
            Ok(HashSet::new())
        );

        // Atom
        assert_eq!(
            free_symbols(&parse!["42"], &mut HashSet::new()),
            Ok(HashSet::new())
        );
        assert_eq!(
            free_symbols(&parse!["#t"], &mut HashSet::new()),
            Ok(HashSet::new())
        );

        // Any quoted symbols are completely ignored
        assert_eq!(
            free_symbols(&parse!["(quote (a b c))"], &mut HashSet::new()),
            Ok(HashSet::new())
        );

        // procedure
        assert_eq!(
            free_symbols(&parse!["(a b c)"], &mut HashSet::new()),
            Ok(HashSet::from([&cell!["a"], &cell!["b"], &cell!["c"]]))
        );

        // Nested primitive procedures
        assert_eq!(
            free_symbols(&parse!["(+ (* a b) (* c d) e)"], &mut HashSet::new()),
            Ok(HashSet::from([
                &cell!["a"],
                &cell!["b"],
                &cell!["c"],
                &cell!["d"],
                &cell!["e"]
            ]))
        );

        // Improper list
        assert_eq!(
            free_symbols(&parse!["(a b . c)"], &mut HashSet::new()),
            Ok(HashSet::from([&cell!["a"], &cell!["b"], &cell!["c"]]))
        );

        assert_eq!(
            free_symbols(&parse!["(a b . c)"], &mut HashSet::from([&cell!["c"]])),
            Ok(HashSet::from([&cell!["a"], &cell!["b"]]))
        );

        // Define
        assert_eq!(
            free_symbols(&parse!["(define a b)"], &mut HashSet::new()),
            Ok(HashSet::from([&cell!["b"]]))
        );

        // lambdas
        assert_eq!(
            free_symbols(&parse!["(lambda (x) (+ x y) z)"], &mut HashSet::new()),
            Ok(HashSet::from([&cell!["y"], &cell!["z"]]))
        );
        assert!(free_symbols(&parse!["(lambda)"], &mut HashSet::new()).is_err());
        assert!(free_symbols(&parse!["(lambda (10) (+ x y))"], &mut HashSet::new()).is_err());
        assert!(free_symbols(&parse!["(lambda (a 10) (+ x y))"], &mut HashSet::new()).is_err());
    }

    #[test]
    fn free_syms_returns_errors() {
        assert!(free_symbols(&parse!["(define)"], &mut HashSet::new()).is_err());
    }
}
