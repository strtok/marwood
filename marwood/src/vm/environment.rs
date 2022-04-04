use crate::cell::Cell;
use crate::vm::lambda::Lambda;
use crate::vm::vcell::VCell;
use crate::vm::Error;
use crate::vm::Error::InvalidDefineSyntax;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

/// Environment
///
/// Marwood's environment is composed of two different types of environments:
///
/// * The global environment, represented by a single instance of GlobalEnvironment
///   on the VM.
/// * Lexical environments, represented by the LexicalEnvironment structure. Each
///   procedure in Marwood that is a closure contains a lexical environment represented
///   by LexicalEnvironment. This environment is created by the CLOSURE instruction, which
///   represents the point in time the (lambda ...) expression is evaluated.
///
/// These data structures differ slightly in that the global environment containns
/// the notion of a "deep binding" represented by a HashMap along with shallow bindings,
/// and LexicalEnvironment only has the notion of shallow bindings because lexical bindings
/// are known at compile time.

/// Binding Source
///
/// Binding is used to specify the source of a binding in a lexical environment.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BindingSource {
    /// The symbol is not lexically bound, and should use the global
    /// environment
    Global,

    /// The binding is derived from the lambda's own argument
    Argument(usize),

    /// The binding is inherited from the IOF's Nth argument
    IofArgument(usize),

    /// The binding is inherited from the Nth slot of the IOF's
    /// environment
    IofEnvironment(usize),

    /// The binding is an internal definition (i.e. define in the body)
    ///
    /// For example, in `(lambda (x) (define y 10) (+ x y))`, y is an
    /// internal definition.
    InternalDefinition,
}

/// Binding Location
///
/// Binding is used to specify the location of a binding in a lexical environment.
/// It is used by the compiler during symbol lookup to determine whether or not a
/// symbol is in the lexical environment, global, or should be fetched directly from
/// the stack.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BindingLocation {
    Argument(usize),
    Global,
    Environment(usize),
}

/// Environment Map
///
/// An environment map is a set of instructions on how to build a
/// lexical environment given an IOF's environment and formal arguments.
///
/// This map is used in two places:
///
/// * During compilation when evaluating a symbol to determine if it
///   references a local argument, the lexical environment, or the
///   global environment.
///
/// * During creation of a procedure as a result of runtime execution
///   of (lambda ...)
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EnvironmentMap {
    /// Map
    ///
    /// A vector of sym->source mapping, given a location in the IOF
    /// for a given symbol.
    map: Vec<(VCell, BindingSource)>,
}

impl EnvironmentMap {
    pub fn new() -> EnvironmentMap {
        EnvironmentMap { map: vec![] }
    }

    /// New From IOF
    ///
    /// Create a new lambda, and populate its environment map
    /// given the IOF and set of free symbols.
    pub fn new_from_iof(
        args: &[VCell],
        internally_defined: &[VCell],
        iof: &Lambda,
        free_symbols: &[VCell],
    ) -> EnvironmentMap {
        let mut map = args
            .iter()
            .enumerate()
            .map(|it| (it.1.clone(), BindingSource::Argument(it.0)))
            .collect::<Vec<(VCell, BindingSource)>>();

        internally_defined
            .iter()
            .map(|it| (it.clone(), BindingSource::InternalDefinition))
            .for_each(|binding| map.push(binding));

        free_symbols
            .iter()
            .filter_map(|sym| {
                if let Some(slot) = iof.envmap.get_slot(sym) {
                    Some((sym.clone(), BindingSource::IofEnvironment(slot)))
                } else if let Some((n, _)) = iof.args.iter().enumerate().find(|(_, it)| *it == sym)
                {
                    Some((sym.clone(), BindingSource::IofArgument(n)))
                } else {
                    None
                }
            })
            .for_each(|binding| map.push(binding));
        EnvironmentMap { map }
    }

    /// Get Slot
    ///
    /// Given a symbol, find the slot in the Environment
    pub fn get_slot(&self, sym: &VCell) -> Option<usize> {
        self.map
            .iter()
            .enumerate()
            .find(|it| it.1 .0 == *sym)
            .map(|it| it.0)
    }

    /// Slots Len
    ///
    /// Return the number of slots in the map. This corresponds to how
    /// many slots a LexicalEnvironment will have.
    pub fn slots_len(&self) -> usize {
        self.map.len()
    }

    /// Map
    ///
    /// Return an iterable version of the map for debugging purposes.
    pub fn get_map(&self) -> &[(VCell, BindingSource)] {
        &self.map
    }
}

impl Default for EnvironmentMap {
    fn default() -> Self {
        EnvironmentMap::new()
    }
}

/// Lexical Environment
///
/// LexicalEnvironment represents a series of shallow binding slots for
/// a procedure of which are populated during the evaluation of the lambda
/// procedure and during procedure application.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LexicalEnvironment {
    slots: RefCell<Vec<VCell>>,
}

impl LexicalEnvironment {
    pub fn new(size: usize) -> LexicalEnvironment {
        LexicalEnvironment {
            slots: RefCell::new(vec![VCell::undefined(); size]),
        }
    }

    pub fn slot_len(&self) -> usize {
        self.slots.borrow().len()
    }

    pub fn get(&self, index: usize) -> VCell {
        self.slots
            .borrow()
            .get(index)
            .expect("slot index out of bounds")
            .clone()
    }

    pub fn put(&self, index: usize, vcell: VCell) {
        *self
            .slots
            .borrow_mut()
            .get_mut(index)
            .expect("slot index out of bounds") = vcell;
    }
}

/// Global Environment
///
/// GlobalEnvironment represents a binding of a symbol to a value in the heap.
/// The environment tracks both deep bindings (sym -> slot), and also a
/// vector of shallow bindings (slot -> vcell).
#[derive(Debug, PartialEq, Eq)]
pub struct GlobalEnvironment {
    /// Deep bindings is a map of symbol ptr -> slot, and
    /// is used by the compiler to aassociate a symbol at compilation
    /// time with the environment slot.
    bindings: HashMap<usize, usize>,

    /// Environment slots. The compiler produces shallow bindings as
    /// ptr into this vector at compile time.
    slots: Vec<VCell>,
}

impl GlobalEnvironment {
    pub fn new() -> GlobalEnvironment {
        GlobalEnvironment {
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

    /// Get
    ///
    /// Given a symbol symbol reference, return the object bound
    /// to the symbole or None. This provides a deep binding lookup
    /// by symbol.
    ///
    /// # Arguments
    /// `sym` - The symbol to provide a binding for
    pub fn get<T: Into<usize>>(&mut self, sym: T) -> Option<VCell> {
        let sym: usize = sym.into();
        self.bindings.get(&sym).map(|slot| self.get_slot(*slot))
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

impl Default for GlobalEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

/// Free Symbols
///
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
pub fn free_symbols(cell: &Cell) -> Result<HashSet<&Cell>, Error> {
    let mut env = HashSet::new();
    let mut free_set = HashSet::new();
    find_free_symbols(cell, &mut env, &mut free_set)?;
    Ok(free_set)
}

/// Find Free Symbols
///
/// This is a recursive call from free_symbols.
fn find_free_symbols<'a>(
    cell: &'a Cell,
    env: &mut HashSet<&'a Cell>,
    free: &mut HashSet<&'a Cell>,
) -> Result<(), Error> {
    match cell {
        Cell::Symbol(_) => match env.contains(&cell) {
            true => Ok(()),
            false => {
                free.insert(cell);
                Ok(())
            }
        },
        Cell::Pair(car, cdr) => {
            let car = car.as_ref();
            let cdr = cdr.as_ref();
            let mut env = env.clone();
            find_free_symbols_in_proc((car, cdr), &mut env, free)?;
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Find Free Symbols In Proc
///
/// This is a recursive call from free_syumbols.
fn find_free_symbols_in_proc<'a>(
    (car, cdr): (&'a Cell, &'a Cell),
    env: &mut HashSet<&'a Cell>,
    free: &mut HashSet<&'a Cell>,
) -> Result<(), Error> {
    if car.is_quote() {
        return Ok(());
    }

    if car.is_symbol() && !car.is_primitive_symbol() && !env.contains(car) {
        free.insert(car);
    }

    // This is a special case where the procedure being applied is a procedure
    // e.g. ((proc) 1 2)
    if car.is_pair() {
        find_free_symbols(car, env, free)?;
    }

    let mut rest = match car {
        Cell::Symbol(sym) => match sym.as_str() {
            "define" => {
                let sym_or_args = cdr
                    .car()
                    .ok_or_else(|| Error::InvalidNumArgs("define".into()))?;

                if sym_or_args.is_pair() {
                    for sym in sym_or_args.cdr().unwrap() {
                        if sym.is_symbol() {
                            env.insert(sym);
                        }
                    }
                }

                cdr.cdr()
                    .ok_or_else(|| Error::InvalidNumArgs("define".into()))?
            }
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

    while rest.is_pair() {
        find_free_symbols(rest.car().unwrap(), env, free)?;
        rest = rest.cdr().unwrap();
    }

    // improper list
    if !rest.is_nil() {
        find_free_symbols(rest, env, free)?;
    }

    Ok(())
}

/// Interally defined symbols
///
/// Given the body of a lambda, return the list of internally defined
/// symbols as a result of calls to define at the beginning of the block.
///
/// Internal define expressions are only allowed at the beginning of the
/// block. Any additional defines will be skipped in anticipation of the
/// compiler compiling this lambda expression forming an error.
///
/// This function also acts as a syntax check to ensure define is only
/// present at the beginning of a body.
///
/// # Arguments
/// `body` - The body of the lambda
pub fn internally_defined_symbols(body: &Cell) -> Result<HashSet<&Cell>, Error> {
    let mut symbols = HashSet::new();
    let mut beginning_of_body = true;

    for expr in body {
        if expr.is_pair() && expr.car().unwrap().is_define() {
            if !beginning_of_body {
                return Err(InvalidDefineSyntax(format!("out of context: {}", expr)));
            }
            let expr = expr.cdr().unwrap();
            if expr.is_pair() {
                let expr = expr.car().unwrap();
                if expr.is_symbol() {
                    symbols.insert(expr);
                } else if expr.is_pair() && expr.car().unwrap().is_symbol() {
                    symbols.insert(expr.car().unwrap());
                }
            }
            continue;
        } else {
            beginning_of_body = false;
        }
    }
    Ok(symbols)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{cell, lex, parse};

    #[test]
    fn same_binding_same_slot() {
        let mut env = GlobalEnvironment::new();
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
        let mut env = GlobalEnvironment::new();
        assert_eq!(env.get_binding(50_usize), 0);
        env.put_slot(0, VCell::nil());
    }

    #[test]
    fn primitive_symbol_check() {
        assert!(cell!["lambda"].is_primitive_symbol());
        assert!(!cell!["foo"].is_primitive_symbol());
        assert!(!cell![100].is_primitive_symbol());
    }

    #[test]
    fn free_syms() {
        // Single symbol not in the environment
        assert_eq!(free_symbols(&parse!["a"]), Ok(HashSet::from([&cell!["a"]])));

        // Atom
        assert_eq!(free_symbols(&parse!["42"]), Ok(HashSet::new()));
        assert_eq!(free_symbols(&parse!["#t"]), Ok(HashSet::new()));

        // Any quoted symbols are completely ignored
        assert_eq!(free_symbols(&parse!["(quote (a b c))"]), Ok(HashSet::new()));

        // procedure
        assert_eq!(
            free_symbols(&parse!["(a b c)"]),
            Ok(HashSet::from([&cell!["a"], &cell!["b"], &cell!["c"]]))
        );

        // Nested primitive procedures
        assert_eq!(
            free_symbols(&parse!["(+ (* a b) (* c d) e)"]),
            Ok(HashSet::from([
                &cell!["+"],
                &cell!["*"],
                &cell!["a"],
                &cell!["b"],
                &cell!["c"],
                &cell!["d"],
                &cell!["e"]
            ]))
        );

        // Improper list
        assert_eq!(
            free_symbols(&parse!["(a b . c)"]),
            Ok(HashSet::from([&cell!["a"], &cell!["b"], &cell!["c"]]))
        );

        // Define
        assert_eq!(
            free_symbols(&parse!["(define a b)"]),
            Ok(HashSet::from([&cell!["b"]]))
        );
        assert_eq!(
            free_symbols(&parse!["(define (a x) (+ x y))"]),
            Ok(HashSet::from([&cell!["+"], &cell!["y"]]))
        );

        // inner define procedure
        assert_eq!(
            free_symbols(&parse![
                r#"
                (define (factorial n)
                    (define (factorial n acc)
                       (if (= n 0) 
                          acc
                          (factorial (- n 1) (* n acc))))
                    (factorial n 1))                    
            "#
            ]),
            Ok(HashSet::from([
                &cell!["="],
                &cell!["-"],
                &cell!["*"],
                &cell!["factorial"]
            ]))
        );

        // lambdas
        assert_eq!(
            free_symbols(&parse!["(lambda (x) (+ x y) z)"]),
            Ok(HashSet::from([&cell!["y"], &cell!["z"], &cell!["+"]]))
        );

        assert_eq!(
            free_symbols(&parse!("(lambda (n) (+ ((adder num) n)))")),
            Ok(HashSet::from([&cell!["adder"], &cell!["num"], &cell!["+"]]))
        );

        assert!(free_symbols(&parse!["(lambda)"]).is_err());
        assert!(free_symbols(&parse!["(lambda (10) (+ x y))"]).is_err());
        assert!(free_symbols(&parse!["(lambda (a 10) (+ x y))"]).is_err());
    }

    #[test]
    fn free_syms_returns_errors() {
        assert!(free_symbols(&parse!["(define)"]).is_err());
    }

    #[test]
    fn internally_defined_symbols_returns_vec() {
        assert_eq!(
            internally_defined_symbols(&parse!["((define foo 10)(define (bar baz) 10))"]),
            Ok(HashSet::from([&cell!["foo"], &cell!["bar"]]))
        );
        assert!(internally_defined_symbols(&parse![
            "((define foo 10)(set! foo 5)(define (bar baz) 10))"
        ])
        .is_err());
    }
}
