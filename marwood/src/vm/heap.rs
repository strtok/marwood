use crate::cell;
use crate::cell::Cell;
use crate::vm::gc;
use crate::vm::gc::State;
use crate::vm::lambda::Lambda;
use crate::vm::vcell::VCell;
use log::trace;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub struct Heap {
    free_list: Vec<usize>,
    heap: Vec<VCell>,
    heap_map: gc::Map,
    symbol_table: HashMap<String, usize>,
}

impl Heap {
    /// New
    ///
    /// Construct a new heap of the given chunk size, and allocating an initial
    /// chunk of free vcells.
    pub fn new(chunk_size: usize) -> Heap {
        Heap {
            heap: vec![VCell::undefined(); chunk_size],
            free_list: (0..chunk_size).rev().into_iter().collect(),
            heap_map: gc::Map::new(chunk_size),
            symbol_table: HashMap::new(),
        }
    }

    /// Alloc
    ///
    /// Return the next free slot from the free list.
    pub fn alloc(&mut self) -> usize {
        let ptr = self.free_list.pop().unwrap();
        self.heap_map.set(ptr, State::Allocated);
        ptr
    }

    /// Free
    ///
    /// Free a vcell, overwriting its value with Undefined and
    /// adding it back to the free list.
    pub fn free(&mut self, ptr: usize) {
        self.heap_map.set(ptr, State::Free);
        if let Some(VCell::Symbol(sym)) = self.heap.get(ptr) {
            self.symbol_table.remove(&**sym);
        }
        *self.heap.get_mut(ptr).unwrap() = VCell::Undefined;
        self.free_list.push(ptr);
    }

    /// Put
    ///
    /// Put the given cell value on the next available free vcell in the
    /// heap and return the position of the vcell.
    pub fn put<T: Into<VCell> + Clone>(&mut self, vcell: T) -> VCell {
        let vcell = vcell.into();
        match &vcell {
            VCell::Ptr(_) => panic!("put() on {} would double box", vcell),
            VCell::Symbol(sym) => match self.symbol_table.get(sym.deref()) {
                Some(ptr) => VCell::ptr(*ptr),
                None => {
                    let ptr = self.alloc();
                    *self.heap.get_mut(ptr).expect("heap index is out of bounds") = vcell.clone();
                    self.symbol_table.insert(sym.deref().into(), ptr);
                    VCell::ptr(ptr)
                }
            },
            vcell => {
                let ptr = self.alloc();
                *self.heap.get_mut(ptr).expect("heap index is out of bounds") = vcell.clone();
                VCell::Ptr(ptr)
            }
        }
    }

    /// Put Cell
    ///
    /// Allocate the given cell on the heap, returning a VCell::Ptr
    /// to the root of the allocated structure. This will recursively
    /// allocate a structure and may result in multiple allocations.
    ///
    /// # Arguments
    /// `ast` - The structure to allocate recursively on the heap.
    pub fn put_cell(&mut self, ast: &cell::Cell) -> VCell {
        match *ast {
            cell::Cell::Undefined => self.put(VCell::Undefined),
            cell::Cell::Void => self.put(VCell::Void),
            cell::Cell::Nil => self.put(VCell::Nil),
            cell::Cell::Number(ref val) => self.put(VCell::Number(val.clone())),
            cell::Cell::Bool(val) => self.put(VCell::Bool(val)),
            cell::Cell::Pair(ref car, ref cdr) => {
                match (self.put_cell(car.deref()), self.put_cell(cdr.deref())) {
                    (VCell::Ptr(car), VCell::Ptr(cdr)) => self.put(VCell::Pair(car, cdr)),
                    _ => panic!("expected ptr, got {:?}", ast),
                }
            }
            cell::Cell::Symbol(ref sym) => self.put(VCell::symbol(sym.clone())),
            cell::Cell::Closure => panic!("unexpected closure"),
            cell::Cell::Macro => panic!("unexpected macro"),
            cell::Cell::Lambda => panic!("unexpected lambda"),
            cell::Cell::Vector(ref vector) => {
                let mut outv = Vec::with_capacity(vector.len());
                for it in vector {
                    outv.push(self.put_cell(it))
                }
                self.put(VCell::vector(outv))
            }
        }
    }

    /// Get at Index
    ///
    /// Get the vcell at ptr.
    ///
    /// # Arguments
    /// `ptr` - The index of the vcell to return.
    pub fn get_at_index(&self, ptr: usize) -> &VCell {
        self.heap.get(ptr).expect("heap index out of bounds")
    }

    /// Get at Index Mut
    ///
    /// Get the vcell at ptr.
    ///
    /// # Arguments
    /// `ptr` - The index of the vcell to return.
    pub fn get_at_index_mut(&mut self, ptr: usize) -> &mut VCell {
        self.heap.get_mut(ptr).expect("heap index out of bounds")
    }

    /// Get
    ///
    /// Return a vcell from the heap at the given ptr. If the vcell
    /// is not a heap pointer, panic.
    ///
    /// # Arguments
    /// `vcell` - The ptr vcell
    pub fn get(&self, vcell: &VCell) -> VCell {
        match vcell {
            VCell::Ptr(ptr) => self.get_at_index(*ptr).clone(),
            _ => panic!("heap get() called on non-reference value {}", vcell),
        }
    }

    /// Get Sym Ref
    ///
    /// Return a Some(VCell) ptr to the given sym reference if sym is known, otherwise
    /// None.
    ///
    /// # Arguments
    /// `sym` - The symbol to lookup
    pub fn get_sym_ref(&self, sym: &Cell) -> Option<VCell> {
        if let Cell::Symbol(sym) = sym {
            self.symbol_table.get(sym).map(|it| VCell::ptr(*it))
        } else {
            None
        }
    }

    /// Get As Ast
    ///
    /// Return a Cell representation of the given vcell by copying the recursive
    /// structure out of the heap into a Cell structure.
    ///
    /// Panic if the type is not capable of being represented as a cell.
    ///
    /// # Arguments
    /// `vcell` - The vcell to map to a cell
    pub fn get_as_cell(&self, vcell: &VCell) -> Cell {
        match vcell {
            VCell::Bool(val) => Cell::Bool(*val),
            VCell::Number(val) => Cell::Number(val.clone()),
            VCell::Nil => Cell::Nil,
            VCell::Pair(ref car, cdr) => Cell::new_pair(
                self.get_as_cell(&VCell::Ptr(*car)),
                self.get_as_cell(&VCell::Ptr(*cdr)),
            ),
            VCell::Ptr(ptr) => self.get_as_cell(self.get_at_index(*ptr)),
            VCell::Symbol(s) => Cell::Symbol(s.deref().into()),
            VCell::Undefined => Cell::Undefined,
            VCell::Void => Cell::Void,
            VCell::Closure(_, _) => Cell::Closure,
            VCell::Lambda(_) => Cell::Lambda,
            VCell::BuiltInProc(_) => Cell::Lambda,
            VCell::Macro(_) => Cell::Macro,
            VCell::Vector(vector) => {
                let mut outv = Vec::with_capacity(vector.len());
                for idx in 0..vector.len() {
                    outv.push(self.get_as_cell(&vector.get(idx).unwrap()));
                }
                Cell::Vector(outv)
            }
            // Any internal values used by bytecode aren't convertible to Cells and
            // result in a panic.
            VCell::Acc
            | VCell::ArgumentCount(_)
            | VCell::BasePointer(_)
            | VCell::BasePointerOffset(_)
            | VCell::EnvironmentPointer(_)
            | VCell::GlobalEnvSlot(_)
            | VCell::LexicalEnv(_)
            | VCell::LexicalEnvSlot(_)
            | VCell::LexicalEnvPtr(_, _)
            | VCell::OpCode(_)
            | VCell::InstructionPointer(_, _) => {
                panic!("cannot convert VCell {} to Cell", vcell)
            }
        }
    }

    /// Mark
    ///
    /// Mark the given root vcell in the gc map, and recursively mark any of
    /// its children.
    ///
    /// # Arguments
    /// `root` - The root vcell to mark
    pub fn mark(&mut self, root: usize) {
        let mut ptr = root;
        loop {
            let vcell = match self.heap.get(ptr) {
                Some(vcell) => vcell.clone(),
                None => {
                    return;
                }
            };

            // Avoid cyclic graphs by following already marked paths
            if self.heap_map.is_marked(ptr) {
                return;
            } else {
                self.heap_map.mark(ptr);
            }

            //trace!("mark {} => {}", ptr, vcell);
            match vcell {
                VCell::Pair(car, cdr) => {
                    self.mark(car);
                    ptr = cdr;
                }
                VCell::Ptr(cdr) => {
                    ptr = cdr;
                }
                VCell::Lambda(ptr) => {
                    self.mark_lambda(&*ptr);
                }
                VCell::Closure(lambda, env) => {
                    self.mark(lambda);
                    self.mark(env);
                }
                VCell::LexicalEnv(env) => {
                    let env = env.as_ref();
                    for it in 0..env.slot_len() {
                        self.mark_vcell(&env.get(it));
                    }
                }
                VCell::Vector(vector) => {
                    for idx in 0..vector.len() {
                        let vcell = vector.get(idx).unwrap();
                        self.mark_vcell(&vcell);
                    }
                }
                VCell::EnvironmentPointer(ptr) => self.mark(ptr),
                VCell::Acc
                | VCell::ArgumentCount(_)
                | VCell::BasePointer(_)
                | VCell::BasePointerOffset(_)
                | VCell::Bool(_)
                | VCell::BuiltInProc(_)
                | VCell::GlobalEnvSlot(_)
                | VCell::LexicalEnvSlot(_)
                | VCell::LexicalEnvPtr(_, _)
                | VCell::InstructionPointer(_, _)
                | VCell::Nil
                | VCell::Number(_)
                | VCell::OpCode(_)
                | VCell::Symbol(_)
                | VCell::Macro(_)
                | VCell::Undefined
                | VCell::Void => {}
            }
        }
    }

    pub fn mark_vcell(&mut self, vcell: &VCell) {
        match vcell {
            VCell::InstructionPointer(lambda, _) => {
                self.mark(*lambda);
            }
            VCell::Lambda(lambda) => self.mark_lambda(lambda.as_ref()),
            VCell::Closure(lambda, env) => {
                self.mark(*lambda);
                self.mark(*env)
            }
            VCell::Pair(car, cdr) => {
                self.mark(*car);
                self.mark(*cdr);
            }
            VCell::Ptr(ptr) => {
                self.mark(*ptr);
            }
            VCell::LexicalEnvPtr(ptr, _) => {
                self.mark(*ptr);
            }
            VCell::Vector(vector) => {
                for idx in 0..vector.len() {
                    let vcell = vector.get(idx).unwrap();
                    self.mark_vcell(&vcell);
                }
            }
            VCell::EnvironmentPointer(ep) => self.mark(*ep),
            VCell::Acc
            | VCell::ArgumentCount(_)
            | VCell::BasePointer(_)
            | VCell::BasePointerOffset(_)
            | VCell::Bool(_)
            | VCell::GlobalEnvSlot(_)
            | VCell::LexicalEnv(_)
            | VCell::LexicalEnvSlot(_)
            | VCell::Nil
            | VCell::Number(_)
            | VCell::OpCode(_)
            | VCell::Symbol(_)
            | VCell::BuiltInProc(_)
            | VCell::Macro(_)
            | VCell::Undefined
            | VCell::Void => {}
        }
    }

    /// Mark Lambda
    ///
    /// Iterate the lambda byte code and mark any value that contains a reference type
    pub fn mark_lambda(&mut self, lambda: &Lambda) {
        // Mark every bytecode cell
        for it in &lambda.bc {
            self.mark_vcell(it)
        }

        // Mark every argument (symbol)
        for it in &lambda.args {
            self.mark_vcell(it);
        }

        // Mark every symbol the envmap refers to
        for it in lambda.envmap.get_map().iter() {
            self.mark_vcell(&it.0);
        }
    }

    /// Sweep
    ///
    /// Iterate the heap map. Performing the following for each object state:
    ///
    /// * State::Free - no op
    /// * State::Allocated - Mark the vcell as State::Free and append it to the
    ///        free list.
    /// * State::Used - Mark the vcell as allocated.
    pub fn sweep(&mut self) {
        let before = self.free_list.len();
        for it in 0..self.heap.len() {
            match self.heap_map.get(it) {
                Some(State::Allocated) => {
                    self.free(it);
                }
                Some(State::Used) => {
                    self.heap_map.set(it, State::Allocated);
                }
                _ => {}
            }
        }
        trace!("freed {} vcell(s)", self.free_list.len() - before);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell::Cell;
    use crate::number::Number;
    use crate::{cell, cons};

    const CHUNK_SIZE: usize = 1024;

    #[test]
    fn alloc_allocs_and_sets_gc_state() {
        let mut heap = Heap::new(CHUNK_SIZE);
        assert_eq!(heap.heap_map.get(0), Some(State::Free));
        assert_eq!(heap.heap_map.get(1), Some(State::Free));
        assert_eq!(heap.alloc(), 0);
        assert_eq!(heap.heap_map.get(0), Some(State::Allocated));
        assert_eq!(heap.alloc(), 1);
        assert_eq!(heap.heap_map.get(1), Some(State::Allocated));
        *heap.get_at_index_mut(0) = VCell::Number(Number::from(42));
        *heap.get_at_index_mut(1) = VCell::Number(Number::from(43));
        assert_eq!(heap.get_at_index(0), &VCell::Number(Number::from(42)));
        assert_eq!(heap.get_at_index(1), &VCell::Number(Number::from(43)));
    }

    #[test]
    fn symbols_are_interned() {
        let mut heap = Heap::new(CHUNK_SIZE);
        assert_eq!(heap.put_cell(&cell!["foo"]), heap.put_cell(&cell!["foo"]));
        assert_ne!(heap.put_cell(&cell!["foo"]), heap.put_cell(&cell!["bar"]));
    }

    #[test]
    fn put_ast() {
        let mut heap = Heap::new(CHUNK_SIZE);
        // FixedNum
        {
            let vcell = heap.put_cell(&cell![42]);
            assert_eq!(heap.get_as_cell(&vcell), cell![42]);
        }
        // bool
        {
            let mut heap = Heap::new(CHUNK_SIZE);
            let true_vcell = heap.put_cell(&cell![true]);
            let false_vcell = heap.put_cell(&cell![false]);
            assert_eq!(heap.get_as_cell(&true_vcell), cell![true]);
            assert_eq!(heap.get_as_cell(&false_vcell), cell![false]);
        }
        // Nil
        {
            let mut heap = Heap::new(CHUNK_SIZE);
            let vcell = heap.put_cell(&cell![]);
            assert_eq!(heap.get_as_cell(&vcell), cell![]);
        }
        // Pair
        {
            let mut heap = Heap::new(CHUNK_SIZE);
            let vcell = heap.put_cell(&cons![10, 20]);
            assert_eq!(heap.get_as_cell(&vcell), cons![10, 20]);
        }
        // Symbol
        {
            let mut heap = Heap::new(CHUNK_SIZE);
            let vcell = heap.put_cell(&cell!["foo"]);
            assert_eq!(heap.get_as_cell(&vcell), cell!["foo"]);
        }
    }

    #[test]
    fn single_vcell_mark() {
        let mut heap = Heap::new(CHUNK_SIZE);
        let root = heap.put_cell(&cell![42]);
        assert_eq!(heap.heap_map.get(0), Some(State::Allocated));
        heap.mark(root.as_ptr().unwrap());
        assert_eq!(heap.heap_map.get(0), Some(State::Used));
        heap.sweep();
        assert_eq!(heap.free_list.len(), CHUNK_SIZE - 1);
        heap.sweep();
        assert_eq!(heap.free_list.len(), CHUNK_SIZE);
    }

    #[test]
    fn pair_mark_and_sweep() {
        let mut heap = Heap::new(CHUNK_SIZE);
        let root = heap.put_cell(&cons![100, 200]);
        assert_eq!(heap.heap_map.get(0), Some(State::Allocated));
        assert_eq!(heap.heap_map.get(1), Some(State::Allocated));
        assert_eq!(heap.heap_map.get(2), Some(State::Allocated));
        heap.mark(root.as_ptr().unwrap());
        assert_eq!(heap.heap_map.get(0), Some(State::Used));
        assert_eq!(heap.heap_map.get(1), Some(State::Used));
        assert_eq!(heap.heap_map.get(2), Some(State::Used));
        heap.sweep();
        assert_eq!(heap.free_list.len(), CHUNK_SIZE - 3);
        heap.sweep();
        assert_eq!(heap.free_list.len(), CHUNK_SIZE);
    }

    #[test]
    fn cyclic_mark_and_sweep() {
        let mut heap = Heap::new(CHUNK_SIZE);
        let car = heap.put_cell(&cell![100]);
        let pair = heap.put(VCell::Pair(car.as_ptr().unwrap(), 1));
        heap.mark(pair.as_ptr().unwrap());
        heap.sweep();
        assert_eq!(heap.free_list.len(), CHUNK_SIZE - 2);
    }
}
