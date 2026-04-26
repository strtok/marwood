use crate::cell;
use crate::cell::Cell;
use crate::vm::continuation::Continuation;
use crate::vm::gc;
use crate::vm::gc::State;
use crate::vm::lambda::Lambda;
use crate::vm::vcell::VCell;
use log::trace;
use num::ToPrimitive;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;

pub type HeapRef = usize;

#[derive(Debug)]
pub struct Heap {
    chunk_size: usize,
    free_list: Vec<usize>,
    heap: Vec<VCell>,
    heap_map: gc::Map,
    /// Intern table for symbols and hygienic identifiers, keyed by
    /// (name, scope). `Cell::Symbol` always uses scope 0; each
    /// `Cell::Identifier` uses its carried scope. Different scopes for
    /// the same name produce distinct heap slots, which is the
    /// mechanism that gives hygienic identifiers distinct binding
    /// identity at compile time.
    symbol_table: HashMap<(String, u32), usize>,
}

impl Heap {
    /// New
    ///
    /// Construct a new heap of the given chunk size, and allocating an initial
    /// chunk of free vcells.
    pub fn new(chunk_size: usize) -> Heap {
        Heap {
            chunk_size,
            heap: vec![VCell::undefined(); chunk_size],
            free_list: (0..chunk_size).rev().collect(),
            heap_map: gc::Map::new(chunk_size),
            symbol_table: HashMap::new(),
        }
    }

    /// Grow
    ///
    /// Grow the heap by one chunk, adding the newly created nodes
    /// too the free list.
    pub fn grow(&mut self) {
        let current_size = self.heap.len();
        let new_size = ((current_size / self.chunk_size) as f64 * 1.5)
            .ceil()
            .to_usize()
            .unwrap()
            * self.chunk_size;
        self.heap.resize(new_size, VCell::undefined());
        self.heap_map.resize(new_size);
        (current_size..new_size).for_each(|it| self.free_list.push(it));
    }

    /// Alloc
    ///
    /// Return the next free slot from the free list.
    pub fn alloc(&mut self) -> usize {
        match self.free_list.pop() {
            None => {
                self.grow();
                self.alloc()
            }
            Some(ptr) => {
                self.heap_map.set(ptr, State::Allocated);
                ptr
            }
        }
    }

    /// Free
    ///
    /// Free a vcell, overwriting its value with Undefined and
    /// adding it back to the free list.
    pub fn free(&mut self, ptr: usize) {
        self.heap_map.set(ptr, State::Free);
        if let Some(VCell::Symbol(_)) = self.heap.get(ptr) {
            // VCell::Symbol doesn't carry the scope tag, so scan the
            // intern table for the (name, scope) entry pointing at
            // this slot. `free` is not on a hot path.
            let key = self
                .symbol_table
                .iter()
                .find(|(_, v)| **v == ptr)
                .map(|(k, _)| k.clone());
            if let Some(key) = key {
                self.symbol_table.remove(&key);
            }
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
            VCell::Ptr(_) => vcell,
            VCell::Symbol(sym) => {
                let key = (sym.deref().clone(), 0u32);
                match self.symbol_table.get(&key) {
                    Some(ptr) => VCell::ptr(*ptr),
                    None => {
                        let ptr = self.alloc();
                        *self.heap.get_mut(ptr).expect("heap index is out of bounds") =
                            vcell.clone();
                        self.symbol_table.insert(key, ptr);
                        VCell::ptr(ptr)
                    }
                }
            }
            vcell => {
                let ptr = self.alloc();
                *self.heap.get_mut(ptr).expect("heap index is out of bounds") = vcell.clone();
                VCell::Ptr(ptr)
            }
        }
    }

    /// Maybe Put
    ///
    /// Put the given cell value on the next available free vcell in the
    /// heap and return the position of the vcell.
    ///
    /// Unlike put, maybe_put will not place non-mutable values on the heap
    /// (e.g. numbers, booleans, etc). Mutable / aggregate structures such
    /// as pairs and vectors must still be placed on the heap.
    pub fn maybe_put<T: Into<VCell> + Clone>(&mut self, vcell: T) -> VCell {
        let vcell = vcell.into();
        match &vcell {
            VCell::Number(_)
            | VCell::Bool(_)
            | VCell::Char(_)
            | VCell::Nil
            | VCell::Void
            | VCell::Undefined => vcell,
            VCell::Ptr(_) => vcell,
            VCell::Symbol(sym) => {
                let key = (sym.deref().clone(), 0u32);
                match self.symbol_table.get(&key) {
                    Some(ptr) => VCell::ptr(*ptr),
                    None => {
                        let ptr = self.alloc();
                        *self.heap.get_mut(ptr).expect("heap index is out of bounds") =
                            vcell.clone();
                        self.symbol_table.insert(key, ptr);
                        VCell::ptr(ptr)
                    }
                }
            }
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
        let mut labels = HashMap::new();
        self.put_cell_with_labels(ast, &mut labels)
    }

    fn put_cell_with_labels(
        &mut self,
        ast: &cell::Cell,
        labels: &mut HashMap<u32, HeapRef>,
    ) -> VCell {
        let vcell = self.maybe_put_cell_with_labels(ast, labels);
        if vcell.is_ptr() {
            vcell
        } else {
            self.put(vcell)
        }
    }

    /// Maybe Put Cell
    ///
    /// Allocate the given cell on the heap, returning a VCell::Ptr
    /// to the root of the allocated structure. This will recursively
    /// allocate a structure and may result in multiple allocations.
    ///
    /// Unline put_cell, maybe_put_cell will not place non-aggregate types
    /// such as numbers, bools, nil, void, etc on the heap.
    ///
    /// # Arguments
    /// `ast` - The structure to allocate recursively on the heap.
    pub fn maybe_put_cell(&mut self, ast: &cell::Cell) -> VCell {
        let mut labels = HashMap::new();
        self.maybe_put_cell_with_labels(ast, &mut labels)
    }

    fn maybe_put_cell_with_labels(
        &mut self,
        ast: &cell::Cell,
        labels: &mut HashMap<u32, HeapRef>,
    ) -> VCell {
        match *ast {
            cell::Cell::Undefined => VCell::Undefined,
            cell::Cell::Void => VCell::Void,
            cell::Cell::Nil => VCell::Nil,
            cell::Cell::Number(ref val) => VCell::Number(val.clone()),
            cell::Cell::Bool(val) => VCell::Bool(val),
            cell::Cell::Char(val) => VCell::Char(val),
            cell::Cell::Pair(ref car, ref cdr) => {
                match (
                    self.put_cell_with_labels(car.deref(), labels),
                    self.put_cell_with_labels(cdr.deref(), labels),
                ) {
                    (VCell::Ptr(car), VCell::Ptr(cdr)) => self.put(VCell::Pair(car, cdr)),
                    _ => panic!("expected ptr, got {:?}", ast),
                }
            }
            cell::Cell::String(ref s) => self.put(VCell::string(s.clone())),
            cell::Cell::Symbol(ref sym) => self.put(VCell::symbol(sym.clone())),
            cell::Cell::Identifier { ref name, scope } => {
                self.put_scoped_symbol(name.clone(), scope)
            }
            cell::Cell::Continuation => panic!("unexpected continuation"),
            cell::Cell::Macro => panic!("unexpected macro"),
            cell::Cell::Procedure(_) => panic!("unexpected lambda"),
            cell::Cell::Vector(ref vector) => {
                let mut outv = Vec::with_capacity(vector.len());
                for it in vector {
                    outv.push(self.maybe_put_cell_with_labels(it, labels))
                }
                self.put(VCell::vector(outv))
            }
            // R7RS datum-label resolution: pre-allocate a placeholder
            // slot so back-references created while interning the inner
            // datum already point at the correct heap location, then
            // copy the inner result into that slot. This is the same
            // mechanism `set-cdr!` uses to tie the knot at runtime.
            cell::Cell::DatumDef(label, ref inner) => {
                let placeholder = self.alloc();
                *self.heap.get_mut(placeholder).expect("heap index out of bounds") =
                    VCell::Undefined;
                labels.insert(label, placeholder);
                let result = self.maybe_put_cell_with_labels(inner, labels);
                let value = match result {
                    VCell::Ptr(p) => self.get_at_index(p).clone(),
                    other => other,
                };
                *self.get_at_index_mut(placeholder) = value;
                VCell::Ptr(placeholder)
            }
            cell::Cell::DatumRef(label) => match labels.get(&label) {
                Some(&idx) => VCell::Ptr(idx),
                None => panic!("unresolved datum label #{}#", label),
            },
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
    /// is not a heap pointer, return it.
    ///
    /// # Arguments
    /// `vcell` - The ptr vcell
    pub fn get<'a, T: Into<Cow<'a, VCell>>>(&self, vcell: T) -> VCell {
        match vcell.into() {
            Cow::Borrowed(vcell) => match vcell {
                VCell::Ptr(ptr) => self.get_at_index(*ptr).clone(),
                vcell => vcell.clone(),
            },
            Cow::Owned(vcell) => match vcell {
                VCell::Ptr(ptr) => self.get_at_index(ptr).clone(),
                vcell => vcell,
            },
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
        // Macros are global and unscoped, so look up by name only —
        // a Cell::Identifier and a Cell::Symbol with the same name
        // resolve to the same macro registration.
        match sym {
            Cell::Symbol(name) => self
                .symbol_table
                .get(&(name.clone(), 0))
                .map(|it| VCell::ptr(*it)),
            Cell::Identifier { name, .. } => self
                .symbol_table
                .get(&(name.clone(), 0))
                .map(|it| VCell::ptr(*it)),
            _ => None,
        }
    }

    /// Intern a (name, scope) symbol slot. Used by hygienic
    /// `Cell::Identifier` allocation. Reuses an existing slot when
    /// (name, scope) has been interned before; otherwise allocates a
    /// new VCell::Symbol(name) on the heap.
    pub fn put_scoped_symbol(&mut self, name: String, scope: u32) -> VCell {
        let key = (name, scope);
        if let Some(ptr) = self.symbol_table.get(&key) {
            return VCell::ptr(*ptr);
        }
        let ptr = self.alloc();
        *self.heap.get_mut(ptr).expect("heap index is out of bounds") =
            VCell::symbol(key.0.clone());
        self.symbol_table.insert(key, ptr);
        VCell::ptr(ptr)
    }

    /// Get As Ast
    ///
    /// Return a Cell representation of the given vcell by copying the recursive
    /// structure out of the heap into a Cell structure.
    ///
    /// Cyclic and back-referenced pair / vector structure is rendered using
    /// R7RS datum-label syntax via `Cell::DatumDef` / `Cell::DatumRef`. A
    /// pre-pass DFS labels heap refs that participate in a back-edge; the
    /// build pass then wraps each labeled ref in a `DatumDef` on first
    /// encounter and emits a `DatumRef` for repeats. Acyclic input pays a
    /// pre-pass walk but is otherwise unchanged.
    ///
    /// Panic if the type is not capable of being represented as a cell.
    ///
    /// # Arguments
    /// `vcell` - The vcell to map to a cell
    pub fn get_as_cell(&self, vcell: &VCell) -> Cell {
        let mut labels = HashMap::<HeapRef, u32>::new();
        let mut on_stack = HashSet::<HeapRef>::new();
        let mut scanned = HashSet::<HeapRef>::new();
        let mut next_label: u32 = 0;
        self.scan_for_cycles(
            vcell,
            &mut on_stack,
            &mut scanned,
            &mut labels,
            &mut next_label,
        );
        let mut emitted = HashSet::<u32>::new();
        self.build_cell(vcell, &labels, &mut emitted)
    }

    /// DFS pre-pass that labels every heap ref reached via a back-edge —
    /// i.e., the cycle anchors. Refs shared without a cycle (DAG sharing)
    /// are intentionally not labeled; this matches R7RS `write` semantics
    /// where labels appear only when needed to terminate output.
    fn scan_for_cycles(
        &self,
        vcell: &VCell,
        on_stack: &mut HashSet<HeapRef>,
        scanned: &mut HashSet<HeapRef>,
        labels: &mut HashMap<HeapRef, u32>,
        next_label: &mut u32,
    ) {
        let (idx, deref) = match vcell {
            VCell::Ptr(p) => (Some(*p), self.get_at_index(*p)),
            _ => (None, vcell),
        };
        match deref {
            VCell::Pair(car, cdr) => {
                if let Some(idx) = idx {
                    if on_stack.contains(&idx) {
                        labels.entry(idx).or_insert_with(|| {
                            let l = *next_label;
                            *next_label += 1;
                            l
                        });
                        return;
                    }
                    if !scanned.insert(idx) {
                        return;
                    }
                    on_stack.insert(idx);
                }
                self.scan_for_cycles(
                    &VCell::Ptr(*car),
                    on_stack,
                    scanned,
                    labels,
                    next_label,
                );
                self.scan_for_cycles(
                    &VCell::Ptr(*cdr),
                    on_stack,
                    scanned,
                    labels,
                    next_label,
                );
                if let Some(idx) = idx {
                    on_stack.remove(&idx);
                }
            }
            VCell::Vector(vector) => {
                if let Some(idx) = idx {
                    if on_stack.contains(&idx) {
                        labels.entry(idx).or_insert_with(|| {
                            let l = *next_label;
                            *next_label += 1;
                            l
                        });
                        return;
                    }
                    if !scanned.insert(idx) {
                        return;
                    }
                    on_stack.insert(idx);
                }
                for i in 0..vector.len() {
                    self.scan_for_cycles(
                        &vector.get(i).unwrap(),
                        on_stack,
                        scanned,
                        labels,
                        next_label,
                    );
                }
                if let Some(idx) = idx {
                    on_stack.remove(&idx);
                }
            }
            _ => {}
        }
    }

    fn build_cell(
        &self,
        vcell: &VCell,
        labels: &HashMap<HeapRef, u32>,
        emitted: &mut HashSet<u32>,
    ) -> Cell {
        let (idx, deref) = match vcell {
            VCell::Ptr(p) => (Some(*p), self.get_at_index(*p)),
            _ => (None, vcell),
        };
        if let Some(idx) = idx
            && let Some(&label) = labels.get(&idx)
        {
            if !emitted.insert(label) {
                return Cell::DatumRef(label);
            }
            let inner = self.build_cell_body(deref, labels, emitted);
            return Cell::DatumDef(label, Box::new(inner));
        }
        self.build_cell_body(deref, labels, emitted)
    }

    fn build_cell_body(
        &self,
        deref: &VCell,
        labels: &HashMap<HeapRef, u32>,
        emitted: &mut HashSet<u32>,
    ) -> Cell {
        match deref {
            VCell::Bool(val) => Cell::Bool(*val),
            VCell::Char(val) => Cell::Char(*val),
            VCell::Number(val) => Cell::Number(val.clone()),
            VCell::Nil => Cell::Nil,
            VCell::Pair(_, _) => {
                let mut v = vec![];
                let mut current = deref.clone();
                loop {
                    v.push(self.build_cell(&current.as_car().unwrap(), labels, emitted));
                    let cdr_ptr = current.as_cdr().unwrap().as_ptr().unwrap();
                    // If the cdr is a labeled ref already emitted on this
                    // walk, terminate the list with a DatumRef tail —
                    // this is the back-edge that closes the cycle.
                    if let Some(&label) = labels.get(&cdr_ptr)
                        && emitted.contains(&label)
                    {
                        return Cell::new_improper_list(v, Cell::DatumRef(label));
                    }
                    let next = self.get_at_index(cdr_ptr);
                    if next.is_pair() {
                        // Labeled-but-not-yet-emitted in cdr position
                        // (cycle anchor reached for the first time
                        // from inside): hand off to build_cell so it
                        // wraps the rest in DatumDef.
                        if labels.contains_key(&cdr_ptr) {
                            let rest = self.build_cell(
                                &VCell::Ptr(cdr_ptr),
                                labels,
                                emitted,
                            );
                            return Cell::new_improper_list(v, rest);
                        }
                        current = next.clone();
                    } else if next.is_nil() {
                        return Cell::new_list(v);
                    } else {
                        return Cell::new_improper_list(
                            v,
                            self.build_cell(&VCell::Ptr(cdr_ptr), labels, emitted),
                        );
                    }
                }
            }
            VCell::String(s) => Cell::String(s.borrow().deref().into()),
            VCell::Symbol(s) => Cell::Symbol(s.deref().into()),
            VCell::Undefined => Cell::Undefined,
            VCell::Void => Cell::Void,
            VCell::Continuation(_) => Cell::Continuation,
            VCell::Closure(ptr, _) => match self.get_at_index(*ptr).as_lambda() {
                Ok(lambda) => Cell::Procedure(Some(lambda.to_string())),
                Err(_) => Cell::Procedure(None),
            },
            VCell::Lambda(lambda) => Cell::Procedure(Some(lambda.to_string())),
            VCell::BuiltInProc(proc) => Cell::Procedure(Some(proc.desc().to_string())),
            VCell::Macro(_) => Cell::Macro,
            VCell::Vector(vector) => {
                let mut outv = Vec::with_capacity(vector.len());
                for i in 0..vector.len() {
                    outv.push(self.build_cell(&vector.get(i).unwrap(), labels, emitted));
                }
                Cell::Vector(outv)
            }
            // Pointers get resolved by build_cell before reaching here.
            VCell::Ptr(_) => self.build_cell(deref, labels, emitted),
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
                panic!("cannot convert VCell {} to Cell", deref)
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
                VCell::Continuation(cont) => {
                    self.mark_continuation(&cont);
                }
                VCell::Lambda(ptr) => {
                    self.mark_lambda(&ptr);
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
                | VCell::Char(_)
                | VCell::BuiltInProc(_)
                | VCell::GlobalEnvSlot(_)
                | VCell::LexicalEnvSlot(_)
                | VCell::LexicalEnvPtr(_, _)
                | VCell::InstructionPointer(_, _)
                | VCell::Nil
                | VCell::Number(_)
                | VCell::OpCode(_)
                | VCell::String(_)
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
            VCell::Continuation(cont) => {
                self.mark_continuation(cont);
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
            | VCell::Char(_)
            | VCell::GlobalEnvSlot(_)
            | VCell::LexicalEnv(_)
            | VCell::LexicalEnvSlot(_)
            | VCell::Nil
            | VCell::Number(_)
            | VCell::OpCode(_)
            | VCell::String(_)
            | VCell::Symbol(_)
            | VCell::BuiltInProc(_)
            | VCell::Macro(_)
            | VCell::Undefined
            | VCell::Void => {}
        }
    }

    /// Mark Continuation
    ///
    /// Iterate the saved VM state in the continuation
    pub fn mark_continuation(&mut self, cont: &Continuation) {
        for it in cont.stack().iter() {
            self.mark_vcell(it);
        }
        self.mark(cont.ip().0);
        self.mark(cont.ep());
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
    ///   free list.
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

    /// Size
    ///
    /// The total capacity of the heap.
    pub fn capacity(&self) -> usize {
        self.heap.len()
    }

    /// Free Size
    ///
    /// The number of nodes in the free list.
    pub fn free_size(&self) -> usize {
        self.free_list.len()
    }

    /// Used Size
    ///
    /// The number of nodes in used.
    pub fn used_size(&self) -> usize {
        self.capacity() - self.free_size()
    }

    /// Chunk Size
    ///
    /// The chunk allocation size of this heap.
    pub fn chunk_size(&self) -> usize {
        self.chunk_size
    }

    /// Print the heap slots between start and end
    pub fn trace(&self, start: usize, end: usize) {
        for it in (start..end).rev() {
            trace!(
                "${:02x} = {}",
                it,
                self.heap.get(it).unwrap_or(&VCell::Undefined)
            );
        }
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

    #[test]
    fn heap_auto_grows() {
        let mut heap = Heap::new(8);
        assert_eq!(heap.chunk_size(), 8);
        assert_eq!(heap.capacity(), 8);
        assert_eq!(heap.free_size(), 8);
        for _ in 0..9 {
            heap.put_cell(&cell![0]);
        }
        assert_eq!(heap.chunk_size(), 8);
        assert_eq!(heap.capacity(), 16);
        assert_eq!(heap.free_size(), 7);
    }
}
