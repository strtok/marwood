use crate::cell;
use crate::cell::Cell;
use crate::vm::gc;
use crate::vm::gc::State;
use crate::vm::node::TaggedPtr::{NodePtr, SymbolPtr};
use crate::vm::node::{Node, Ptr};
use log::trace;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub struct Heap {
    chunk_size: usize,
    free_list: Vec<usize>,
    heap: Vec<Node>,
    heap_map: gc::Map,
    str_map: HashMap<String, usize>,
}

impl Heap {
    /// New
    ///
    /// Construct a new node of the given chunk size, and allocating an initial
    /// chunk of free nodes.
    pub fn new(chunk_size: usize) -> Heap {
        Heap {
            chunk_size,
            heap: vec![Node::undefined(); chunk_size],
            free_list: (0..chunk_size).rev().into_iter().collect(),
            heap_map: gc::Map::new(chunk_size),
            str_map: HashMap::new(),
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
    /// Free a node, overwriting its value with Undefined and
    /// adding it back to the free list.
    pub fn free(&mut self, ptr: usize) {
        self.heap_map.set(ptr, State::Free);
        if let Some(Node::Symbol(sym)) = self.heap.get(ptr) {
            self.str_map.remove(&**sym);
        }
        *self.heap.get_mut(ptr).unwrap() = Node::Undefined;
        self.free_list.push(ptr);
    }

    /// Put
    ///
    /// Put the given cell value on the next available free node in the
    /// heap and return the position of the node.
    pub fn put<T: Into<Node> + Clone>(&mut self, node: T) -> Node {
        let node = node.into();
        match &node {
            Node::Ptr(ptr) => ptr.into(),
            Node::Symbol(sym) => match self.str_map.get(sym.deref()) {
                Some(ptr) => Node::symbol_ptr(*ptr),
                None => {
                    let ptr = self.alloc();
                    *self.heap.get_mut(ptr).expect("heap index is out of bounds") = node.clone();
                    self.str_map.insert(sym.deref().into(), ptr);
                    Node::symbol_ptr(ptr)
                }
            },
            node => {
                let ptr = self.alloc();
                *self.heap.get_mut(ptr).expect("heap index is out of bounds") = node.clone();
                Node::from(Ptr::new_node_ptr(ptr))
            }
        }
    }

    /// Put Cell
    ///
    /// Allocate the given cell on the heap, returning a Node::Ptr
    /// to the root of the allocated structure. This will recursively
    /// allocate a structure and may result in multiple allocations.
    ///
    /// # Arguments
    /// `ast` - The structure to allocate recursively on the heap.
    pub fn put_cell(&mut self, ast: &cell::Cell) -> Node {
        match *ast {
            cell::Cell::Undefined => self.put(Node::Undefined),
            cell::Cell::Void => self.put(Node::Void),
            cell::Cell::Nil => self.put(Node::Nil),
            cell::Cell::Number(val) => self.put(Node::FixedNum(val)),
            cell::Cell::Bool(val) => self.put(Node::Bool(val)),
            cell::Cell::Pair(ref car, ref cdr) => {
                match (self.put_cell(car.deref()), self.put_cell(cdr.deref())) {
                    (Node::Ptr(car), Node::Ptr(cdr)) => self.put(Node::Pair(car, cdr)),
                    _ => panic!("expected ptr, got {:?}", ast),
                }
            }
            cell::Cell::Symbol(ref sym) => self.put(Node::symbol(sym.clone())),
        }
    }

    /// Get at Index
    ///
    /// Get the node at ptr.
    ///
    /// # Arguments
    /// `ptr` - The index of the node to return.
    pub fn get_at_index(&self, ptr: usize) -> &Node {
        self.heap.get(ptr).expect("heap index out of bounds")
    }

    /// Get at Index Mut
    ///
    /// Get the node at ptr.
    ///
    /// # Arguments
    /// `ptr` - The index of the node to return.
    pub fn get_at_index_mut(&mut self, ptr: usize) -> &mut Node {
        self.heap.get_mut(ptr).expect("heap index out of bounds")
    }

    /// Get
    ///
    /// Return a node from the heap at the given ptr. If the node is not
    /// a ptr, then return the node.
    ///
    /// # Arguments
    /// `node` - The ptr node
    pub fn get(&self, node: &Node) -> Node {
        match node {
            Node::Ptr(ptr) => match ptr.get() {
                NodePtr(ptr) => self.get_at_index(ptr).clone(),
                SymbolPtr(ptr) => self.get_at_index(ptr).clone(),
            },
            _ => node.clone(),
        }
    }

    /// Get As Ast
    ///
    /// Return a Cell representation of the given node by copying the recursive
    /// structure out of the heap into a Cell structure.
    ///
    /// Panic if the type is not capable of being represented as a cell.
    ///
    /// # Arguments
    /// `node` - The node to map to a cell
    pub fn get_as_cell(&self, node: &Node) -> Cell {
        match node {
            Node::Ptr(ptr) => self.get_as_cell(self.get_at_index(ptr.as_usize())),
            Node::FixedNum(val) => Cell::Number(*val),
            Node::Nil => Cell::Nil,
            Node::Bool(val) => Cell::Bool(*val),
            Node::Pair(ref car, cdr) => {
                Cell::new_pair(self.get_as_cell(&car.into()), self.get_as_cell(&cdr.into()))
            }
            Node::EnvSlot(_) => panic!("unexpected environment slot"),
            Node::OpCode(_) => panic!("unexpected opcode"),
            Node::Undefined => Cell::Undefined,
            Node::Symbol(s) => Cell::Symbol(s.deref().into()),
            Node::Void => Cell::Void,
        }
    }

    /// Mark
    ///
    /// Mark the given root node in the gc map, and recursively mark any of
    /// its children.
    ///
    /// # Arguments
    /// `root` - The root node to mark
    pub fn mark(&mut self, root: Ptr) {
        let mut ptr = root;
        loop {
            let node = match self.heap.get(ptr.as_usize()) {
                Some(node) => node.clone(),
                None => {
                    return;
                }
            };

            // Avoid cyclic graphs by following already marked paths
            if self.heap_map.is_marked(ptr.as_usize()) {
                return;
            } else {
                self.heap_map.mark(ptr.as_usize());
            }

            trace!("mark {} => {}", ptr.as_usize(), node);
            match node {
                Node::Pair(car, cdr) => {
                    self.mark(car);
                    ptr = cdr;
                }
                Node::Ptr(cdr) => {
                    ptr = cdr;
                }
                _ => {
                    break;
                }
            }
        }
    }

    /// Sweep
    ///
    /// Iterate the heap map. Performing the following for each object state:
    ///
    /// * State::Free - no op
    /// * State::Allocated - Mark the node as State::Free and append it to the
    ///        free list.
    /// * State::Used - Mark the node as allocated.
    pub fn sweep(&mut self) {
        let before = self.free_list.len();
        for it in 0..self.heap.len() {
            match self.heap_map.get(it) {
                Some(State::Allocated) => {
                    trace!("free {} => {}", it, self.heap.get(it).unwrap());
                    self.free(it);
                }
                Some(State::Used) => {
                    self.heap_map.set(it, State::Allocated);
                }
                _ => {}
            }
        }
        trace!("freed {} node(s)", self.free_list.len() - before);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell::Cell;
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
        *heap.get_at_index_mut(0) = Node::FixedNum(42.into());
        *heap.get_at_index_mut(1) = Node::FixedNum(43.into());
        assert_eq!(heap.get_at_index(0), &Node::FixedNum(42.into()));
        assert_eq!(heap.get_at_index(1), &Node::FixedNum(43.into()));
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
            let node = heap.put_cell(&cell![42]);
            assert_eq!(heap.get_as_cell(&node), cell![42]);
        }
        // bool
        {
            let mut heap = Heap::new(CHUNK_SIZE);
            let true_node = heap.put_cell(&cell![true]);
            let false_node = heap.put_cell(&cell![false]);
            assert_eq!(heap.get_as_cell(&true_node), cell![true]);
            assert_eq!(heap.get_as_cell(&false_node), cell![false]);
        }
        // Nil
        {
            let mut heap = Heap::new(CHUNK_SIZE);
            let node = heap.put_cell(&cell![]);
            assert_eq!(heap.get_as_cell(&node), cell![]);
        }
        // Pair
        {
            let mut heap = Heap::new(CHUNK_SIZE);
            let node = heap.put_cell(&cons![10, 20]);
            assert_eq!(heap.get_as_cell(&node), cons![10, 20]);
        }
        // Symbol
        {
            let mut heap = Heap::new(CHUNK_SIZE);
            let node = heap.put_cell(&cell!["foo"]);
            assert_eq!(heap.get_as_cell(&node), cell!["foo"]);
        }
    }

    #[test]
    fn single_node_mark() {
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
        let pair = heap.put(Node::Pair(car.as_ptr().unwrap(), Ptr::new_node_ptr(1)));
        heap.mark(pair.as_ptr().unwrap());
        heap.sweep();
        assert_eq!(heap.free_list.len(), CHUNK_SIZE - 2);
    }
}
