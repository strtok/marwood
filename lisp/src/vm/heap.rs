use crate::cell;
use crate::cell::Cell;
use crate::vm::gc;
use crate::vm::gc::State;
use crate::vm::node::RefType::{NodePtr, SymbolPtr};
use crate::vm::node::{FixedNum, Node, Ptr, Value};
use crate::vm::string_heap;
use std::ops::Deref;

#[derive(Debug)]
pub struct Heap {
    chunk_size: usize,
    free_list: Vec<usize>,
    heap: Vec<Node>,
    heap_map: gc::Map,
    pub string_heap: string_heap::Heap,
}

impl Heap {
    pub fn new(chunk_size: usize) -> Heap {
        Heap {
            chunk_size,
            heap: vec![Node::undefined(); chunk_size],
            free_list: (0..chunk_size).rev().into_iter().collect(),
            heap_map: gc::Map::new(chunk_size),
            string_heap: string_heap::Heap::new(chunk_size),
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

    /// Put
    ///
    /// Put the given cell value on the next available free node in the
    /// heap and return the position of the node.
    pub fn put<T: Into<Value> + Clone>(&mut self, val: T) -> Node {
        match val.into() {
            Value::Ptr(ptr) => ptr.into(),
            val => {
                let ptr = self.alloc();
                *self.heap.get_mut(ptr).expect("heap index is out of bounds") = Node::new(val);
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
            cell::Cell::Undefined => self.put(Value::Undefined),
            cell::Cell::Void => self.put(Value::Void),
            cell::Cell::Nil => self.put(Value::Nil),
            cell::Cell::Number(val) => self.put(Value::FixedNum(val.into())),
            cell::Cell::Bool(val) => self.put(Value::Bool(val)),
            cell::Cell::Pair(ref car, ref cdr) => {
                match (
                    self.put_cell(car.deref()).val,
                    self.put_cell(cdr.deref()).val,
                ) {
                    (Value::Ptr(car), Value::Ptr(cdr)) => self.put(Value::Pair(car, cdr)),
                    _ => panic!("expected ptr, got {:?}", ast),
                }
            }
            cell::Cell::Symbol(ref sym) => {
                let node = self.string_heap.put_symbol(sym);
                self.put(node)
            }
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
        match node.val {
            Value::Ptr(ptr) => match ptr.get() {
                NodePtr(ptr) => self.get_at_index(ptr).clone(),
                SymbolPtr(ptr) => Node::symbol(ptr),
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
        match node.val {
            Value::Ptr(ptr) => match ptr.get() {
                SymbolPtr(ptr) => self.string_heap.get_at_index(ptr).into(),
                NodePtr(ptr) => self.get_as_cell(self.get_at_index(ptr)),
            },
            Value::FixedNum(FixedNum(val)) => Cell::Number(val),
            Value::Nil => Cell::Nil,
            Value::Bool(val) => Cell::Bool(val),
            Value::Pair(car, cdr) => {
                Cell::new_pair(self.get_as_cell(&car.into()), self.get_as_cell(&cdr.into()))
            }
            Value::EnvSlot(_) => panic!("unexpected environment slot"),
            Value::OpCode(_) => panic!("unexpected opcode"),
            Value::Undefined => Cell::Undefined,
            Value::Void => Cell::Void,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell::Cell;
    use crate::vm::node::Value;
    use crate::{cell, cons};
    const CHUNK_SIZE: usize = 1024;

    #[test]
    fn alloc() {
        let mut heap = Heap::new(CHUNK_SIZE);
        assert_eq!(heap.heap_map.get(0), Some(State::Free));
        assert_eq!(heap.heap_map.get(1), Some(State::Free));
        assert_eq!(heap.alloc(), 0);
        assert_eq!(heap.heap_map.get(0), Some(State::Allocated));
        assert_eq!(heap.alloc(), 1);
        assert_eq!(heap.heap_map.get(1), Some(State::Allocated));
        heap.get_at_index_mut(0).val = Value::FixedNum(42.into());
        heap.get_at_index_mut(1).val = Value::FixedNum(43.into());
        assert_eq!(heap.get_at_index(0).val, Value::FixedNum(42.into()));
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
}
