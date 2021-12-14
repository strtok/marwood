use crate::cell;
use crate::cell::Cell;
use crate::vm::node::{FixedNum, Node, Reference, Value};
use std::ops::Deref;

#[derive(Debug)]
pub struct Heap {
    chunk_size: usize,
    heap: Vec<Node>,
    free_list: Vec<usize>,
}

impl Heap {
    pub fn new(chunk_size: usize) -> Heap {
        Heap {
            chunk_size,
            heap: vec![Node::undefined(); chunk_size],
            free_list: (0..chunk_size).rev().into_iter().collect(),
        }
    }

    /// Alloc
    ///
    /// Return the next free slot from the free list.
    pub fn alloc(&mut self) -> usize {
        self.free_list.pop().unwrap()
    }

    /// Put
    ///
    /// Put the given cell value on the next available free node in the
    /// heap and return the position of the node.
    pub fn put<T: Into<Value> + Clone>(&mut self, val: T) -> Node {
        if let Value::Reference(val) = val.clone().into() {
            Node::from(val)
        } else {
            let idx = self.alloc();
            *self.heap.get_mut(idx).expect("heap index is out of bounds") = Node::new(val.into());
            Node::from(Reference(idx))
        }
    }

    /// Put Cell
    ///
    /// Allocate the given cell on the heap, returning a Node::Reference
    /// to the root of the allocated structure. This will recursively
    /// allocate a structure and may result in multiple allocations.
    ///
    /// # Arguments
    /// `ast` - The structure to allocate recursively on the heap.
    pub fn put_cell(&mut self, ast: &cell::Cell) -> Node {
        match *ast {
            cell::Cell::Nil => self.put(Value::Nil),
            cell::Cell::Number(val) => self.put(Value::FixedNum(FixedNum(val))),
            cell::Cell::Bool(val) => self.put(Value::Bool(val)),
            cell::Cell::Pair(ref car, ref cdr) => {
                match (
                    self.put_cell(car.deref()).val,
                    self.put_cell(cdr.deref()).val,
                ) {
                    (Value::Reference(car), Value::Reference(cdr)) => {
                        self.put(Value::Pair(car, cdr))
                    }
                    _ => panic!("expected references"),
                }
            }
            cell::Cell::Symbol(_) => panic!("symbols not supported yet"),
        }
    }

    /// Get at Index
    ///
    /// Get the node at idx.
    ///
    /// # Arguments
    /// `idx` - The index of the node to return.
    pub fn get_at_index(&self, idx: usize) -> &Node {
        self.heap.get(idx).expect("heap index out of bounds")
    }

    /// Get at Index Mut
    ///
    /// Get the node at idx.
    ///
    /// # Arguments
    /// `idx` - The index of the node to return.
    pub fn get_at_index_mut(&mut self, idx: usize) -> &mut Node {
        self.heap.get_mut(idx).expect("heap index out of bounds")
    }

    /// Get
    ///
    /// Return a node from the heap at the given reference. If the node is not
    /// a reference, then return the node.
    ///
    /// # Arguments
    /// `node` - The reference node
    pub fn get(&self, node: &Node) -> Node {
        match node.val {
            Value::Reference(Reference(idx)) => self.get_at_index(idx).clone(),
            _ => node.clone(),
        }
    }

    /// Get As Ast
    ///
    /// Return a Cell representation of the given node by copying the recursive
    /// structure out of the heap into a Cell structure.
    ///
    /// # Arguments
    /// `node` - The node to map to a cell
    pub fn get_as_cell(&self, node: &Node) -> Cell {
        match node.val {
            Value::Reference(Reference(idx)) => self.get_as_cell(self.get_at_index(idx)),
            Value::FixedNum(FixedNum(val)) => Cell::Number(val),
            Value::Nil => Cell::Nil,
            Value::Bool(val) => Cell::Bool(val),
            Value::Pair(Reference(car), Reference(cdr)) => Cell::new_pair(
                self.get_as_cell(self.get_at_index(car)),
                self.get_as_cell(self.get_at_index(cdr)),
            ),
            Value::OpCode(_) => panic!("unexpected opcode"),
            Value::Undefined => panic!("unexpected undefined"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell::Cell;
    use crate::vm::node::{FixedNum, Value};
    use crate::{cell, cons};
    const CHUNK_SIZE: usize = 1024;

    #[test]
    fn alloc() {
        let mut heap = Heap::new(CHUNK_SIZE);
        assert_eq!(heap.alloc(), 0);
        assert_eq!(heap.alloc(), 1);
        heap.get_at_index_mut(0).val = Value::FixedNum(FixedNum(42));
        heap.get_at_index_mut(1).val = Value::FixedNum(FixedNum(43));
        assert_eq!(heap.get_at_index(0).val, Value::FixedNum(FixedNum(42)));
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
    }
}
