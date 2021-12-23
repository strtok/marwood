use crate::cell;
use crate::cell::Cell;
use crate::vm::node::RefType::{NodePtr, SymbolPtr};
use crate::vm::node::{FixedNum, Node, Ptr, Value};
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug)]
pub struct Heap {
    chunk_size: usize,
    free_list: Vec<usize>,
    heap: Vec<Node>,
    pub string_heap: StringHeap,
}

impl Heap {
    pub fn new(chunk_size: usize) -> Heap {
        Heap {
            chunk_size,
            heap: vec![Node::undefined(); chunk_size],
            free_list: (0..chunk_size).rev().into_iter().collect(),
            string_heap: StringHeap::new(chunk_size),
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
    /// Allocate the given cell on the heap, returning a Node::Reference
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
                    _ => panic!("expected references, got {:?}", ast),
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
    /// Return a node from the heap at the given reference. If the node is not
    /// a reference, then return the node.
    ///
    /// # Arguments
    /// `node` - The reference node
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

/// String Heap
///
/// String heap is a heap that supports garbage collected string interning
/// of strings. The heap is used as a storage for both user defined symbols,
/// as well as immutable UTF-8 safe strings supporting scheme's string type.
#[derive(Debug)]
pub struct StringHeap {
    chunk_size: usize,
    heap: Vec<String>,
    free_list: Vec<usize>,
    map: HashMap<String, usize>,
}

impl StringHeap {
    pub fn new(chunk_size: usize) -> StringHeap {
        StringHeap {
            chunk_size,
            heap: vec![String::new(); chunk_size],
            free_list: (0..chunk_size).rev().into_iter().collect(),
            map: HashMap::new(),
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
    /// Put the given String on the next available free node in the
    /// heap and return the position of the node.
    pub fn put_symbol(&mut self, sym: &str) -> Node {
        match self.map.get(sym) {
            Some(ptr) => Node::symbol(*ptr),
            None => {
                let ptr = self.alloc();
                *self.heap.get_mut(ptr).expect("heap index is out of bounds") = sym.into();
                self.map.insert(sym.into(), ptr);
                Node::symbol(ptr)
            }
        }
    }

    /// Get at Index
    ///
    /// Get the node at ptr.
    ///
    /// # Arguments
    /// `ptr` - The index of the node to return.
    pub fn get_at_index(&self, ptr: usize) -> &str {
        self.heap.get(ptr).expect("heap index out of bounds")
    }

    /// Get Symbol
    ///
    /// Get symbol at ptr. This is a reverse lookup of the symbol
    /// map and is generally only executed if an error occurred.
    ///
    /// # Arguments
    /// `ptr` - The index of the symbol to return.
    pub fn get_symbol<T: Into<usize>>(&self, ptr: T) -> Option<String> {
        let ptr = ptr.into();
        self.map
            .iter()
            .find(|it| *(it.1) == ptr)
            .map(|it| it.0.clone())
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
        assert_eq!(heap.alloc(), 0);
        assert_eq!(heap.alloc(), 1);
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

    #[test]
    fn string_heap() {
        let mut heap = StringHeap::new(CHUNK_SIZE);
        assert_eq!(
            heap.put_symbol("foo").val,
            Value::Ptr(Ptr::new_symbol_ptr(0))
        );
        assert_eq!(
            heap.put_symbol("bar").val,
            Value::Ptr(Ptr::new_symbol_ptr(1))
        );
        assert_eq!(
            heap.put_symbol("foo").val,
            Value::Ptr(Ptr::new_symbol_ptr(0))
        );
        assert_eq!(heap.get_symbol(0_usize), Some("foo".into()));
        assert_eq!(heap.get_symbol(1_usize), Some("bar".into()));
        assert_eq!(heap.get_symbol(2_usize), None);
    }
}
