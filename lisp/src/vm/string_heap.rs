use crate::vm::node::Node;
use std::collections::HashMap;

/// Heap
///
/// A string heap is a heap that supports garbage collected string interning
/// of strings. The heap is used as a storage for both user defined symbols,
/// as well as immutable UTF-8 safe strings supporting scheme's string type.
#[derive(Debug)]
pub struct Heap {
    chunk_size: usize,
    heap: Vec<String>,
    free_list: Vec<usize>,
    map: HashMap<String, usize>,
}

impl Heap {
    pub fn new(chunk_size: usize) -> Heap {
        Heap {
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
    use crate::vm::node::{Ptr, Value};
    const CHUNK_SIZE: usize = 1024;

    #[test]
    fn string_heap() {
        let mut heap = Heap::new(CHUNK_SIZE);
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
