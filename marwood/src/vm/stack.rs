use crate::error::Error;
use crate::error::Error::InvalidStackIndex;
use crate::vm::vcell::VCell;
use log::trace;
use std::fmt::Display;

/// Stack
///
/// Stack represents a bottom up stack designed to be accessed
/// directly by a stack pointer. It supports push, pop and relative
/// access operations to support VM operations on a stack.
///
/// It is self-growing, in that it will automatically resize
/// itself. It does not currently attempt to shrink itself,
/// because it would need to know whether or not the VM is
/// still referencing addresses positive to the stack pointer.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Stack {
    /// Stack contents
    stack: Vec<VCell>,

    /// Stack Pointer. SP points to the top value to be pushed onto the stack,
    /// This value backs the SP register of the VM
    sp: usize,
}

impl Stack {
    /// Make a new stack with an initial stack size of 256 slots.
    pub fn new() -> Stack {
        Stack {
            stack: vec![VCell::undefined(); 256],
            sp: 0,
        }
    }

    /// Clear
    ///
    /// Clear clears any old stack values so that they're no longer
    /// participating in garbage collection.
    pub fn clear(&mut self) {
        let size = self.stack.len();
        self.stack = vec![VCell::undefined(); size];
    }

    /// Iter
    ///
    /// Return an iterator to the stack vector
    pub fn iter(&self) -> impl Iterator<Item = &VCell> {
        self.stack.iter()
    }

    /// Iter
    ///
    /// Return an iterator to the stack vector up to the stack pointer
    pub fn iter_to_sp(&self) -> impl Iterator<Item = &VCell> {
        self.stack[0..self.sp + 1].iter()
    }

    /// Get
    ///
    /// Get a VC at the given stack location.
    ///
    /// This supports absolute stack addressing modes, such as those
    /// needed for BP[offset]
    pub fn get(&self, index: usize) -> Result<&VCell, Error> {
        self.stack.get(index).ok_or(InvalidStackIndex(index))
    }

    /// Get Mut
    ///
    /// Get a VC at the given stack location.
    ///
    /// This supports absolute stack addressing modes, such as those
    /// needed for BP[offset]
    pub fn get_mut(&mut self, index: usize) -> Result<&mut VCell, Error> {
        self.stack.get_mut(index).ok_or(InvalidStackIndex(index))
    }

    /// Get Offset
    ///
    /// Get a vcell relative to the stack pointer, where an offset of 0
    /// is the top of the stack (the last stack value pushed).
    ///
    /// This supports stackr elative accessing modes, such as those used in
    /// call frames / argument passing.
    ///
    /// # Arguments
    /// `offset` - The offset from the top of the stack to access, where an offset
    /// of 0 is the top oif the stack.
    pub fn get_offset(&self, offset: i64) -> Result<&VCell, Error> {
        let index = (self.sp as i64 + offset) as usize;
        self.stack.get(index).ok_or(InvalidStackIndex(index))
    }

    /// Get Offset Mut
    ///
    /// Identical to get(), but returns a mut stack value.
    pub fn get_offset_mut(&mut self, offset: i64) -> Result<&mut VCell, Error> {
        let index = (self.sp as i64 + offset) as usize;
        self.stack.get_mut(index).ok_or(InvalidStackIndex(index))
    }

    /// Sp
    ///
    /// Return the value of the SP register. This is generally used by VM
    /// instructions that need to store an offset to the SP register (e.g.
    /// to form a BP register).
    pub fn get_sp(&self) -> usize {
        self.sp
    }

    /// Get Sp Mut
    ///
    /// Get a mutable reference to the SP register. This is used by instructions
    /// that modify the SP register directly (e.g. during call frame building/teardown)
    pub fn get_sp_mut(&mut self) -> &mut usize {
        &mut self.sp
    }

    /// Grow
    ///
    /// Grow the stack by doubling its current size. Any new elements
    /// have the value of VCell::Undefined
    fn grow(&mut self) {
        self.stack.resize(self.stack.len() * 2, VCell::Undefined);
    }

    /// Len
    ///
    /// Return the current stack size
    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn is_empty(&self) -> bool {
        self.sp == 0
    }

    /// Push
    ///
    /// Push a value at the top of the stack, incrementing SP
    pub fn push<T: Into<VCell> + Display>(&mut self, vcell: T) {
        match self.stack.get_mut(self.sp + 1) {
            Some(slot) => {
                *slot = vcell.into();
                self.sp += 1;
            }
            None => {
                self.grow();
                self.push(vcell)
            }
        }
    }

    /// Pop
    ///
    /// Pop the top of the stack, returning a Option<VCell> that is
    /// Some(&VCell), or None if the stack was empty.
    pub fn pop(&mut self) -> Result<&VCell, Error> {
        if self.sp > 0 {
            self.sp -= 1;
            self.stack
                .get(self.sp + 1)
                .ok_or(InvalidStackIndex(self.sp + 1))
        } else {
            Err(InvalidStackIndex(0))
        }
    }

    /// Print a stack trace of the slots between start and end
    pub fn trace(&self, start: usize, end: usize) {
        for it in (start..end).rev() {
            trace!(
                "${:02x} = {}",
                it,
                self.get(it).unwrap_or(&VCell::Undefined)
            );
        }
    }

    /// To Continuation
    ///
    /// Clone a subset of the stack into the return Stack, cloning
    /// only the portion of the stack necessary to save the current continuation.
    pub fn to_continuation(&self) -> Stack {
        Stack {
            stack: self.stack[0..self.sp + 1].to_vec(),
            sp: self.sp,
        }
    }

    /// Restore Continuation
    ///
    /// Given a continuation stack returned by to_continuation, restore it.
    pub fn restore_continuation(&mut self, cont: &Stack) {
        self.stack
            .split_at_mut(cont.stack.len())
            .0
            .clone_from_slice(&cont.stack);
        self.sp = cont.sp;
    }
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::number::Number;

    #[test]
    fn stack_grows_on_push() {
        let mut stack = Stack::new();
        assert_eq!(stack.len(), 256);
        for i in 0..1024 {
            stack.push(VCell::Number(Number::from(i)));
        }
        assert_eq!(stack.len(), 2048)
    }

    #[test]
    fn relative_access() {
        let mut stack = Stack::new();
        stack.push(VCell::number(0));
        stack.push(VCell::number(1));
        stack.push(VCell::number(2));
        assert_eq!(stack.get_offset(2), Ok(&VCell::Undefined));
        assert_eq!(stack.get_offset(1), Ok(&VCell::Undefined));
        assert_eq!(stack.get_offset(0), Ok(&VCell::number(2)));
        assert_eq!(stack.get_offset(-1), Ok(&VCell::number(1)));
        assert_eq!(stack.get_offset(-2), Ok(&VCell::number(0)));
        assert_eq!(stack.get_offset(-3), Ok(&VCell::Undefined));
    }

    #[test]
    fn push_and_pop() {
        let mut stack = Stack::new();
        stack.push(VCell::number(1));
        stack.push(VCell::number(2));
        assert_eq!(stack.pop(), Ok(&VCell::number(2)));
        assert_eq!(stack.pop(), Ok(&VCell::number(1)));
        assert_eq!(stack.pop(), Err(InvalidStackIndex(0)));
    }

    #[test]
    fn continuation() {
        let mut stack = Stack::new();
        stack.push(VCell::number(1));
        stack.push(VCell::number(2));
        stack.push(VCell::number(3));
        assert_eq!(stack.get_sp(), 3);
        let cont = stack.to_continuation();

        *stack.get_sp_mut() = 0;
        stack.push(VCell::number(4));
        stack.push(VCell::number(5));
        assert_eq!(stack.get_sp(), 2);
        assert_eq!(stack.get_offset(0), Ok(&VCell::number(5)));
        assert_eq!(stack.get_offset(-1), Ok(&VCell::number(4)));

        stack.restore_continuation(&cont);
        assert_eq!(stack.get_sp(), 3);
        assert_eq!(stack.get_offset(0), Ok(&VCell::number(3)));
        assert_eq!(stack.get_offset(-1), Ok(&VCell::number(2)));
        assert_eq!(stack.get_offset(-2), Ok(&VCell::number(1)));
    }
}
