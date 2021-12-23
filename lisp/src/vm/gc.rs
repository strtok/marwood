/// GcMap
///
/// GcMap is a map used to track the disposition of objects on a
/// heap during mark&sweep garbage collection.
///
/// The map tracks three possible states for each element in a heap:
/// * Free
/// * Allocated
/// * Used
///
/// The initial state for a node is free (0x0, and then when allocated
/// the state is transitioned to Allocated.
pub struct Map {
    size: usize,
    map: Vec<u8>,
}

/// State
///
/// State represents the disposition of an object on the heap in regards
/// to garbage collection.
///
/// Free: The object is not in use, and is on the free list ready to be
///       allocated.
///
/// Allocated: The object is allocated and believed to be in use. It is
///       subject to garbage collection checks.
///
/// Used: The object has been marked during garbage collection. This is an
///       ephemeral state used during GC before the object is either marked
///       allocated again, or free.
#[derive(Debug, Eq, PartialEq, Clone)]
#[repr(u8)]
pub enum State {
    Free,
    Allocated,
    Used,
}

impl State {
    pub fn bits(&self) -> u8 {
        match self {
            State::Free => 0b00,
            State::Allocated => 0b01,
            State::Used => 0b10,
        }
    }
}

impl From<u8> for State {
    fn from(val: u8) -> State {
        match val {
            0b00 => State::Free,
            0b01 => State::Allocated,
            0b10 => State::Used,
            _ => panic!("invalid gc state"),
        }
    }
}

impl Map {
    /// New
    ///
    /// Construct a new GcMap of len elements, initializing every element
    /// to the value 0 (free).
    pub fn new(size: usize) -> Map {
        Map {
            size,
            map: vec![0; size],
        }
    }

    /// Get
    ///
    /// Given the index into the map, retrieve the state of the object,
    /// or None if the index was out of bounds.
    ///
    /// # Arguments
    /// `index` - The index to retrieve the state for.
    pub fn get(&self, index: usize) -> Option<State> {
        self.map
            .get(index / 4)
            .map(|it| (it >> ((index % 4) * 2)) & 0b11)
            .map(|it| it.into())
    }

    /// Set
    ///
    /// Given the index into the map, set the state to the value given.
    ///
    /// Panic if the index is invalid.
    ///
    /// # Arguments
    /// `index` - The index to mutate
    /// `state` - The state to set at the index
    pub fn set(&mut self, index: usize, state: State) {
        match self.map.get_mut(index / 4) {
            Some(byte) => {
                let mask: u8 = 0b11 << ((index % 4) * 2);
                let flag: u8 = state.bits() << ((index % 4) * 2);
                *byte &= !mask;
                *byte |= flag;
            }
            None => panic!("invalid gc index"),
        }
    }

    pub fn capacity(&self) -> usize {
        self.size
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn initial_state_is_free() {
        let gcmap = Map::new(8);
        assert_eq!(gcmap.size, 8);
        for it in 0..32 {
            assert_eq!(gcmap.get(it), Some(State::Free));
        }
        assert_eq!(gcmap.get(32), None);
    }

    #[test]
    fn mutation() {
        let mut gcmap = Map::new(8);
        gcmap.set(0, State::Used);
        gcmap.set(1, State::Allocated);
        gcmap.set(15, State::Used);
        assert_eq!(gcmap.get(0), Some(State::Used));
        assert_eq!(gcmap.get(15), Some(State::Used));
        assert_eq!(gcmap.get(1), Some(State::Allocated));
        assert_eq!(gcmap.get(2), Some(State::Free));
    }
}
