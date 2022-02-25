use crate::cell::Cell;
use crate::vm::environment::{BindingLocation, EnvironmentMap};
use crate::vm::vcell::VCell;

/// Lambda
///
/// Lambda represents a unit of executable bytecode constructed
/// by the compiler with an entry point of bc[0].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lambda {
    pub top_level: bool,
    pub is_vararg: bool,
    pub envmap: EnvironmentMap,
    pub args: Vec<VCell>,
    pub bc: Vec<VCell>,
    pub desc: Option<Cell>,
}

impl Lambda {
    /// New
    ///
    /// Create a new lambda with an empty bytecode vector
    /// and the given argument list.
    ///
    /// # Arguments
    /// `args` - A vector of VCell::Ptr, each guaranteed to
    ///          point to a symbol representing a formal argument.
    pub fn new(args: Vec<VCell>) -> Lambda {
        Lambda {
            top_level: false,
            is_vararg: false,
            envmap: EnvironmentMap::new(),
            args,
            bc: vec![],
            desc: None,
        }
    }

    /// New From IOF
    ///
    /// Create a new lambda, and populate its environment map
    /// given the IOF and set of free symbols.
    pub fn new_from_iof(
        args: Vec<VCell>,
        internally_defined: Vec<VCell>,
        iof: &Lambda,
        free_symbols: &[VCell],
        is_vararg: bool,
    ) -> Lambda {
        let envmap = EnvironmentMap::new_from_iof(&args, &internally_defined, iof, free_symbols);
        Lambda {
            top_level: false,
            args,
            is_vararg,
            envmap,
            bc: vec![],
            desc: None,
        }
    }

    pub fn set_top_level(&mut self) {
        self.top_level = true;
    }
    pub fn is_top_level(&self) -> bool {
        self.top_level
    }
    pub fn set_desc(&mut self, cell: Cell) {
        let cell = Cell::from(vec![Cell::from("λ"), cell]);
        self.desc = Some(cell);
    }

    /// Get
    ///
    /// Get the opcode or operand at the given index
    pub fn get(&self, index: usize) -> Option<&VCell> {
        self.bc.get(index)
    }

    /// Emit
    ///
    /// Emit the byte code to the internal bc vector. This
    /// method is used during compilation when forming a lambda.
    pub fn emit<T: Into<VCell>>(&mut self, vcell: T) {
        self.bc.push(vcell.into());
    }

    /// Binding Location
    ///
    /// Return the binding for the given symbol. First check the lexical environment.
    /// It's possible the arguments were copied into the lexical environment in case they're
    /// needed by inner procedures, or for other reasons such as set!.
    ///
    /// If the symbol is neither in the lexical environmenr or a known argument then it's
    /// globally bound.
    ///
    /// # Arguments
    /// `sym` - The symbol to lookup the binding location of.
    pub fn binding_location(&self, sym: &VCell) -> BindingLocation {
        if let Some(slot) = self.envmap.get_slot(sym) {
            BindingLocation::Environment(slot)
        } else if let Some((arg, _)) = self.args.iter().enumerate().find(|it| it.1 == sym) {
            BindingLocation::Argument(arg)
        } else {
            BindingLocation::Global
        }
    }

    /// Argument Count
    ///
    /// Return the number of arguments
    pub fn argc(&self) -> usize {
        self.args.len()
    }
}

impl From<Vec<VCell>> for Lambda {
    fn from(bc: Vec<VCell>) -> Self {
        Lambda {
            top_level: false,
            envmap: EnvironmentMap::new(),
            args: vec![],
            is_vararg: false,
            bc,
            desc: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lambda_binding() {
        let lambda = Lambda::new(vec![VCell::ptr(100), VCell::ptr(200)]);
        assert_eq!(
            lambda.binding_location(&VCell::ptr(100)),
            BindingLocation::Argument(0)
        );
        assert_eq!(
            lambda.binding_location(&VCell::ptr(200)),
            BindingLocation::Argument(1)
        );
        assert_eq!(
            lambda.binding_location(&VCell::ptr(300)),
            BindingLocation::Global
        );
    }
}
