use crate::vm::lambda::Binding::Global;
use crate::vm::vcell::VCell;

/// Lambda
///
/// Lambda represents a unit of executable bytecode constructed
/// by the compiler with an entry point of bc[0].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lambda {
    pub args: Vec<VCell>,
    pub bc: Vec<VCell>,
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
        Lambda { args, bc: vec![] }
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

    /// Binding
    ///
    /// Return the binding for the given symbol
    pub fn binding(&self, sym: &VCell) -> Binding {
        match self.args.iter().enumerate().find(|it| it.1 == sym) {
            Some((it, _)) => Binding::Argument(it),
            _ => Global,
        }
    }

    /// Argument Count
    ///
    /// Return the number of arguments
    pub fn argc(&self) -> usize {
        self.args.len()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Binding {
    Global,
    Argument(usize),
}

impl From<Vec<VCell>> for Lambda {
    fn from(bc: Vec<VCell>) -> Self {
        Lambda { args: vec![], bc }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lambda_binding() {
        let lambda = Lambda::new(vec![VCell::ptr(100), VCell::ptr(200)]);
        assert_eq!(lambda.binding(&VCell::ptr(100)), Binding::Argument(0));
        assert_eq!(lambda.binding(&VCell::ptr(200)), Binding::Argument(1));
        assert_eq!(lambda.binding(&VCell::ptr(300)), Binding::Global);
    }
}
