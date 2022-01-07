use crate::cell::Cell;
use crate::vm::opcode::OpCode;
use crate::vm::run::RuntimeError::{
    ExpectedPair, ExpectedStackValue, InvalidArgs, InvalidBytecode, InvalidNumArgs,
    InvalidProcedure, VariableNotBound,
};
use crate::vm::vcell::{Lambda, VCell};
use crate::vm::{Error, Vm};

impl Vm {
    /// Run
    ///
    /// Run the virtual machine until it encounters a HALT instruction,
    /// and return the value contained within the ACC register as a Cell
    pub fn run(&mut self) -> Result<Cell, Error> {
        loop {
            match self.run_one() {
                Ok(true) => break,
                Ok(false) => continue,
                Err(e) => return Err(e.into_error(self)),
            }
        }
        let cell = self.heap.get_as_cell(&self.acc);
        self.run_gc();
        Ok(cell)
    }

    /// Execute one instruction, returning either a bool or runtime error.
    /// If the bool is set true, it indicates a HALT was encountered and
    /// execution should cease.
    fn run_one(&mut self) -> Result<bool, RuntimeError> {
        let op_code = self.read_opcode()?;
        match op_code {
            OpCode::Add | OpCode::Sub | OpCode::Mul => {
                let proc_name = match op_code {
                    OpCode::Add => "+",
                    OpCode::Sub => "-",
                    OpCode::Mul => "*",
                    _ => return Err(InvalidBytecode),
                };
                let x = self.heap.get(&self.acc);
                let y = self.pop_stack()?;
                let y = self.heap.get(&y);
                let x = x.as_fixed_num().ok_or_else(|| {
                    InvalidArgs(proc_name.to_string(), "number".to_string(), x.clone())
                })?;
                let y = y.as_fixed_num().ok_or_else(|| {
                    InvalidArgs(proc_name.to_string(), "number".to_string(), y.clone())
                })?;
                match op_code {
                    OpCode::Add => self.acc = self.heap.put(VCell::fixed_num(x + y)),
                    OpCode::Sub => self.acc = self.heap.put(VCell::fixed_num(y - x)),
                    OpCode::Mul => self.acc = self.heap.put(VCell::fixed_num(x * y)),
                    _ => return Err(InvalidBytecode),
                };
            }
            OpCode::CallAcc => {
                // Push the current environment pointer
                self.stack.push(VCell::Ptr(usize::MAX));
                // Push return IP
                self.stack
                    .push(VCell::InstructionPointer(self.ip.0, self.ip.1));
                // Verify the procedure is a procedure
                if !self.heap.get(&self.acc).is_procedure() {
                    return Err(InvalidProcedure(self.heap.get(&self.acc)));
                }
                // Jump to ip=0 of lambda in ACC
                self.ip.0 = self.acc.as_ptr().ok_or(InvalidBytecode)?;
                self.ip.1 = 0;
            }
            OpCode::Car => {
                let arg = self.heap.get(&self.acc);
                self.acc = car(arg)?;
            }
            OpCode::Cdr => {
                let arg = self.heap.get(&self.acc);
                self.acc = cdr(arg)?;
            }
            OpCode::Cons => {
                let car = self.acc.clone().as_ptr().ok_or(InvalidBytecode)?;
                let cdr = self.pop_stack()?.as_ptr().ok_or(InvalidBytecode)?;
                self.acc = self.heap.put(VCell::Pair(car, cdr));
            }
            OpCode::Enter => {
                // Enter performs runtime validation of the called procedure,
                // and sets up the local stack frame.
                //
                // At this point the stack should appear as follows:
                //  0: Return IP
                // -1: Last Frame EP
                // -2: Number of arguments passed by calee
                // -3: ArgN...
                //
                // Get a reference to the lambda being entered, so that
                // certain runtime type checks can be performed.
                let lambda = &self.acc;
                let lambda = self.heap.get(lambda);
                let lambda = lambda
                    .as_lambda()
                    .ok_or_else(|| InvalidProcedure(lambda.clone()))?;

                // Verify that the number of args passed (SP[-2]) is
                // equal to the argument expectations of the lambda
                if self
                    .stack
                    .get_offset(-2)
                    .ok_or(InvalidBytecode)?
                    .as_fixed_num()
                    .ok_or(InvalidBytecode)? as usize
                    != lambda.args.len()
                {
                    return Err(InvalidNumArgs);
                }

                // At this point the stack should appear as follows:
                //  0: Last Frame's BP
                // -1: Return IP
                // -2: Last Frame EP
                // -3: Number of arguments passed by calee
                // -4: ArgN...
                //
                // Push the old BP on to the stack, and set BP
                // so that BP[0] is ArgN.
                self.stack.push(VCell::BasePointer(self.bp));
                self.bp = self.stack.get_sp() - 4;
            }
            OpCode::EnvGet => {
                let env_slot = self
                    .read_operand()?
                    .as_env_slot()
                    .expect("expected env slot");
                self.acc = self.globenv.get_slot(env_slot);
                if self.acc == VCell::Undefined {
                    return Err(VariableNotBound(
                        self.get_str_bound_to(VCell::env_slot(env_slot)),
                    ));
                }
            }
            OpCode::EnvSet => {
                let env_slot = self
                    .read_operand()?
                    .as_env_slot()
                    .expect("expected env slot");
                self.globenv.put_slot(env_slot, self.acc.clone());
                self.acc = self.heap.put(VCell::void());
            }
            OpCode::Eq => {
                let arg = self.pop_stack()?;
                self.acc = self.heap.put(self.acc == arg);
            }
            OpCode::Mov => {
                let vcell = self.load_operand()?;
                self.store_operand(vcell)?;
            }
            OpCode::MovImmediate => {
                let vcell = self.read_operand()?;
                self.store_operand(vcell)?;
            }
            OpCode::Push => {
                let vcell = self.load_operand()?;
                self.stack.push(vcell);
            }
            OpCode::PushImmediate => {
                let vcell = self.read_operand()?;
                self.stack.push(vcell);
            }
            OpCode::PushAcc => {
                self.stack.push(self.acc.clone());
            }
            OpCode::Ret => {
                // Restore the calee's SP given the argument
                // count.
                let n = self
                    .stack
                    .get(self.bp + 1)
                    .ok_or(InvalidBytecode)?
                    .as_fixed_num()
                    .ok_or(InvalidBytecode)? as usize;
                *self.stack.get_sp_mut() = self.bp - n;

                // Restore the calee's IP
                self.ip = self
                    .stack
                    .get(self.bp + 3)
                    .ok_or(InvalidBytecode)?
                    .as_ip()
                    .ok_or(InvalidBytecode)?;

                // Restore the calee's BP
                self.bp = self
                    .stack
                    .get(self.bp + 4)
                    .ok_or(InvalidBytecode)?
                    .as_bp()
                    .ok_or(InvalidBytecode)?;
            }
            OpCode::Halt => return Ok(true),
        }

        Ok(false)
    }

    /// Get Symbol Bound To
    ///
    /// Given either an environment slot, or a symbol reference, return the
    /// original string bound to the reference.
    ///
    /// The runtime cost of this function is > O(1), but it's only needed
    /// during an error path or debugging.
    ///
    /// # Arguments
    /// `vcell` - The vcell containing an environment slot of sym reference
    /// to provide a reverse lookup for.
    ///
    /// # Returns
    /// Returns the bound symbol, or "#<undefined>" if symbol lookup failed
    /// for any reason.
    pub fn get_str_bound_to<T: Into<VCell>>(&self, vcell: T) -> String {
        let vcell = vcell.into();
        match vcell {
            VCell::EnvSlot(slot) => match self.globenv.get_symbol(slot) {
                Some(sym_ref) => self.get_str_bound_to(VCell::Ptr(sym_ref)),
                None => "#<undefined>".into(),
            },
            VCell::Ptr(_) => self
                .heap
                .get(&vcell)
                .as_symbol()
                .unwrap_or("#<undefined>")
                .into(),
            _ => "#<undefined>".into(),
        }
    }

    /// Pop Stack
    ///
    /// Pop a value off the stack. Error if the stack is empty.
    fn pop_stack(&mut self) -> Result<VCell, RuntimeError> {
        self.stack
            .pop()
            .ok_or(ExpectedStackValue)
            .map(|it| it.clone())
    }

    /// Lambda
    ///
    /// Dereference the currently executing lambda
    fn lambda(&self) -> &Lambda {
        self.heap
            .get_at_index(self.ip.0)
            .as_lambda()
            .expect("expected lambda")
    }

    /// Read Arg
    ///
    /// Read an argument vcell from program[ip], increment ip and
    /// return the value.
    fn read_operand(&mut self) -> Result<VCell, RuntimeError> {
        let operand = self
            .lambda()
            .get(self.ip.1)
            .cloned()
            .filter(|it| !it.is_opcode())
            .ok_or(InvalidBytecode);
        self.ip.1 += 1;
        operand
    }

    /// Deref Arg
    ///
    /// Read an argument and return the value that it references.
    /// If the operand is not a reference type, return InvalidBytecode
    fn load_operand(&mut self) -> Result<VCell, RuntimeError> {
        match self.read_operand()? {
            VCell::Acc => Ok(self.acc.clone()),
            VCell::Ptr(ptr) => Ok(self.heap.get_at_index(ptr).clone()),
            VCell::EnvSlot(slot) => Ok(self.heap.get(&self.globenv.get_slot(slot))),
            _ => Err(InvalidBytecode),
        }
    }

    /// Store Using Operand
    ///
    /// Read an operand and use it as a destination to store the
    /// given vcell.
    fn store_operand(&mut self, vcell: VCell) -> Result<(), RuntimeError> {
        match self.read_operand()? {
            VCell::Acc => {
                self.acc = vcell;
            }
            VCell::Ptr(ptr) => {
                *self.heap.get_at_index_mut(ptr) = vcell;
            }
            VCell::EnvSlot(slot) => {
                self.globenv.put_slot(slot, vcell);
            }
            _ => return Err(InvalidBytecode),
        }
        Ok(())
    }

    /// Read Op
    ///
    /// Read an op code from program[ip], increment ip and
    /// return the opcode.
    fn read_opcode(&mut self) -> Result<OpCode, RuntimeError> {
        let op = self
            .lambda()
            .get(self.ip.1)
            .cloned()
            .filter(|it| it.is_opcode())
            .map(|it| it.as_opcode().unwrap())
            .ok_or(InvalidBytecode);
        self.ip.1 += 1;
        op
    }

    /// Run GC
    ///
    /// Run GC performs two steps in order:
    ///
    /// 1. It performs a mark on all roots:
    ///    * The global environment
    ///    * Any data referecned by the running program & stack
    ///
    /// 2. A sweep, freeing any vcells not marked as used in step #1.
    pub fn run_gc(&mut self) {
        self.globenv
            .iter_bindings()
            .for_each(|it| self.heap.mark(*it));

        self.globenv
            .iter_slots()
            .filter_map(|it| it.as_ptr())
            .for_each(|it| self.heap.mark(it));

        self.heap.mark(self.ip.0);
        self.heap.sweep();
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum RuntimeError {
    ExpectedPair(VCell),
    InvalidArgs(String, String, VCell),
    InvalidBytecode,
    InvalidNumArgs,
    InvalidProcedure(VCell),
    ExpectedStackValue,
    VariableNotBound(String),
}

impl RuntimeError {
    /// Into Error
    ///
    /// Convert a runtime error into a vm::Error, converting any runtime values
    /// (e.g. ptrs to heap values) to printable forms -- This function requires
    /// read only access to the Vm in order to access the heap.
    fn into_error(self, vm: &Vm) -> Error {
        match self {
            ExpectedPair(vcell) => Error::ExpectedPair(vm.heap.get_as_cell(&vcell).to_string()),
            InvalidArgs(proc, expected, got) => {
                Error::InvalidArgs(proc, expected, vm.heap.get_as_cell(&got).to_string())
            }
            InvalidBytecode => Error::InvalidBytecode,
            InvalidNumArgs => Error::InvalidNumArgs("procedure".into()),
            InvalidProcedure(vcell) => {
                Error::InvalidProcedure(vm.heap.get_as_cell(&vcell).to_string())
            }
            ExpectedStackValue => Error::ExpectedStackValue,
            VariableNotBound(sym) => Error::VariableNotBound(sym),
        }
    }
}

fn car<T: AsRef<VCell>>(vcell: T) -> Result<VCell, RuntimeError> {
    vcell
        .as_ref()
        .as_car()
        .ok_or_else(|| ExpectedPair(vcell.as_ref().clone()))
}

fn cdr<T: AsRef<VCell>>(vcell: T) -> Result<VCell, RuntimeError> {
    vcell
        .as_ref()
        .as_cdr()
        .ok_or_else(|| ExpectedPair(vcell.as_ref().clone()))
}

#[cfg(test)]
mod tests {}
