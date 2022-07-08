use crate::cell::Cell;
use crate::error::Error;
use crate::error::Error::{
    InvalidBytecode, InvalidNumArgs, InvalidProcedure, InvalidSyntax, VariableNotBound,
};
use crate::vm::environment::{BindingSource, EnvironmentMap, LexicalEnvironment};
use crate::vm::lambda::Lambda;
use crate::vm::opcode::OpCode;
use crate::vm::trace::StackTrace;
use crate::vm::vcell::VCell;
use crate::vm::vcell::VCell::LexicalEnvPtr;
use crate::vm::Vm;
use log::trace;
use std::rc::Rc;

impl Vm {
    /// Run
    ///
    /// Run the virtual machine until it encounters a HALT instruction,
    /// and return the value contained within the ACC register as a Cell
    pub fn run(&mut self) -> Result<Cell, Error> {
        self.run_count(usize::MAX).map(|it| it.unwrap())
    }

    pub fn run_count(&mut self, count: usize) -> Result<Option<Cell>, Error> {
        self.last_stacktrace = None;
        let mut cycles = 0;
        loop {
            cycles += 1;
            if cycles % 8192 == 0 {
                self.run_gc();
            }
            if cycles == count {
                self.run_gc();
                return Ok(None);
            }
            match self.run_one() {
                Ok(true) => break,
                Ok(false) => continue,
                Err(e) => {
                    self.last_stacktrace = Some(StackTrace::new(
                        &self.stack,
                        &self.heap,
                        self.ip,
                        self.acc.clone(),
                    ));
                    return Err(e);
                }
            }
        }
        trace!("cycles: {}", cycles);
        let cell = self.heap.get_as_cell(&self.acc);
        self.stack.clear();
        self.run_gc();
        Ok(Some(cell))
    }

    /// Run One
    ///
    /// Execute one instruction, returning either a bool or runtime error.
    /// If the bool is set true, it indicates a HALT was encountered and
    /// execution should cease.
    fn run_one(&mut self) -> Result<bool, Error> {
        self.trace_instruction();
        let op_code = self.read_opcode()?;
        match op_code {
            // Virtual Machine Instructions
            //
            // The instructions in this section are abstract virtual machine instructions
            // that provide support for the scheme VM. Operations such as stack manipulation,
            // loading and storing via MOV, and HALT.
            OpCode::Jmp => {
                self.ip.1 = self.read_operand()?.as_ptr()?;
            }
            OpCode::Jnt => {
                let offset = self.read_operand()?.as_ptr()?;
                if let VCell::Bool(false) = self.heap.get(&self.acc) {
                    self.ip.1 = offset;
                }
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
            OpCode::Halt => return Ok(true),
            // Procedure Application
            //
            // The opcodes CLOSURE, CALL, ENTER and RET are related to creation and application
            // of procedures.
            //
            // * CLOSURE creates a Closure object, which ties together a Lambda and LexicalEnvironment
            // * CALL applies a procedure, setting up the call frame from the caller's point of view
            // * ENTER is the first instruction of a procedure, and finishes setting up a call frame
            //   from the procedure's point of view
            //
            //
            OpCode::ClosureAcc => {
                let lambda_ptr = self.acc.as_ptr()?;
                let lambda = self.heap.get_at_index(lambda_ptr).as_lambda()?;

                // Build the lexical environment
                let lexical_env = self.build_closure_environment(&lambda.envmap)?;
                let lexical_env = VCell::LexicalEnv(Rc::new(lexical_env));
                let lexical_env_ptr = self.heap.put(lexical_env).as_ptr()?;

                // Build a Closure object on the heap
                let closure_ptr = self.heap.put(VCell::Closure(lambda_ptr, lexical_env_ptr));
                self.acc = closure_ptr;
            }
            OpCode::CallAcc => {
                let lambda = match self.heap.get(&self.acc) {
                    VCell::Closure(lambda, _) => lambda,
                    VCell::Lambda(_) => self.acc.as_ptr()?,
                    VCell::BuiltInProc(proc) => {
                        let proc = proc.as_ref();
                        self.acc = match proc.eval(self)? {
                            VCell::Ptr(ptr) => VCell::Ptr(ptr),
                            vcell => self.heap.maybe_put(vcell),
                        };
                        return Ok(false);
                    }
                    VCell::Continuation(cont) => {
                        let cont = &*cont;
                        if self.stack.pop()?.as_argc()? == 0 {
                            return Err(InvalidSyntax("expected value".into()));
                        }
                        let result = self.stack.pop()?.clone();
                        self.restore_continuation(cont);
                        self.acc = result;
                        return Ok(false);
                    }
                    other => {
                        return Err(InvalidProcedure(self.heap.get_as_cell(&other)));
                    }
                };
                self.stack.push(VCell::EnvironmentPointer(self.ep));
                self.stack
                    .push(VCell::InstructionPointer(self.ip.0, self.ip.1));
                self.ip.0 = lambda;
                self.ip.1 = 0;
            }
            OpCode::TCallAcc => {
                let lambda = match self.heap.get(&self.acc) {
                    VCell::Closure(lambda, _) => lambda,
                    VCell::Lambda(_) => self.acc.as_ptr()?,
                    VCell::BuiltInProc(proc) => {
                        let proc = proc.as_ref();
                        self.acc = match proc.eval(self)? {
                            VCell::Ptr(ptr) => VCell::Ptr(ptr),
                            vcell => self.heap.maybe_put(vcell),
                        };
                        return Ok(false);
                    }
                    VCell::Continuation(cont) => {
                        let cont = &*cont;
                        if self.stack.pop()?.as_argc()? == 0 {
                            return Err(InvalidSyntax("expected value".into()));
                        }
                        let result = self.stack.pop()?.clone();
                        self.restore_continuation(cont);
                        self.acc = result;
                        return Ok(false);
                    }
                    other => {
                        return Err(InvalidProcedure(self.heap.get_as_cell(&other)));
                    }
                };

                let argc = self.stack.get_offset(0)?.as_argc()?;
                let frame_argc = self.stack.get(self.bp + 1)?.as_argc()?;

                if argc == frame_argc {
                    let saved_bp = self.stack.get(self.bp + 4)?.clone();
                    for it in 0..argc {
                        let val = self.stack.get_offset(-1_i64 - it as i64)?.clone();
                        *self.stack.get_mut(self.bp - it)? = val;
                    }
                    *self.stack.get_sp_mut() = self.bp + 3;
                    self.bp = saved_bp.as_bp()?;
                    self.ip.0 = lambda;
                    self.ip.1 = 0;
                } else {
                    let saved_sp = self.stack.get_sp();
                    let saved_ep = self.stack.get(self.bp + 2)?.clone();
                    let saved_ip = self.stack.get(self.bp + 3)?.clone();
                    let saved_bp = self.stack.get(self.bp + 4)?.clone();

                    *self.stack.get_sp_mut() = self.bp - frame_argc;
                    for it in (0..argc).rev() {
                        let val = self.stack.get(saved_sp - it - 1)?.clone();
                        self.stack.push(val);
                    }

                    self.stack.push(VCell::ArgumentCount(argc));
                    self.stack.push(saved_ep);
                    self.stack.push(saved_ip);
                    self.bp = saved_bp.as_bp()?;
                    self.ip.0 = lambda;
                    self.ip.1 = 0;
                }
            }
            OpCode::Enter => {
                let (lambda, closure_env) = match self.heap.get(&self.acc) {
                    VCell::Closure(lambda, lexical_env) => (lambda, Some(lexical_env)),
                    VCell::Lambda(_) => (self.acc.as_ptr()?, None),
                    _ => {
                        return Err(InvalidBytecode);
                    }
                };

                let lambda = self.heap.get_at_index(lambda);
                let lambda = lambda.as_lambda()?;
                if self.stack.get_offset(-2)?.as_argc()? != lambda.args.len() {
                    return Err(InvalidNumArgs(lambda.to_string()));
                }

                self.stack.push(VCell::BasePointer(self.bp));
                self.bp = self.stack.get_sp() - 4;

                if let Some(closure_env_ptr) = closure_env {
                    let closure_env = self.heap.get_at_index(closure_env_ptr).as_lexical_env()?;
                    let lexical_env =
                        self.build_lexical_environment(lambda, closure_env_ptr, closure_env)?;
                    let lexical_env_ptr = self
                        .heap
                        .put(VCell::LexicalEnv(Rc::new(lexical_env)))
                        .as_ptr()?;
                    self.ep = lexical_env_ptr;
                }
            }
            OpCode::Ret => {
                let n = self.stack.get(self.bp + 1)?.as_argc()?;

                *self.stack.get_sp_mut() = self.bp - n;
                self.ep = self.stack.get(self.bp + 2)?.as_ep()?;
                self.ip = self.stack.get(self.bp + 3)?.as_ip()?;
                self.bp = self.stack.get(self.bp + 4)?.as_bp()?;
            }
            OpCode::VarArg => {
                // VARARG converts the optional arguments of a vararg procedure into a list.
                // This requires popping off every optional argument, forming a list on the heap,
                // and placing the list at the top of the argument stack.
                let req_argc = self.lambda().args.len() - 1;
                let argc = self.stack.get_offset(-2)?.as_argc()?;
                if argc < req_argc {
                    return Err(InvalidNumArgs("procedure".into()));
                }

                // If there's exactly one vararg, then we can convert it in place
                if argc == req_argc + 1 {
                    let arg = self.heap.put(self.stack.get_offset(-3)?.clone());
                    let nil = self.heap.put(VCell::Nil);
                    *self.stack.get_offset_mut(-3)? =
                        self.heap.put(VCell::Pair(arg.as_ptr()?, nil.as_ptr()?));
                } else {
                    // Save the frame data that CALL put on the stack
                    let saved_ep = self.stack.pop()?.clone();
                    let saved_ip = self.stack.pop()?.clone();
                    let _ = self.stack.pop()?.clone();

                    // Pop each optional arg into a list
                    let varargc = argc - req_argc;
                    let mut varargs = self.heap.put(VCell::Nil).as_ptr()?;
                    for _ in 0..varargc {
                        let arg = self.heap.put(self.stack.pop()?.clone());
                        let pair = VCell::Pair(arg.as_ptr()?, varargs);
                        varargs = self.heap.put(pair).as_ptr()?;
                    }

                    // Push the list on the stack, a new argc, and restore the caller's
                    // frame
                    self.stack.push(VCell::ptr(varargs));
                    self.stack.push(VCell::ArgumentCount(req_argc + 1));
                    self.stack.push(saved_ip);
                    self.stack.push(saved_ep);
                }
            }
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
            VCell::GlobalEnvSlot(slot) => match self.globenv.get_symbol(slot) {
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

    /// Lambda
    ///
    /// Dereference the currently executing lambda
    fn lambda(&self) -> &Lambda {
        self.heap
            .get_at_index(self.ip.0)
            .as_lambda()
            .expect("%ip is not a procedure")
    }

    /// Read Arg
    ///
    /// Read an argument vcell from program[ip], increment ip and
    /// return the value.
    fn read_operand(&mut self) -> Result<VCell, Error> {
        let proc = self.lambda();
        match proc.get(self.ip.1) {
            Some(opand) if opand.is_opcode() => Err(InvalidBytecode),
            Some(opand) => {
                let opand = opand.clone();
                self.ip.1 += 1;
                Ok(opand)
            }
            None => Err(InvalidBytecode),
        }
    }

    /// Deref Arg
    ///
    /// Read an argument and return the value that it references.
    /// If the operand is not a reference type, return InvalidBytecode
    fn load_operand(&mut self) -> Result<VCell, Error> {
        match self.read_operand()? {
            VCell::Acc => Ok(self.acc.clone()),
            VCell::Ptr(ptr) => Ok(self.heap.get_at_index(ptr).clone()),
            VCell::BasePointerOffset(offset) => {
                Ok(self.stack.get((self.bp as i64 + offset) as usize)?.clone())
            }
            VCell::GlobalEnvSlot(slot) => match self.globenv.get_slot(slot) {
                VCell::Undefined => Err(VariableNotBound(
                    self.get_str_bound_to(VCell::env_slot(slot)),
                )),
                cell => Ok(cell),
            },
            VCell::LexicalEnvSlot(slot) => Ok(
                match self.heap.get_at_index(self.ep).as_lexical_env()?.get(slot) {
                    VCell::LexicalEnvPtr(env, slot) => {
                        self.heap.get_at_index(env).as_lexical_env()?.get(slot)
                    }
                    cell => cell,
                },
            ),
            _ => Err(InvalidBytecode),
        }
    }

    /// Store Using Operand
    ///
    /// Read an operand and use it as a destination to store the
    /// given vcell.
    fn store_operand(&mut self, vcell: VCell) -> Result<(), Error> {
        match self.read_operand()? {
            VCell::Acc => {
                self.acc = vcell;
            }
            VCell::Ptr(ptr) => {
                *self.heap.get_at_index_mut(ptr) = vcell;
            }
            VCell::BasePointerOffset(offset) => {
                *self.stack.get_offset_mut((self.bp as i64) + offset)? = vcell;
            }
            VCell::GlobalEnvSlot(slot) => {
                self.globenv.put_slot(slot, vcell);
            }
            VCell::LexicalEnvSlot(slot) => {
                let lexical_env = self.heap.get_at_index(self.ep).as_lexical_env()?;
                match lexical_env.get(slot) {
                    VCell::LexicalEnvPtr(env, slot) => {
                        self.heap
                            .get_at_index(env)
                            .as_lexical_env()?
                            .put(slot, vcell);
                    }
                    _ => {
                        lexical_env.put(slot, vcell);
                    }
                }
            }
            _ => return Err(InvalidBytecode),
        }
        Ok(())
    }

    /// Read Op
    ///
    /// Read an op code from program[ip], increment ip and
    /// return the opcode.
    fn read_opcode(&mut self) -> Result<OpCode, Error> {
        let proc = self.lambda();
        let op = proc.get(self.ip.1);
        match op {
            Some(op) => {
                let op = op.as_opcode()?;
                self.ip.1 += 1;
                Ok(op)
            }
            None => Err(InvalidBytecode),
        }
    }

    /// Load Arg
    ///
    /// Load an argument from the current stack frame given the argument
    /// index.
    fn load_arg(&self, index: usize) -> Result<&VCell, Error> {
        let arg_count = self.stack.get(self.bp + 1)?.as_argc()?;
        let offset = self.bp;
        let offset = (offset - arg_count) + index + 1;
        self.stack.get(offset)
    }

    /// Run GC
    ///
    /// Run GC performs two steps in order:
    ///
    /// 1. Check if heap utilization is > 75%, aborting gc is not.
    ///
    /// 2. It performs a mark on all roots:
    ///    * The global environment
    ///    * Any data referecned by the running program & stack
    ///
    /// 3. A sweep, freeing any vcells not marked as used in step #1.
    pub fn run_gc(&mut self) {
        if (self.heap.used_size() as f64 / self.heap.capacity() as f64) < 0.75_f64 {
            return;
        }

        self.globenv
            .iter_bindings()
            .for_each(|it| self.heap.mark(*it));

        self.globenv
            .iter_slots()
            .filter_map(|it| it.as_ptr().ok())
            .for_each(|it| self.heap.mark(it));

        self.stack
            .iter_to_sp()
            .for_each(|it| self.heap.mark_vcell(it));
        self.heap.mark_vcell(&self.acc);
        self.heap.mark(self.ip.0);
        self.heap.mark(self.ep);
        self.heap.sweep();

        // If after GC the heap utilization is still high, grow the heap.
        if (self.heap.used_size() as f64 / self.heap.capacity() as f64) > 0.75_f64 {
            self.heap.grow();
        }
    }

    /// Build Closure Environment
    ///
    /// Build a lexical environment with the given environment map, assuming
    /// that the current stack is that of the IOF. This function is called by
    /// the CLOSURE instruction as part of creation of a lambda.
    ///
    /// # Arguments
    /// `envmap` - The EnvironmentMap used to build the lexical environment
    fn build_closure_environment(
        &self,
        envmap: &EnvironmentMap,
    ) -> Result<LexicalEnvironment, Error> {
        let lexical_env = LexicalEnvironment::new(envmap.slots_len());
        for (slot, it) in envmap.get_map().iter().enumerate() {
            match it.1 {
                BindingSource::IofArgument(arg) => {
                    lexical_env.put(slot, self.load_arg(arg)?.clone());
                }
                BindingSource::IofEnvironment(iof_slot) => {
                    let iof_env = self.heap.get_at_index(self.ep).as_lexical_env()?;
                    match iof_env.get(iof_slot) {
                        VCell::LexicalEnvPtr(_, _) => lexical_env.put(slot, iof_env.get(iof_slot)),
                        _ => lexical_env.put(slot, VCell::LexicalEnvPtr(self.ep, iof_slot)),
                    };
                }
                // Argument bindings aren't available until CALL/TCALL, and internal definitions are not
                // defined until the lambda's body executes.
                BindingSource::Global
                | BindingSource::Argument(_)
                | BindingSource::InternalDefinition => {}
            }
        }
        Ok(lexical_env)
    }

    /// Chain Lexical Environment
    ///
    /// Given a closure's environment, produce a cloned environment that chains
    /// to the original environment. This function is called by the ENTER instruction
    /// when applying a function.
    ///
    /// # Arguments
    /// `lambda` - The lambda being applied by the ENTER instruction.
    /// `env_ptr` - A pointer to the base environment of lambda.
    /// `env` - The base environment.
    fn build_lexical_environment(
        &self,
        lambda: &Lambda,
        closure_env_ptr: usize,
        closure_env: &LexicalEnvironment,
    ) -> Result<LexicalEnvironment, Error> {
        let lexical_env = closure_env.clone();
        for (slot, it) in lambda.envmap.get_map().iter().enumerate() {
            match it.1 {
                BindingSource::Argument(arg) => {
                    let arg_offset = self.bp - (lambda.argc() - arg) + 1;
                    lexical_env.put(slot, self.stack.get(arg_offset)?.clone());
                }
                BindingSource::IofArgument(_) | BindingSource::IofEnvironment(_) => {
                    match closure_env.get(slot) {
                        VCell::LexicalEnvPtr(_, _) => {}
                        _ => lexical_env.put(slot, LexicalEnvPtr(closure_env_ptr, slot)),
                    }
                }
                BindingSource::Global | BindingSource::InternalDefinition => {}
            }
        }
        Ok(lexical_env)
    }

    /// Pop
    ///
    /// Pop is a wrapper around vm.stack.pop(), providing automatic dereference
    /// if the popped value is a heap reference.
    pub fn pop(&mut self) -> Result<VCell, Error> {
        match self.stack.pop()? {
            VCell::Ptr(ptr) => Ok(self.heap.get_at_index(*ptr).clone()),
            vcell => Ok(vcell.clone()),
        }
    }

    #[allow(dead_code)]
    fn trace_environment_map(&self, envmap: &EnvironmentMap) {
        trace!("--- ENVIRONMENT MAP ---");
        envmap.get_map().iter().for_each(|it| {
            trace!("{} => {:?}", self.get_str_bound_to(it.0.clone()), it.1);
        });
        trace!("--- END MAP ---");
    }

    #[allow(dead_code)]
    fn trace_lexical_environment(&self, env: &LexicalEnvironment) {
        trace!("--- LEXICAL ENVIRONMENT ---");
        for it in 0..env.slot_len() {
            trace!("{} => {:?}", it, env.get(it));
        }
        trace!("--- END LEXICAL ENVIRONMENT ---");
    }

    #[cfg(not(debug_assertions))]
    fn trace_instruction(&self) {}

    #[cfg(debug_assertions)]
    fn trace_instruction(&self) {
        trace!(
            "{:<60} {:>30}",
            format!(
                "{}",
                self.decompile_one(
                    &mut self.heap.get_at_index(self.ip.0).as_lambda().unwrap().bc[self.ip.1..]
                        .iter()
                        .peekable()
                )
                .unwrap()
            ),
            format!(
                "%acc={} {} %sp[{}] {} $ep[{}]",
                self.acc,
                VCell::InstructionPointer(self.ip.0, self.ip.1),
                VCell::Ptr(self.stack.get_sp()),
                VCell::BasePointer(self.bp),
                VCell::EnvironmentPointer(self.ep)
            )
        );
    }
}

#[cfg(test)]
mod tests {}
