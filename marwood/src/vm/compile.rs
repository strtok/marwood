use crate::cell::Cell;
use crate::vm::environment::{free_symbols, BindingLocation};
use crate::vm::lambda::Lambda;
use crate::vm::opcode::OpCode;
use crate::vm::vcell::VCell;
use crate::vm::vcell::VCell::{BasePointerOffset, LexicalEnvSlot};
use crate::vm::Error::{InvalidArgs, InvalidNumArgs, LambdaMissingExpression, UnquotedNil};
use crate::vm::{Error, Vm};
use log::trace;
use std::ops::Deref;

macro_rules! car {
    ($cell:expr) => {{
        $cell
            .car()
            .ok_or(Error::ExpectedPairButFound($cell.to_string()))?
    }};
}

macro_rules! cdr {
    ($cell:expr) => {{
        $cell
            .cdr()
            .ok_or(Error::ExpectedPairButFound($cell.to_string()))?
    }};
}

impl Vm {
    pub fn compile(&mut self, cell: &Cell) -> Result<Lambda, Error> {
        let mut lambda = Lambda::new(vec![]);
        self.compile_expression(&mut lambda, cell)?;
        lambda.emit(OpCode::Halt);
        Ok(lambda)
    }

    pub fn compile_expression(&mut self, lambda: &mut Lambda, cell: &Cell) -> Result<(), Error> {
        match cell {
            Cell::Pair(car, cdr) => match car.deref() {
                Cell::Symbol(proc) => match proc.as_str() {
                    "define" => self.compile_define(lambda, cdr),
                    "lambda" | "λ" => self.compile_lambda(lambda, cell),
                    "quote" => self.compile_quote(lambda, car!(cdr)),
                    "if" => self.compile_if(lambda, cdr),
                    "car" => self.compile_unary(OpCode::Car, "car", lambda, cdr),
                    "cdr" => self.compile_unary(OpCode::Cdr, "cdr", lambda, cdr),
                    "cons" => self.compile_arg2(OpCode::Cons, "cons", lambda, cdr),
                    "eq?" => self.compile_arg2(OpCode::Eq, "eq?", lambda, cdr),
                    "eqv?" => self.compile_arg2(OpCode::Eq, "eq?", lambda, cdr),
                    "not" => self.compile_unary(OpCode::Not, "not", lambda, cdr),
                    "+" => self.compile_var_arg(OpCode::Add, "+", lambda, cdr),
                    "-" => self.compile_var_arg(OpCode::Sub, "-", lambda, cdr),
                    "*" => self.compile_var_arg(OpCode::Mul, "*", lambda, cdr),
                    "boolean?" => self.compile_unary(OpCode::IsBoolean, proc, lambda, cdr),
                    "char?" => self.compile_unary(OpCode::IsChar, proc, lambda, cdr),
                    "list?" => self.compile_unary(OpCode::IsList, proc, lambda, cdr),
                    "number?" => self.compile_unary(OpCode::IsNumber, proc, lambda, cdr),
                    "null?" => self.compile_unary(OpCode::IsNull, proc, lambda, cdr),
                    "pair?" => self.compile_unary(OpCode::IsPair, proc, lambda, cdr),
                    "port?" => self.compile_unary(OpCode::IsPort, proc, lambda, cdr),
                    "procedure?" => self.compile_unary(OpCode::IsProcedure, proc, lambda, cdr),
                    "string?" => self.compile_unary(OpCode::IsString, proc, lambda, cdr),
                    "symbol?" => self.compile_unary(OpCode::IsSymbol, proc, lambda, cdr),
                    "vector?" => self.compile_unary(OpCode::IsVector, proc, lambda, cdr),
                    _ => self.compile_procedure_application(lambda, car, cdr),
                },
                _ => self.compile_procedure_application(lambda, car, cdr),
            },
            Cell::Symbol(_) => self.compile_symbol_expression(lambda, cell),
            Cell::Nil => Err(UnquotedNil),
            Cell::Number(_)
            | Cell::Bool(_)
            | Cell::Void
            | Cell::Undefined
            | Cell::Closure
            | Cell::Lambda => self.compile_quote(lambda, cell),
        }
    }

    /// Compile Symbol Expression
    ///
    /// Given the symbol in sym, evaluate a load from the global environment
    /// or lexical environment, given the binding location returned by lambda.
    ///
    /// `lambda` - The lambda to emit bytecode to
    /// `sym` - The symbol to evaluate
    pub fn compile_symbol_expression(
        &mut self,
        lambda: &mut Lambda,
        sym: &Cell,
    ) -> Result<(), Error> {
        let sym_ref = self.heap.put_cell(sym);
        match lambda.binding_location(&sym_ref) {
            BindingLocation::Global => {
                let sym_ref = sym_ref.as_ptr().expect("expected ptr");
                let env_slot = VCell::env_slot(self.globenv.get_binding(sym_ref));
                lambda.emit(OpCode::Mov);
                lambda.emit(env_slot);
                lambda.emit(VCell::Acc);
            }
            BindingLocation::Argument(n) => {
                let arg_offset = 0_i64 - lambda.argc() as i64 + n as i64 + 1;
                lambda.emit(OpCode::Mov);
                lambda.emit(BasePointerOffset(arg_offset));
                lambda.emit(VCell::Acc);
            }
            BindingLocation::Environment(n) => {
                lambda.emit(OpCode::Mov);
                lambda.emit(LexicalEnvSlot(n));
                lambda.emit(VCell::Acc);
            }
        }
        Ok(())
    }

    /// Compile Define
    ///
    /// (define variable expression)
    /// `lambda` - The lambda to emit bytecode to
    /// `lat` - (variable expression)    
    pub fn compile_define(&mut self, lambda: &mut Lambda, lat: &Cell) -> Result<(), Error> {
        if lat.is_nil() || !cdr!(cdr!(lat)).is_nil() {
            return Err(InvalidNumArgs("define".into()));
        }

        self.compile_expression(lambda, car!(cdr!(lat)))?;
        let symbol = car!(lat);
        if !symbol.is_symbol() {
            return Err(InvalidArgs(
                "define".into(),
                "variable".into(),
                symbol.to_string(),
            ));
        }
        let sym_ref = self.heap.put_cell(symbol).as_ptr()?;
        let env_slot = VCell::env_slot(self.globenv.get_binding(sym_ref));
        lambda.emit(OpCode::Mov);
        lambda.emit(VCell::Acc);
        lambda.emit(env_slot);
        lambda.emit(OpCode::MovImmediate);
        lambda.emit(VCell::void());
        lambda.emit(VCell::Acc);
        Ok(())
    }

    /// Compile Lambda
    ///
    /// Compile a lambda expression (lambda (arg0 arg1 ...) expr1 expr2 ...)
    ///
    /// # Arguments
    /// `iof_lambda` - The immediate outer function in which to inherit an
    ///                environment from
    /// `cell` - The lambda expression.
    pub fn compile_lambda(&mut self, iof: &mut Lambda, expr: &Cell) -> Result<(), Error> {
        let lat = cdr!(expr);
        let formal_args = self.compile_formal_arguments(lat)?;
        let free_symbols = free_symbols(expr)?
            .iter()
            .map(|sym| self.heap.put_cell(sym))
            .collect::<Vec<VCell>>();
        let mut lambda = Lambda::new_from_iof(formal_args, iof, &free_symbols);
        lambda.emit(OpCode::Enter);

        let mut lat = cdr!(lat);
        if lat.is_nil() {
            return Err(LambdaMissingExpression);
        }
        while lat.is_pair() {
            self.compile_expression(&mut lambda, car!(lat))?;
            lat = cdr!(lat);
        }
        lambda.emit(OpCode::Ret);
        trace!("lambda: \n{}", self.decompile_text(&lambda));
        let lambda = self.heap.put(lambda);
        iof.emit(OpCode::MovImmediate);
        iof.emit(lambda);
        iof.emit(VCell::Acc);
        iof.emit(OpCode::ClosureAcc);
        Ok(())
    }

    /// Compile Formal Arguments
    ///
    /// Given a lambda call, extract the formal arguments, place the symbols
    /// on the heap, and return a vector of symbol pointers for each argument.
    ///
    /// # Arguments
    /// `lat` - the remainder of the lambda call, where car(lat) is the formal
    ///         argument list.
    pub fn compile_formal_arguments(&mut self, lat: &Cell) -> Result<Vec<VCell>, Error> {
        let mut args = match lat.car() {
            Some(Cell::Nil) => return Ok(vec![]),
            Some(cell) if cell.is_pair() => Ok(cell),
            Some(cell) => Err(InvalidArgs(
                "lambda".into(),
                "formal argument list".into(),
                cell.to_string(),
            )),
            _ => Err(InvalidArgs(
                "lambda".into(),
                "formal argument list".into(),
                lat.to_string(),
            )),
        }?;

        let mut symbols = vec![];
        while args.is_pair() {
            let arg = car!(args);
            if !arg.is_symbol() {
                return Err(InvalidArgs(
                    "lambda".into(),
                    "symbol".into(),
                    arg.to_string(),
                ));
            }
            symbols.push(self.heap.put_cell(arg));
            args = cdr!(args);
        }

        Ok(symbols)
    }

    /// Compile Procedure Application
    ///
    /// Evaluate the argument expressions in lat, and then apply their
    /// results to proc.
    ///
    /// Procedure calls are in the form:
    ///```example
    ///     (proc arg1 arg2 arg3 ...)
    ///```
    /// Proecure application is as follows:
    ///
    /// 1. Evaluate and push the arguments left-to-right, resulting in
    /// the last argument.
    /// 2. Push the number of arguments on the stack.
    /// 3. Evaluate proc, the result of which will be in ACC.
    /// 4. Emit a CALL instruction that will execute the procedure in ACC.
    ///
    /// # Arguments
    /// `lambda` - The lambda to emit bytecode to
    /// `proc` - The procedure to apply
    /// `lat` - Zero or more expressions to apply to proc as arguments.
    pub fn compile_procedure_application(
        &mut self,
        lambda: &mut Lambda,
        proc: &Cell,
        lat: &Cell,
    ) -> Result<(), Error> {
        // Evaluate and push each argument left-to-right
        let mut n: i64 = 0;
        let mut lat = lat;
        while lat.is_pair() {
            self.compile_expression(lambda, lat.car().unwrap())?;
            lambda.emit(OpCode::PushAcc);
            n += 1;
            lat = lat.cdr().unwrap();
        }

        // Push the argument count
        lambda.emit(OpCode::PushImmediate);
        lambda.emit(VCell::FixedNum(n));

        // Evaluate the procedure to call, and emit a CALL instruction
        self.compile_expression(lambda, proc)?;
        lambda.emit(OpCode::CallAcc);
        Ok(())
    }

    /// Compile If
    ///
    /// Compile an if conditional, which is either one of the following forms:
    ///
    /// * (if ⟨test⟩ ⟨consequent⟩ ⟨alternate⟩)
    /// * (if ⟨test⟩ ⟨consequent⟩)
    ///
    /// If has a few special rules:
    /// * The consequent and alternate expressions may only be evaluated if their
    ///   branch is chosen.
    /// * If test yields false without an alternate, the result is unspecified. Marwood
    ///   will evaluate the `if` expression to #<void>
    pub fn compile_if(&mut self, lambda: &mut Lambda, lat: &Cell) -> Result<(), Error> {
        if lat.is_nil() || !lat.is_list() {
            return Err(InvalidArgs("if".into(), "test".into(), lat.to_string()));
        }
        let (test, consequent, alternate) = match lat.collect_vec().as_slice() {
            [test, consequent] => (*test, *consequent, None),
            [test, consequent, alternate] => (*test, *consequent, Some(*alternate)),
            _ => {
                return Err(InvalidNumArgs("if".into()));
            }
        };

        // Evaluate test
        self.compile_expression(lambda, test)?;

        // JMP if %acc is #f
        lambda.emit(OpCode::Jnt);
        let jnt_operand = lambda.bc.len();
        lambda.emit(VCell::Ptr(0xCAFEBEEF));

        // Compile the consequent and update the JMP offset to be
        // the bytecode directly after the consequent. The consequent's
        // final instruction is a JMP to the end of the alternate.
        self.compile_expression(lambda, consequent)?;
        lambda.emit(OpCode::Jmp);
        let jmp_operand = lambda.bc.len();
        lambda.emit(VCell::Ptr(0xCAFEBEEF));
        *lambda.bc.get_mut(jnt_operand).unwrap() = VCell::ptr(lambda.bc.len());

        // Compile the alternate, or if there is no alternate then evaluate to #<void>
        match alternate {
            Some(alternate) => {
                self.compile_expression(lambda, alternate)?;
            }
            None => {
                lambda.emit(OpCode::MovImmediate);
                lambda.emit(VCell::Void);
                lambda.emit(VCell::Acc);
            }
        }
        *lambda.bc.get_mut(jmp_operand).unwrap() = VCell::ptr(lambda.bc.len());
        Ok(())
    }

    /// Compile Unary
    ///
    /// Compile a procedure that takes one argument (ACC)
    ///
    /// # Arguments
    /// `op` - The opcode operating on the argument
    /// `name` - The name of the procedure, used for error purposes.
    /// `lambda` - The lambda to emit bytecode to
    /// `lat` - The argument to the procedure
    pub fn compile_unary(
        &mut self,
        op: OpCode,
        name: &str,
        lambda: &mut Lambda,
        lat: &Cell,
    ) -> Result<(), Error> {
        if lat.is_nil() || !cdr!(lat).is_nil() {
            return Err(InvalidNumArgs(name.into()));
        }
        self.compile_expression(lambda, car!(lat))?;
        lambda.emit(op);
        Ok(())
    }

    /// Compile Arg 2
    ///
    /// Compile a procedure that takes two arguments (ACC, Arg[0])
    ///
    /// # Arguments
    /// `op` - The opcode operating on the two arguments
    /// `name` - The name of the procedure, used for error purposes.
    /// `lambda` - The lambda to emit bytecode to
    /// `lat` - The arguments to the procedure
    pub fn compile_arg2(
        &mut self,
        op: OpCode,
        name: &str,
        lambda: &mut Lambda,
        lat: &Cell,
    ) -> Result<(), Error> {
        if lat.is_nil() || !cdr!(cdr!(lat)).is_nil() {
            return Err(InvalidNumArgs(name.into()));
        }
        self.compile_expression(lambda, car!(cdr!(lat)))?;
        lambda.emit(OpCode::PushAcc);
        self.compile_expression(lambda, car!(lat))?;
        lambda.emit(op);
        Ok(())
    }

    /// Compile Quote
    ///
    /// Quote is compiled as a single argument instruction (QUOTE VAL). Quote is
    /// special in that the value in cell is not evaluated before being placed
    /// on the heap.
    ///
    /// # Arguments
    /// `lambda` - The lambda to emit bytecode to
    /// `cell` - The value to quote.
    pub fn compile_quote(&mut self, lambda: &mut Lambda, cell: &Cell) -> Result<(), Error> {
        lambda.emit(OpCode::MovImmediate);
        lambda.emit(self.heap.put_cell(cell));
        lambda.emit(VCell::Acc);
        Ok(())
    }

    /// Compile Var Arg
    ///
    /// Compile a procedure that takes zero or more arguments, using the provided
    /// instruction to fold the result of every two arguments with the next.
    ///
    /// Some opcodes may require -at least one- argument, and will return an error
    /// if at least one argument is not provided.
    ///
    /// # Arguments
    /// `op` - The opcode operating on the list of arguments
    /// `name` - The name of the procedure, used for error purposes.
    /// `lambda` - The lambda to emit bytecode to
    /// `lat` - The arguments to the procedure
    pub fn compile_var_arg(
        &mut self,
        op: OpCode,
        name: &str,
        lambda: &mut Lambda,
        lat: &Cell,
    ) -> Result<(), Error> {
        let mut lat = lat;

        let base_value = match op {
            OpCode::Mul => &Cell::Number(1),
            _ => &Cell::Number(0),
        };

        // Special zero arg form. Evaluate to the base value.
        if lat.is_nil() {
            if op == OpCode::Sub {
                return Err(InvalidNumArgs(name.to_string()));
            }
            self.compile_quote(lambda, base_value)?;
            return Ok(());
        }

        let first_arg = car!(lat);
        lat = cdr!(lat);

        // Special one arg form. Add it to 0 so that type
        // checking from the ADD instruction still occurs.
        if lat.is_nil() {
            self.compile_quote(lambda, base_value)?;
            lambda.emit(OpCode::PushAcc);
            self.compile_expression(lambda, first_arg)?;
            lambda.emit(op);
            return Ok(());
        }

        // Each additional arg is added to ACC
        self.compile_expression(lambda, first_arg)?;
        while !lat.is_nil() {
            lambda.emit(OpCode::PushAcc);
            self.compile_expression(lambda, car!(lat))?;
            lambda.emit(op.clone());
            lat = cdr!(lat);
        }

        Ok(())
    }
}
