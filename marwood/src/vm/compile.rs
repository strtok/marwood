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
        $cell.car().ok_or(Error::ExpectedPair($cell.to_string()))?
    }};
}

macro_rules! cdr {
    ($cell:expr) => {{
        $cell.cdr().ok_or(Error::ExpectedPair($cell.to_string()))?
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
                Cell::Symbol(s) if s.eq("define") => self.compile_define(lambda, cdr),
                Cell::Symbol(s) if s.eq("lambda") | s.eq("λ") => self.compile_lambda(lambda, cell),
                Cell::Symbol(s) if s.eq("quote") => self.compile_quote(lambda, car!(cdr)),
                Cell::Symbol(s) if s.eq("car") => {
                    self.compile_unary(OpCode::Car, "car", lambda, cdr)
                }
                Cell::Symbol(s) if s.eq("cdr") => {
                    self.compile_unary(OpCode::Cdr, "cdr", lambda, cdr)
                }
                Cell::Symbol(s) if s.eq("cons") => {
                    self.compile_arg2(OpCode::Cons, "cons", lambda, cdr)
                }
                Cell::Symbol(s) if s.eq("eq?") => self.compile_arg2(OpCode::Eq, "eq?", lambda, cdr),
                Cell::Symbol(s) if s.eq("+") => self.compile_var_arg(OpCode::Add, "+", lambda, cdr),
                Cell::Symbol(s) if s.eq("-") => self.compile_var_arg(OpCode::Sub, "-", lambda, cdr),
                Cell::Symbol(s) if s.eq("*") => self.compile_var_arg(OpCode::Mul, "*", lambda, cdr),
                _ => self.compile_procedure_application(lambda, car, cdr),
            },
            Cell::Symbol(_) => self.compile_symbol_lookup(lambda, cell),
            Cell::Nil => Err(UnquotedNil),
            Cell::Number(_)
            | Cell::Bool(_)
            | Cell::Void
            | Cell::Undefined
            | Cell::Closure
            | Cell::Lambda => self.compile_quote(lambda, cell),
        }
    }

    /// Compile Symbol Eval
    ///
    /// Given the symbol in sym, compile to a ENVGET instruction to fetch
    /// the value bound to the symbol.
    ///
    /// `lambda` - The lambda to emit bytecode to
    /// `sym` - The symbol to evaluate
    pub fn compile_symbol_lookup(&mut self, lambda: &mut Lambda, sym: &Cell) -> Result<(), Error> {
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
        let sym_ref =
            self.heap.put_cell(symbol).as_ptr().ok_or_else(|| {
                InvalidArgs("define".into(), "variable".into(), symbol.to_string())
            })?;
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
