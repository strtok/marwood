use crate::cell::Cell;
use crate::vm::environment::{free_symbols, BindingLocation};
use crate::vm::lambda::Lambda;
use crate::vm::opcode::OpCode;
use crate::vm::vcell::VCell;
use crate::vm::vcell::VCell::{BasePointerOffset, LexicalEnvSlot};
use crate::vm::Error::{
    InvalidArgs, InvalidNumArgs, InvalidSyntactic, LambdaMissingExpression, UnquotedNil,
};
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
    /// Compile
    ///
    /// Compile compiles a single top level expression, returning a Lambda that represents
    /// the top level expression of an Error if compilation failed.
    ///
    /// `expr` - The expression to compile.
    pub fn compile(&mut self, expr: &Cell) -> Result<Lambda, Error> {
        let mut entry_lambda = Lambda::new(vec![]);
        let mut lambda = Lambda::new_from_iof(vec![], &entry_lambda, &[], false);
        lambda.emit(OpCode::Enter);
        self.compile_expression(&mut lambda, true, expr)?;
        lambda.emit(OpCode::Ret);
        trace!("main: \n{}", self.decompile_text(&lambda));
        let lambda = self.heap.put(lambda);
        entry_lambda.emit(OpCode::PushImmediate);
        entry_lambda.emit(VCell::FixedNum(0));
        entry_lambda.emit(OpCode::MovImmediate);
        entry_lambda.emit(lambda);
        entry_lambda.emit(VCell::Acc);
        entry_lambda.emit(OpCode::CallAcc);
        entry_lambda.emit(OpCode::Halt);
        Ok(entry_lambda)
    }

    /// Compile Expression
    ///
    /// Compile expression compiles a single expression, emitting its byte code to the currently
    /// compiling procedure.
    ///
    /// `lambda` - The lambda to emit byte code to
    /// `expr` - The expression to compile.
    /// `tail` - Tail is true if this expression is in a tail position.
    pub fn compile_expression(
        &mut self,
        lambda: &mut Lambda,
        tail: bool,
        expr: &Cell,
    ) -> Result<(), Error> {
        match expr {
            Cell::Pair(car, cdr) => match car.deref() {
                Cell::Symbol(proc) => match proc.as_str() {
                    "define" => self.compile_define(lambda, expr),
                    "lambda" | "λ" => self.compile_lambda(lambda, expr, false),
                    "quote" => self.compile_quote(lambda, car!(cdr)),
                    "if" => self.compile_if(lambda, tail, expr),
                    _ => self.compile_procedure_application(lambda, tail, expr),
                },
                _ => self.compile_procedure_application(lambda, tail, expr),
            },
            Cell::Symbol(_) => self.compile_symbol_expression(lambda, expr),
            Cell::Nil => Err(UnquotedNil),
            Cell::Number(_)
            | Cell::Bool(_)
            | Cell::Void
            | Cell::Undefined
            | Cell::Closure
            | Cell::Lambda => self.compile_quote(lambda, expr),
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
        if sym.is_syntactic_keyword() {
            return Err(InvalidSyntactic(sym.to_string()));
        }
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
    /// A definition should be in one of the following forms:
    ///
    /// * (define ⟨variable⟩ ⟨expression⟩)
    /// * (define (⟨variable⟩ ⟨formals⟩) ⟨body⟩)
    /// * (define (⟨variable⟩ . ⟨formal⟩) ⟨body⟩)
    ///
    /// `lambda` - The lambda to emit bytecode to
    /// `expr` - (define variable expression)    
    pub fn compile_define(&mut self, lambda: &mut Lambda, expr: &Cell) -> Result<(), Error> {
        let rest = cdr!(expr);

        // A define must have at least 2 arguments
        if rest.is_nil() || cdr!(rest).is_nil() {
            return Err(InvalidNumArgs("define".into()));
        }

        // Extract the symbol given the form, and at the same time compile the
        // expression or lambda so that its result will be in %acc for the define.
        let symbol = match car!(rest) {
            Cell::Symbol(_) => {
                if !cdr!(cdr!(rest)).is_nil() {
                    return Err(InvalidNumArgs("define".into()));
                }
                self.compile_expression(lambda, false, car!(cdr!(rest)))?;
                car!(rest)
            }
            Cell::Pair(_, _) => {
                self.compile_lambda(lambda, expr, true)?;
                car!(car!(rest))
            }
            _ => {
                return Err(InvalidArgs(
                    "define".into(),
                    "symbol or (variable formals)".into(),
                    car!(rest).to_string(),
                ));
            }
        };

        if symbol.is_syntactic_keyword() {
            return Err(InvalidSyntactic(symbol.to_string()));
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
    /// A lambda should be the following form:
    ///     
    /// (lambda ⟨formals⟩ ⟨body⟩)
    ///   or
    /// (define ⟨variable formals⟩ ⟨body⟩)
    ///
    /// Where formals is one of the following:
    ///
    /// * (var1 var2 ...): A fixed number of arguments
    /// * var: A variable number of arguments, allocated in a new list bound to var.
    /// * (var1 var2 . rest): A fixed number of arguments, any additional arguments bound
    ///   to a new list bound to rest.
    ///
    /// # Arguments
    /// `iof_lambda` - The immediate outer function in which to inherit an
    ///                environment from
    /// `expr` - The full lambda expression, i.e. (lambda ...). This function takes the
    /// full expression so that free_symbols() considers this lambda when building
    /// an "environment".
    /// `is_define_special` - Is this a define special form?
    pub fn compile_lambda(
        &mut self,
        iof: &mut Lambda,
        expr: &Cell,
        is_define_special: bool,
    ) -> Result<(), Error> {
        let rest = cdr!(expr);
        if rest.is_nil() {
            return Err(InvalidNumArgs("procedure".into()));
        }

        // The position of formal args and body differ slightly
        // on whether this was a define special form or a lambda
        let (formal_args, body) = match is_define_special {
            true => (cdr!(car!(rest)), cdr!(rest)),
            false => (car!(rest), cdr!(rest)),
        };

        // Compile formal args into a list of symbols
        let (formal_args, is_vararg) = self.compile_formal_arguments(formal_args)?;
        let free_symbols = free_symbols(expr)?
            .iter()
            .map(|sym| self.heap.put_cell(sym))
            .collect::<Vec<VCell>>();

        let mut lambda = Lambda::new_from_iof(formal_args, iof, &free_symbols, is_vararg);
        if lambda.is_vararg {
            lambda.emit(OpCode::VarArg);
        }
        lambda.emit(OpCode::Enter);

        // Compile each body expression in sequence
        let mut body = body;
        if body.is_nil() {
            return Err(LambdaMissingExpression);
        }
        while body.is_pair() {
            self.compile_expression(&mut lambda, cdr!(body).is_nil(), car!(body))?;
            body = cdr!(body);
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
    /// `formal_args` - the formal arguments of the lambda or define
    ///
    /// # Returns
    /// (symbols, vararg) where vec is a vector of symbols, and vararg is true
    /// if a vararg form was encountered.
    pub fn compile_formal_arguments(
        &mut self,
        formal_args: &Cell,
    ) -> Result<(Vec<VCell>, bool), Error> {
        if formal_args.is_nil() {
            return Ok((vec![], false));
        }
        let mut symbols = vec![];
        let mut rest = formal_args;
        while rest.is_pair() {
            let symbol = car!(rest);
            if !symbol.is_symbol() {
                return Err(InvalidArgs(
                    "procedure".into(),
                    "symbol".into(),
                    symbol.to_string(),
                ));
            }
            if symbol.is_syntactic_keyword() {
                return Err(InvalidSyntactic(symbol.to_string()));
            }
            symbols.push(self.heap.put_cell(symbol));
            rest = cdr!(rest);
        }

        if rest.is_symbol() {
            if rest.is_syntactic_keyword() {
                return Err(InvalidSyntactic(rest.to_string()));
            }
            symbols.push(self.heap.put_cell(rest));
            Ok((symbols, true))
        } else {
            Ok((symbols, false))
        }
    }

    /// Compile Procedure Application
    ///
    /// Evaluate the argument expressions in expr, and then apply their
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
    /// `expr` - The procedure to apply
    /// `tail` - Tail is true if this procedure application is in a tail position.
    pub fn compile_procedure_application(
        &mut self,
        lambda: &mut Lambda,
        tail: bool,
        expr: &Cell,
    ) -> Result<(), Error> {
        let proc = car!(expr);
        let mut rest = cdr!(expr);
        // Evaluate and push each argument left-to-right
        let mut n: i64 = 0;
        while rest.is_pair() {
            self.compile_expression(lambda, false, rest.car().unwrap())?;
            lambda.emit(OpCode::PushAcc);
            n += 1;
            rest = rest.cdr().unwrap();
        }

        // Push the argument count
        lambda.emit(OpCode::PushImmediate);
        lambda.emit(VCell::FixedNum(n));

        // Evaluate the procedure to call, and emit a CALL instruction
        self.compile_expression(lambda, false, proc)?;
        lambda.emit(match tail {
            true => OpCode::TCallAcc,
            false => OpCode::CallAcc,
        });
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
    ///
    /// # Arguments
    /// `lambda` - The lambda to emit bytecode to
    /// `expr` - The if expression
    /// `tail` - Tail is true if this expression is in a tail position
    pub fn compile_if(
        &mut self,
        lambda: &mut Lambda,
        tail: bool,
        expr: &Cell,
    ) -> Result<(), Error> {
        let rest = cdr!(expr);
        if rest.is_nil() || !rest.is_list() {
            return Err(InvalidArgs("if".into(), "test".into(), rest.to_string()));
        }
        let (test, consequent, alternate) = match rest.collect_vec().as_slice() {
            [test, consequent] => (*test, *consequent, None),
            [test, consequent, alternate] => (*test, *consequent, Some(*alternate)),
            _ => {
                return Err(InvalidNumArgs("if".into()));
            }
        };

        // Evaluate test
        self.compile_expression(lambda, false, test)?;

        // JMP if %acc is #f
        lambda.emit(OpCode::Jnt);
        let jnt_operand = lambda.bc.len();
        lambda.emit(VCell::Ptr(0xCAFEBEEF));

        // Compile the consequent and update the JMP offset to be
        // the bytecode directly after the consequent. The consequent's
        // final instruction is a JMP to the end of the alternate.
        self.compile_expression(lambda, tail, consequent)?;
        lambda.emit(OpCode::Jmp);
        let jmp_operand = lambda.bc.len();
        lambda.emit(VCell::Ptr(0xCAFEBEEF));
        *lambda.bc.get_mut(jnt_operand).unwrap() = VCell::ptr(lambda.bc.len());

        // Compile the alternate, or if there is no alternate then evaluate to #<void>
        match alternate {
            Some(alternate) => {
                self.compile_expression(lambda, tail, alternate)?;
            }
            None => {
                lambda.emit(OpCode::MovImmediate);
                lambda.emit(self.heap.put(VCell::Void));
                lambda.emit(VCell::Acc);
            }
        }
        *lambda.bc.get_mut(jmp_operand).unwrap() = VCell::ptr(lambda.bc.len());
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
    /// `expr` - The expression to quote.
    pub fn compile_quote(&mut self, lambda: &mut Lambda, expr: &Cell) -> Result<(), Error> {
        lambda.emit(OpCode::MovImmediate);
        lambda.emit(self.heap.put_cell(expr));
        lambda.emit(VCell::Acc);
        Ok(())
    }
}
