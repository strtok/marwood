use crate::cell::Cell;
use crate::vm::environment::{free_symbols, internally_defined_symbols, BindingLocation};
use crate::vm::lambda::Lambda;
use crate::vm::opcode::OpCode;
use crate::vm::transform::Transform;
use crate::vm::vcell::VCell;
use crate::vm::vcell::VCell::{BasePointerOffset, LexicalEnvSlot};
use crate::vm::Error::{
    InvalidArgs, InvalidNumArgs, InvalidSyntax, InvalidUsePrimitive, LambdaMissingExpression,
    UnquotedNil,
};
use crate::vm::{Error, Vm};
use log::trace;
use std::ops::Deref;
use std::rc::Rc;

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
    /// Compile Runnable
    ///
    /// Compile compiles a single top level expression, returning a Lambda that represents
    /// the top level expression of an Error if compilation failed.
    ///
    /// `expr` - The expression to compile.
    pub fn compile_runnable(&mut self, expr: &Cell) -> Result<Lambda, Error> {
        let mut entry_lambda = Lambda::new(vec![]);
        let mut lambda = Lambda::new_from_iof(vec![], vec![], &entry_lambda, &[], false);
        lambda.set_top_level();
        lambda.emit(OpCode::Enter);
        self.compile(&mut lambda, true, expr)?;
        lambda.emit(OpCode::Ret);
        trace!("main: \n{}", self.decompile_text(&lambda));
        let lambda = self.heap.put(lambda);
        entry_lambda.emit(OpCode::PushImmediate);
        entry_lambda.emit(VCell::ArgumentCount(0));
        entry_lambda.emit(OpCode::MovImmediate);
        entry_lambda.emit(lambda);
        entry_lambda.emit(VCell::Acc);
        entry_lambda.emit(OpCode::CallAcc);
        entry_lambda.emit(OpCode::Halt);
        Ok(entry_lambda)
    }

    /// Compile
    ///
    /// Apply any pre-compilation transforms to expr, compile it, and return
    /// the resulting bytecode.
    ///
    /// # Arguments
    /// `expr` - The expression to compile.
    pub fn compile(&mut self, lambda: &mut Lambda, tail: bool, expr: &Cell) -> Result<(), Error> {
        trace!("transforming: {}", expr);
        let expr = self.transform(expr)?;
        trace!("compilnig: {}", expr);
        self.compile_expression(lambda, tail, &expr)?;
        Ok(())
    }

    /// Transform
    ///
    /// Apply pre-compilation transforms to expr, returning the transformed
    /// AST.
    pub fn transform(&mut self, expr: &Cell) -> Result<Cell, Error> {
        match expr {
            Cell::Pair(_, _) => self.transform_procedure_application(expr),
            cell => Ok(cell.clone()),
        }
    }

    pub fn transform_procedure_application(&mut self, expr: &Cell) -> Result<Cell, Error> {
        let proc = expr.car().unwrap();
        let mut rest = expr.cdr().unwrap();

        match proc.deref() {
            Cell::Symbol(proc) => match proc.as_str() {
                "quote" | "define-syntax" => return Ok(expr.clone()),
                _ => {}
            },
            _ => {}
        }

        if let Some(sym) = self.heap.get_sym_ref(proc) {
            let vcell = match self.globenv.get(sym.as_ptr()?) {
                Some(VCell::Ptr(ptr)) => Some(self.heap.get_at_index(ptr).clone()),
                vcell => vcell,
            };
            if let Some(VCell::Macro(transform)) = vcell {
                let expansion = transform.transform(expr)?;
                trace!("macro expansion: {} => {}", expr, expansion);
                return self.transform(&expansion);
            }
        }

        let mut v = vec![self.transform(proc)?];
        while rest.is_pair() {
            v.push(self.transform(rest.car().unwrap())?);
            rest = rest.cdr().unwrap();
        }
        if rest.is_nil() {
            Ok(Cell::new_list(v))
        } else {
            let rest = self.transform(rest)?;
            Ok(Cell::new_improper_list(v, rest))
        }
    }

    /// Compile Expression
    ///
    /// Compile expression compiles a single expression, emitting its byte code to the currently
    /// compiling procedure.
    ///
    /// # Arguments
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
            Cell::Pair(_, _) => self.compile_procedure_application(lambda, tail, expr),
            Cell::Symbol(_) => self.compile_symbol_expression(lambda, expr),
            Cell::Nil => Err(UnquotedNil),
            Cell::Bool(_)
            | Cell::Continuation
            | Cell::Char(_)
            | Cell::Number(_)
            | Cell::Macro
            | Cell::String(_)
            | Cell::Undefined
            | Cell::Vector(_)
            | Cell::Void
            | Cell::Procedure(_) => self.compile_quote(lambda, expr),
        }
    }

    /// Compile Procedure Application
    ///
    /// Apply the given procedure by either:
    /// * Expanding a macro if proc is a macro in the current
    ///   environment
    /// * Emitting inlined bytecode for built-in primitives, such
    ///   as lambda and define
    /// * Compiling a runtime procedure application
    ///
    /// # Arguments
    /// `lambda` - The lambda to emit byte code to
    /// `tail` - Tail is true if this expression is in a tail position.
    /// `expr` - The expression to compile.
    pub fn compile_procedure_application(
        &mut self,
        lambda: &mut Lambda,
        tail: bool,
        expr: &Cell,
    ) -> Result<(), Error> {
        let proc = expr.car().unwrap();
        let rest = expr.cdr().unwrap();
        match proc.deref() {
            Cell::Symbol(proc) => match proc.as_str() {
                "define" => self.compile_define(lambda, expr),
                "define-syntax" => self.compile_define_syntax(lambda, expr),
                "lambda" | "λ" => self.compile_lambda(lambda, expr, false),
                "quote" => self.compile_quote(lambda, car!(rest)),
                "if" => self.compile_if(lambda, tail, expr),
                "set!" => self.compile_set(lambda, tail, expr),
                _ => self.compile_runtime_procedure_application(lambda, tail, expr),
            },
            _ => self.compile_runtime_procedure_application(lambda, tail, expr),
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
        if sym.is_primitive_symbol() {
            return Err(InvalidUsePrimitive(sym.to_string()));
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

        if symbol.is_primitive_symbol() {
            return Err(InvalidUsePrimitive(symbol.to_string()));
        }

        let sym_ref = self.heap.put_cell(symbol);
        lambda.emit(OpCode::Mov);
        lambda.emit(VCell::Acc);
        match lambda.binding_location(&sym_ref) {
            BindingLocation::Global => {
                let sym_ref = sym_ref.as_ptr().expect("expected ptr");
                let env_slot = VCell::env_slot(self.globenv.get_binding(sym_ref));
                lambda.emit(env_slot);
            }
            BindingLocation::Argument(n) => {
                let arg_offset = 0_i64 - lambda.argc() as i64 + n as i64 + 1;
                lambda.emit(BasePointerOffset(arg_offset));
            }
            BindingLocation::Environment(n) => {
                lambda.emit(LexicalEnvSlot(n));
            }
        }

        lambda.emit(OpCode::MovImmediate);
        lambda.emit(VCell::void());
        lambda.emit(VCell::Acc);
        Ok(())
    }

    /// Set
    ///
    /// Set is a primitive that provides support for the set! procedure.
    ///
    /// A set! must be in the following form:
    ///
    /// * (set ⟨variable⟩ ⟨expression⟩)
    ///
    /// `lambda` - The lambda to emit bytecode to
    /// `expr` - (set! variable expression)
    /// `tail` - whether or not this set is in a tail context
    pub fn compile_set(
        &mut self,
        lambda: &mut Lambda,
        _tail: bool,
        expr: &Cell,
    ) -> Result<(), Error> {
        let rest = cdr!(expr);
        let (variable, expression) = match rest.collect_vec().as_slice() {
            [variable, expression] => (*variable, *expression),
            _ => {
                return Err(InvalidNumArgs("set!".into()));
            }
        };

        if !variable.is_symbol() || variable.is_primitive_symbol() {
            return Err(InvalidSyntax(format!(
                "expected variable, but got {}",
                variable
            )));
        }

        self.compile_expression(lambda, false, expression)?;

        let sym_ref = self.heap.put_cell(variable);
        lambda.emit(OpCode::Mov);
        lambda.emit(VCell::Acc);
        match lambda.binding_location(&sym_ref) {
            BindingLocation::Global => {
                let sym_ref = sym_ref.as_ptr().expect("expected ptr");
                let env_slot = VCell::env_slot(self.globenv.get_binding(sym_ref));
                lambda.emit(env_slot);
            }
            BindingLocation::Argument(n) => {
                let arg_offset = 0_i64 - lambda.argc() as i64 + n as i64 + 1;
                lambda.emit(BasePointerOffset(arg_offset));
            }
            BindingLocation::Environment(n) => {
                lambda.emit(LexicalEnvSlot(n));
            }
        }

        lambda.emit(OpCode::MovImmediate);
        lambda.emit(self.heap.put(VCell::Void));
        lambda.emit(VCell::Acc);
        Ok(())
    }

    /// Compile Define Syntax
    ///
    /// Compile a top-level macro to a VCell::Transform, storing it in the
    /// heap.
    ///
    /// `lambda` - The lambda to emit bytecode to
    /// `expr` - (define variable expression)    
    pub fn compile_define_syntax(&mut self, lambda: &mut Lambda, expr: &Cell) -> Result<(), Error> {
        let transform = Transform::try_new(expr)?;
        let symbol = transform.keyword().clone();
        let transform = self.heap.put(VCell::Macro(Rc::new(transform)));

        let sym_ref = self.heap.put_cell(&symbol).as_ptr()?;
        let env_slot = VCell::env_slot(self.globenv.get_binding(sym_ref));

        lambda.emit(OpCode::MovImmediate);
        lambda.emit(transform);
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
        let (formal_args_ast, body) = match is_define_special {
            true => (cdr!(car!(rest)), cdr!(rest)),
            false => (car!(rest), cdr!(rest)),
        };

        // Compile formal args into a list of symbols
        let (formal_args, is_vararg) = self.compile_formal_arguments(formal_args_ast)?;
        let free_symbols = free_symbols(expr)?
            .iter()
            .inspect(|it| trace!("free: {}", it))
            .map(|sym| self.heap.put_cell(sym))
            .collect::<Vec<VCell>>();
        let internally_defined = internally_defined_symbols(body)?
            .iter()
            .inspect(|it| trace!("internal: {}", it))
            .map(|sym| self.heap.put_cell(sym))
            .collect::<Vec<VCell>>();

        let mut lambda = Lambda::new_from_iof(
            formal_args,
            internally_defined,
            iof,
            &free_symbols,
            is_vararg,
        );
        lambda.set_desc(formal_args_ast.clone());
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
            if symbol.is_primitive_symbol() {
                return Err(InvalidUsePrimitive(symbol.to_string()));
            }
            symbols.push(self.heap.put_cell(symbol));
            rest = cdr!(rest);
        }

        if rest.is_symbol() {
            if rest.is_primitive_symbol() {
                return Err(InvalidUsePrimitive(rest.to_string()));
            }
            symbols.push(self.heap.put_cell(rest));
            Ok((symbols, true))
        } else {
            Ok((symbols, false))
        }
    }

    /// Compile Runtime Procedure Application
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
    pub fn compile_runtime_procedure_application(
        &mut self,
        lambda: &mut Lambda,
        tail: bool,
        expr: &Cell,
    ) -> Result<(), Error> {
        let proc = car!(expr);
        let mut rest = cdr!(expr);
        // Evaluate and push each argument left-to-right
        let mut n = 0;
        while rest.is_pair() {
            self.compile_expression(lambda, false, rest.car().unwrap())?;
            lambda.emit(OpCode::PushAcc);
            n += 1;
            rest = rest.cdr().unwrap();
        }

        // Push the argument count
        lambda.emit(OpCode::PushImmediate);
        lambda.emit(VCell::ArgumentCount(n));

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
