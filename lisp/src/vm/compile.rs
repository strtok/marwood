use crate::cell::Cell;
use crate::vm::node::Node;
use crate::vm::opcode::OpCode;
use crate::vm::Error::{InvalidArgs, InvalidNumArgs};
use crate::vm::{Error, Vm};
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
    pub fn compile(&mut self, cell: &Cell) -> Result<Vec<Node>, Error> {
        let mut bc = vec![];
        self.compile_expression(&mut bc, cell)?;
        bc.push(Node::from(OpCode::Halt));
        Ok(bc)
    }

    pub fn compile_expression(&mut self, bc: &mut Vec<Node>, cell: &Cell) -> Result<(), Error> {
        match cell {
            Cell::Pair(car, cdr) => match car.deref() {
                Cell::Symbol(s) if s.eq("define") => self.compile_define(bc, cdr)?,
                Cell::Symbol(s) if s.eq("quote") => self.compile_quote(bc, car!(cdr))?,
                Cell::Symbol(s) if s.eq("car") => self.compile_car(bc, cdr)?,
                Cell::Symbol(s) if s.eq("cdr") => self.compile_cdr(bc, cdr)?,
                Cell::Symbol(s) if s.eq("cons") => self.compile_cons(bc, cdr)?,
                Cell::Symbol(s) if s.eq("eq?") => self.compile_eq(bc, cdr)?,
                Cell::Symbol(s) if s.eq("+") => self.compile_operator(bc, cdr, OpCode::Add)?,
                Cell::Symbol(s) if s.eq("-") => self.compile_operator(bc, cdr, OpCode::Sub)?,
                Cell::Symbol(s) if s.eq("*") => self.compile_operator(bc, cdr, OpCode::Mul)?,
                _ => return Err(Error::UnknownProcedure(car.to_string())),
            },
            Cell::Symbol(_) => self.compile_symbol_lookup(bc, cell)?,
            Cell::Number(_) | Cell::Bool(_) | Cell::Nil | Cell::Void | Cell::Undefined => {
                self.compile_quote(bc, cell)?
            }
        };
        Ok(())
    }

    pub fn compile_symbol_lookup(&mut self, bc: &mut Vec<Node>, sym: &Cell) -> Result<(), Error> {
        let sym_ref = self.heap.put_cell(sym);
        let sym_ref = self
            .heap
            .get(&sym_ref)
            .as_symbol_reference()
            .expect("expected symbol");
        let env_slot = Node::env_slot(self.globenv.get_binding(sym_ref));
        bc.push(OpCode::EnvGet.into());
        bc.push(env_slot);
        Ok(())
    }

    /// Compile Define
    ///
    /// (define variable expression)
    pub fn compile_define(&mut self, bc: &mut Vec<Node>, lat: &Cell) -> Result<(), Error> {
        if lat.is_nil() || !cdr!(cdr!(lat)).is_nil() {
            return Err(InvalidNumArgs("define".into()));
        }

        self.compile_expression(bc, car!(cdr!(lat)))?;
        let symbol = car!(lat);
        let sym_ref = self.heap.put_cell(symbol);
        let sym_ref = self
            .heap
            .get(&sym_ref)
            .as_symbol_reference()
            .ok_or_else(|| InvalidArgs("define".into(), "variable".into(), symbol.to_string()))?;
        let env_slot = Node::env_slot(self.globenv.get_binding(sym_ref));
        bc.push(OpCode::EnvSet.into());
        bc.push(env_slot);
        Ok(())
    }

    pub fn compile_eq(&mut self, bc: &mut Vec<Node>, lat: &Cell) -> Result<(), Error> {
        if lat.is_nil() || !cdr!(cdr!(lat)).is_nil() {
            return Err(InvalidNumArgs("eq?".into()));
        }
        self.compile_expression(bc, car!(cdr!(lat)))?;
        bc.push(OpCode::Push.into());
        self.compile_expression(bc, car!(lat))?;
        bc.push(OpCode::Eq.into());
        Ok(())
    }

    pub fn compile_cons(&mut self, bc: &mut Vec<Node>, lat: &Cell) -> Result<(), Error> {
        if lat.is_nil() || !cdr!(cdr!(lat)).is_nil() {
            return Err(InvalidNumArgs("cons".into()));
        }
        self.compile_expression(bc, car!(cdr!(lat)))?;
        bc.push(OpCode::Push.into());
        self.compile_expression(bc, car!(lat))?;
        bc.push(OpCode::Cons.into());
        Ok(())
    }

    pub fn compile_car(&mut self, bc: &mut Vec<Node>, lat: &Cell) -> Result<(), Error> {
        self.compile_expression(bc, car!(lat))?;
        bc.push(OpCode::Car.into());
        Ok(())
    }

    pub fn compile_cdr(&mut self, bc: &mut Vec<Node>, lat: &Cell) -> Result<(), Error> {
        self.compile_expression(bc, car!(lat))?;
        bc.push(OpCode::Cdr.into());
        Ok(())
    }

    pub fn compile_quote(&mut self, bc: &mut Vec<Node>, cell: &Cell) -> Result<(), Error> {
        bc.push(OpCode::Quote.into());
        bc.push(self.heap.put_cell(cell));
        Ok(())
    }

    pub fn compile_operator(
        &mut self,
        bc: &mut Vec<Node>,
        lat: &Cell,
        op: OpCode,
    ) -> Result<(), Error> {
        let mut lat = lat;

        let base_value = match op {
            OpCode::Mul => &Cell::Number(1),
            _ => &Cell::Number(0),
        };

        // Special zero arg form. Evaluate to the base value.
        if lat.is_nil() {
            if op == OpCode::Sub {
                return Err(InvalidNumArgs("-".to_string()));
            }
            self.compile_quote(bc, base_value)?;
            return Ok(());
        }

        let first_arg = car!(lat);
        lat = cdr!(lat);

        // Special one arg form. Add it to 0 so that type
        // checking from the ADD instruction still occurs.
        if lat.is_nil() {
            self.compile_quote(bc, base_value)?;
            bc.push(OpCode::Push.into());
            self.compile_expression(bc, first_arg)?;
            bc.push(op.into());
            return Ok(());
        }

        // Each additional arg is added to ACC
        self.compile_expression(bc, first_arg)?;
        while !lat.is_nil() {
            bc.push(OpCode::Push.into());
            self.compile_expression(bc, car!(lat))?;
            bc.push(op.clone().into());
            lat = cdr!(lat);
        }

        Ok(())
    }
}
