use crate::cell::Cell;
use crate::vm::node::{Node, Value};
use crate::vm::opcode::OpCode;
use crate::vm::Error::InvalidNumArgs;
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
                Cell::Symbol(s) if s.eq("quote") => self.compile_quote(bc, car!(cdr))?,
                Cell::Symbol(s) if s.eq("car") => self.compile_car(bc, cdr)?,
                Cell::Symbol(s) if s.eq("cdr") => self.compile_cdr(bc, cdr)?,
                Cell::Symbol(s) if s.eq("cons") => self.compile_cons(bc, cdr)?,
                Cell::Symbol(s) if s.eq("+") => self.compile_plus(bc, cdr)?,
                _ => return Err(Error::UnknownProcedure(car.to_string())),
            },
            Cell::Number(_) => self.compile_quote(bc, cell)?,
            _ => return Err(Error::UnknownProcedure(cell.to_string())),
        };
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

    pub fn compile_plus(&mut self, bc: &mut Vec<Node>, lat: &Cell) -> Result<(), Error> {
        let mut lat = lat;

        // Special zero arg form. Evaluate to 0.
        if lat.is_nil() {
            self.compile_quote(bc, &Cell::Number(0))?;
            return Ok(());
        }

        self.compile_expression(bc, car!(lat))?;
        lat = cdr!(lat);

        // Special one arg form. The result is already in ACC
        if lat.is_nil() {
            return Ok(());
        }

        // Each additional arg is added to ACC
        while !lat.is_nil() {
            bc.push(OpCode::Push.into());
            self.compile_expression(bc, car!(lat))?;
            bc.push(OpCode::Add.into());
            lat = cdr!(lat);
        }

        Ok(())
    }

    pub fn decompile_text(&self, program: &[Node]) -> String {
        let mut text = String::new();
        for it in self.decompile(program) {
            if it.1.is_empty() {
                text.push_str(&format!(
                    "{} {} #{}\n",
                    it.0,
                    it.1.join(","),
                    it.2.join(",")
                ));
            } else {
                text.push_str(&format!("{}\n", it.0));
            }
        }
        text
    }

    pub fn decompile(&self, program: &[Node]) -> Vec<(String, Vec<String>, Vec<String>)> {
        let mut cur = program.iter();
        let mut result: Vec<(String, Vec<String>, Vec<String>)> = vec![];
        while let Some(node) = cur.next() {
            result.push(match node.val {
                Value::OpCode(ref op) => match op {
                    OpCode::Add => ("ADD".into(), vec![], vec![]),
                    OpCode::Car => ("CAR".into(), vec![], vec![]),
                    OpCode::Cdr => ("CDR".into(), vec![], vec![]),
                    OpCode::Cons => ("CONS".into(), vec![], vec![]),
                    OpCode::Halt => ("HALT".into(), vec![], vec![]),
                    OpCode::Push => ("PUSH".into(), vec![], vec![]),
                    OpCode::Quote => {
                        let arg = cur.next().unwrap();
                        (
                            "QUOTE".into(),
                            vec![arg.to_string()],
                            vec![self.heap.get_as_cell(arg).to_string()],
                        )
                    }
                },
                _ => ("UNKNOWN".into(), vec![], vec![]),
            });
        }
        result
    }
}
