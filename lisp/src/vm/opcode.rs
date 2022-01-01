use crate::vm::vcell::VCell;
use crate::vm::Vm;
use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum OpCode {
    Add,
    Car,
    Cdr,
    Cons,
    EnvGet,
    EnvSet,
    Eq,
    Mov,
    MovImmediate,
    Mul,
    Halt,
    Push,
    Sub,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Operand {
    Immediate,
    Reference,
    Dereference,
}

struct Schema {
    name: &'static str,
    operands: Vec<Operand>,
}

impl Schema {
    fn new(name: &'static str, operands: Vec<Operand>) -> Schema {
        Schema { name, operands }
    }
}

fn schema() -> &'static HashMap<OpCode, Schema> {
    lazy_static! {
        static ref SCHEMA: HashMap<OpCode, Schema> = HashMap::from([
            (OpCode::Add, Schema::new("ADD", vec![])),
            (OpCode::Car, Schema::new("CAR", vec![])),
            (OpCode::Cdr, Schema::new("CDR", vec![])),
            (OpCode::Cons, Schema::new("CONS", vec![])),
            (
                OpCode::EnvGet,
                Schema::new("ENVGET", vec![Operand::Reference])
            ),
            (
                OpCode::EnvSet,
                Schema::new("ENVSET", vec![Operand::Reference])
            ),
            (OpCode::Eq, Schema::new("EQ", vec![])),
            (
                OpCode::Mov,
                Schema::new("MOV", vec![Operand::Dereference, Operand::Reference])
            ),
            (
                OpCode::MovImmediate,
                Schema::new("MOV", vec![Operand::Immediate, Operand::Reference])
            ),
            (OpCode::Mul, Schema::new("MUL", vec![])),
            (OpCode::Halt, Schema::new("HALT", vec![])),
            (OpCode::Push, Schema::new("PUSH", vec![])),
            (OpCode::Sub, Schema::new("SUB", vec![]))
        ]);
    }
    &SCHEMA
}

impl Vm {
    pub fn decompile_text(&self, program: &[VCell]) -> String {
        let mut text = String::new();
        for it in self.decompile(program) {
            if !it.1.is_empty() && !it.2.is_empty() {
                text.push_str(&format!(
                    "{0: <8} {1: <10} //{2: <10}\n",
                    it.0,
                    it.1.join(" "),
                    it.2.join(" ")
                ));
            } else if !it.1.is_empty() {
                text.push_str(&format!("{0: <8} {1: <10}\n", it.0, it.1.join(" ")));
            } else {
                text.push_str(&format!("{}\n", it.0));
            }
        }
        text
    }

    pub fn decompile(&self, program: &[VCell]) -> Vec<(String, Vec<String>, Vec<String>)> {
        let mut cur = program.iter();
        let mut result = vec![];
        while let Some(VCell::OpCode(opcode)) = cur.next() {
            let schema = schema().get(opcode).expect("unknown opcode");
            let mut operands = vec![];
            let mut values = vec![];
            for it in &schema.operands {
                let operand = cur.next().expect("expected operand");
                if operand.is_ptr() && values.is_empty() {
                    values.push(self.heap.get_as_cell(operand).to_string());
                }
                match it {
                    Operand::Immediate | Operand::Reference => {
                        operands.push(operand.to_string());
                    }
                    Operand::Dereference => {
                        if operand.is_reference() {
                            operands.push(format!("[{}]", operand));
                        } else {
                            operands.push(format!("{}", operand));
                        }
                    }
                }

                if values.is_empty() {
                    match operand {
                        VCell::Ptr(_) => {
                            values.push(self.heap.get_as_cell(operand).to_string());
                        }
                        VCell::EnvSlot(slot) => {
                            let ptr = self.globenv.get_slot(*slot);
                            values.push(self.heap.get_as_cell(&ptr).to_string());
                        }
                        _ => {}
                    }
                }
            }
            result.push((schema.name.to_string(), operands, values));
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mov() {
        {
            let mut vm = Vm::new();
            let ptr = vm.heap.put(VCell::Bool(true));
            vm.bc = vec![
                OpCode::MovImmediate.into(),
                ptr.clone(),
                VCell::Acc,
                OpCode::Halt.into(),
            ];
            assert!(vm.run().is_ok());
            assert_eq!(vm.acc, ptr);
        }
        {
            let mut vm = Vm::new();
            let ptr = vm.heap.put(VCell::Bool(true));
            vm.bc = vec![
                OpCode::Mov.into(),
                ptr.clone(),
                VCell::Acc,
                OpCode::Halt.into(),
            ];
            assert!(vm.run().is_ok());
            assert_eq!(vm.acc, VCell::Bool(true));
        }
    }
}
