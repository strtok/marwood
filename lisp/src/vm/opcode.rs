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
pub enum Operand {
    /// Immediate
    ///
    /// The operand value is used, unless:
    /// * The operand is a register, in which case the value in the
    ///   register is used
    Immediate,

    /// StoreReference
    ///
    /// The operand is used as a reference to a location to store a
    /// value. It must be a register or reference type.
    ///
    /// * If the operand is a register, the value is stored in the
    /// register itself. No dereference occurs.
    /// * If the operand is a reference type, the value is stored
    /// in the location the reference points to.
    StoreReference,

    /// Dereference
    ///
    /// The operand is used as a source of a value. It must be a register
    /// or reference type.
    ///
    /// * If the operand is a register, then the value is read from the
    /// register.
    /// * If the operand is a reference type, the value is read from
    /// the location the reference points to.
    LoadReference,
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

#[rustfmt::skip]
fn schema() -> &'static HashMap<OpCode, Schema> {
    lazy_static! {
        static ref SCHEMA: HashMap<OpCode, Schema> = HashMap::from([
            (OpCode::Add, Schema::new("ADD", vec![])),
            (OpCode::Car, Schema::new("CAR", vec![])),
            (OpCode::Cdr, Schema::new("CDR", vec![])),
            (OpCode::Cons, Schema::new("CONS", vec![])),
            (OpCode::EnvGet, Schema::new("ENVGET", vec![Operand::LoadReference])),
            (OpCode::EnvSet, Schema::new("ENVSET", vec![Operand::StoreReference])),
            (OpCode::Eq, Schema::new("EQ", vec![])),
            (OpCode::Mov, Schema::new("MOV", vec![Operand::LoadReference, Operand::StoreReference])),
            (OpCode::MovImmediate, Schema::new("MOV", vec![Operand::Immediate, Operand::StoreReference])),
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
        for instruction in self.decompile(program) {
            let operands = instruction
                .1
                .iter()
                .map(|it| match it.1 {
                    Operand::Immediate => {
                        format!("{}", it.0.to_string())
                    }
                    Operand::StoreReference | Operand::LoadReference => match it.0 {
                        VCell::Acc => format!("{}", it.0.to_string()),
                        _ => format!("[{}]", it.0.to_string()),
                    },
                })
                .collect::<Vec<_>>();
            if !instruction.1.is_empty() && !instruction.2.is_empty() {
                text.push_str(&format!(
                    "{0: <8} {1: <10} //{2: <10}\n",
                    instruction.0,
                    operands.join(" "),
                    instruction.2.join(" ")
                ));
            } else if !instruction.1.is_empty() {
                text.push_str(&format!(
                    "{0: <8} {1: <10}\n",
                    instruction.0,
                    operands.join(" ")
                ));
            } else {
                text.push_str(&format!("{}\n", instruction.0));
            }
        }
        text
    }

    pub fn decompile(
        &self,
        program: &[VCell],
    ) -> Vec<(String, Vec<(VCell, Operand)>, Vec<String>)> {
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
                operands.push((operand.clone(), it.clone()));

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
