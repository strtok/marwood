use crate::error::Error;
use crate::error::Error::InvalidBytecode;
use crate::vm::lambda::Lambda;
use crate::vm::vcell::VCell;
use crate::vm::Vm;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum OpCode {
    // VM Primitives
    Jmp,
    Jnt,
    Mov,
    MovImmediate,
    Push,
    PushAcc,
    PushImmediate,
    Halt,

    // Procedures
    CallAcc,
    ClosureAcc,
    Enter,
    Ret,
    TCallAcc,
    VarArg,
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

    /// Acc
    ///
    /// The operand is the ACC register. This is used to render instructions
    /// where the operand is encoded in the instruction (e.g. CallAcc).
    Acc,
}

/// Schema
///
/// Schema and the static mapped returned by Schema() are used by the
/// decompiler to determine the formatting for opcodes and operands
/// when decompiling a set of byte code.
///
/// While schema would be useful to the virtual machine runtime, it's
/// not currently referenced by the VM runtime code for performance
/// concerns.
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
            (OpCode::CallAcc, Schema::new("CALL", vec![Operand::Acc])),
            (OpCode::ClosureAcc, Schema::new("CLOSURE", vec![Operand::Acc])),
            (OpCode::Enter, Schema::new("ENTER", vec![])),
            (OpCode::Halt, Schema::new("HALT", vec![])),
            (OpCode::Jmp, Schema::new("JMP", vec![Operand::Immediate])),
            (OpCode::Jnt, Schema::new("JNT", vec![Operand::Immediate])),
            (OpCode::Mov, Schema::new("MOV", vec![Operand::LoadReference, Operand::StoreReference])),
            (OpCode::MovImmediate, Schema::new("MOV", vec![Operand::Immediate, Operand::StoreReference])),
            (OpCode::Push, Schema::new("PUSH", vec![Operand::LoadReference])),
            (OpCode::PushImmediate, Schema::new("PUSH", vec![Operand::Immediate])),
            (OpCode::PushAcc, Schema::new("PUSH", vec![Operand::Acc])),
            (OpCode::Ret, Schema::new("RET", vec![])),
            (OpCode::TCallAcc, Schema::new("TCALL", vec![Operand::Acc])),
            (OpCode::VarArg, Schema::new("VARARG", vec![]))
        ]);
    }
    &SCHEMA
}

#[derive(Debug)]
pub struct DecompiledInstruction {
    op: &'static str,
    operands: Vec<(VCell, Operand)>,
    values: Vec<String>,
}

impl Display for DecompiledInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let op = self.op;
        let operands = self
            .operands
            .iter()
            .map(|it| match it.1 {
                Operand::StoreReference | Operand::LoadReference => match it.0 {
                    VCell::Acc => it.0.to_string(),
                    _ => format!("[{}]", it.0),
                },
                _ => it.0.to_string(),
            })
            .collect::<Vec<_>>();
        let values = &self.values;
        if !operands.is_empty() && !values.is_empty() {
            write!(
                f,
                "{0: <8} {1: <12} {2: <12} //{3: <10}",
                op,
                operands.get(0).unwrap_or(&"".to_string()),
                operands.get(1).unwrap_or(&"".to_string()),
                values.join(" ")
            )
        } else if !operands.is_empty() {
            write!(
                f,
                "{0: <8} {1: <12} {2: <12}",
                op,
                operands.get(0).unwrap_or(&"".to_string()),
                operands.get(1).unwrap_or(&"".to_string()),
            )
        } else {
            write!(f, "{}", op)
        }
    }
}

impl Vm {
    pub fn decompile_text(&self, lambda: &Lambda) -> String {
        let mut text = String::new();
        for instruction in self.decompile(lambda).unwrap() {
            text.push_str(&format!("{}\n", instruction));
        }
        text
    }

    pub fn decompile(&self, lambda: &Lambda) -> Result<Vec<DecompiledInstruction>, Error> {
        let mut cur = lambda.bc.iter().peekable();
        let mut instructions = vec![];
        while cur.peek().is_some() {
            instructions.push(self.decompile_one(&mut cur)?);
        }

        Ok(instructions)
    }

    pub fn decompile_one<'a, T: Iterator<Item = &'a VCell>>(
        &self,
        iter: &mut Peekable<T>,
    ) -> Result<DecompiledInstruction, Error> {
        if let Some(VCell::OpCode(opcode)) = iter.next() {
            let schema = schema().get(opcode).expect("unknown opcode");
            let mut operands = vec![];
            let mut values = vec![];
            for it in &schema.operands {
                let operand = match it {
                    Operand::Acc => &VCell::Acc,
                    _ => iter.next().expect("expected operand"),
                };

                operands.push((operand.clone(), it.clone()));

                if values.is_empty() {
                    match operand {
                        VCell::Ptr(_) => {
                            values.push(self.heap.get_as_cell(operand).to_string());
                        }
                        VCell::GlobalEnvSlot(slot) => {
                            let ptr = self.globenv.get_slot(*slot);
                            values.push(self.heap.get_as_cell(&ptr).to_string());
                        }
                        _ => {}
                    }
                }
            }
            Ok(DecompiledInstruction {
                op: schema.name,
                operands,
                values,
            })
        } else {
            Err(InvalidBytecode)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mov() {
        // MOV $01 %acc
        {
            let mut vm = Vm::new();
            let ptr = vm.heap.put(VCell::Bool(true));
            vm.ip.0 = vm
                .heap
                .put(Lambda::from(vec![
                    OpCode::MovImmediate.into(),
                    ptr.clone(),
                    VCell::Acc,
                    OpCode::Halt.into(),
                ]))
                .as_ptr()
                .unwrap();
            vm.ip.1 = 0;
            assert!(vm.run().is_ok());
            assert_eq!(vm.acc, ptr);
        }
        // MOV [$01] %acc
        {
            let mut vm = Vm::new();
            let ptr = vm.heap.put(VCell::Bool(true));
            vm.ip.0 = vm
                .heap
                .put(Lambda::from(vec![
                    OpCode::Mov.into(),
                    ptr.clone(),
                    VCell::Acc,
                    OpCode::Halt.into(),
                ]))
                .as_ptr()
                .unwrap();
            vm.ip.1 = 0;
            assert!(vm.run().is_ok());
            assert_eq!(vm.acc, VCell::Bool(true));
        }
    }
}
