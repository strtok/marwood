use crate::cell::Cell;
use crate::vm::heap::Heap;
use crate::vm::opcode::OpCode;
use crate::vm::stack::Stack;
use crate::vm::vcell::VCell;

/// Stack Frame
///
/// Stack Frame represents one stack frame in a backtrace. It includes a
/// description of the applied procedure, and the arguments pushed to the
/// procedure in order.
#[derive(Debug)]
pub struct StackFrame {
    pub name: Option<String>,
    pub desc: Option<Cell>,
}

/// Stack Trace
///
/// Stack trace represents a decoded stack trace in order of procedure
/// application, frames[0] containing the entry procedure.
#[derive(Debug)]
pub struct StackTrace {
    pub frames: Vec<StackFrame>,
}

impl StackTrace {
    /// New
    ///
    /// Given a stack, heap and %ip register, construct a stack trace.
    pub fn new(stack: &Stack, heap: &Heap, ip: (usize, usize), acc: VCell) -> StackTrace {
        let mut frames = vec![];

        // Get currently running lambda
        let mut ip_idx = ip.1;
        let ip = heap.get_at_index(ip.0).as_lambda().unwrap();

        // Reverse %ip to last instruction
        ip_idx -= 1;
        while ip_idx > 0 && !matches!(ip.get(ip_idx).unwrap(), VCell::OpCode(_)) {
            ip_idx -= 1;
        }
        let op_code = ip.get(ip_idx).unwrap().as_opcode().unwrap();

        // If the just executed instruction was procedure application for a builtin, then
        // the builtin is the top frame.
        match op_code {
            OpCode::TCallAcc | OpCode::CallAcc => {
                if let VCell::BuiltInProc(proc) = heap.get(&acc) {
                    frames.push(StackFrame {
                        name: Some(proc.desc().to_owned()),
                        desc: None,
                    })
                }
            }
            _ => {}
        }

        // Push the currently executing lambda onto the frames
        frames.push(StackFrame {
            name: None,
            desc: ip.desc_args.clone(),
        });

        // Iterate the stack backwards
        for sp in (0..stack.get_sp()).rev() {
            if let Ok(VCell::InstructionPointer(ip, _)) = stack.get(sp) {
                let ip = heap.get_at_index(*ip).as_lambda().unwrap();
                frames.push(StackFrame {
                    name: None,
                    desc: ip.desc_args.clone(),
                });
            }
        }

        StackTrace { frames }
    }
}
