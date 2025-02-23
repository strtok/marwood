use std::{collections::HashSet, ops::Range};

use super::{Vm, lambda::Lambda, opcode::OpCode, vcell::VCell};

#[derive(Debug)]
struct Rewrite {
    range: Range<usize>,
    instructions: Vec<VCell>,
}

#[allow(clippy::single_match)]
impl Vm {
    pub fn optimize(&self, mut lambda: Lambda) -> Lambda {
        // Make a copy of the byte code, keeping track of the original
        // offsets of each instruction. This will be useful for re-calculating
        // JMP offsets after rewriting instructions.
        let mut bc = lambda.bc.iter().cloned().enumerate().collect::<Vec<_>>();

        // Look for optimizable patterns, building up a plan of instructions
        // to be rewritten and what they're rewritten with.
        let plan = bc.windows(5).fold(vec![], |mut plan, window| {
            match *window {
                // MOV <intermediate> %acc
                // PUSH %acc
                //  -> PUSH <intermediate
                [
                    (start, VCell::OpCode(OpCode::MovImmediate)),
                    (_, ref operand),
                    (_, VCell::Acc),
                    (end, VCell::OpCode(OpCode::PushAcc)),
                    ..,
                ] => {
                    plan.push(Rewrite {
                        range: Range {
                            start,
                            end: end + 1,
                        },
                        instructions: vec![VCell::OpCode(OpCode::PushImmediate), operand.clone()],
                    });
                }
                [
                    (start, VCell::OpCode(OpCode::Mov)),
                    (_, ref operand),
                    (_, VCell::Acc),
                    (end, VCell::OpCode(OpCode::PushAcc)),
                    ..,
                ] => {
                    plan.push(Rewrite {
                        range: Range {
                            start,
                            end: end + 1,
                        },
                        instructions: vec![VCell::OpCode(OpCode::Push), operand.clone()],
                    });
                }

                _ => {}
            }
            plan
        });

        // Build a list of instructions to be filtered
        let removed_idxs = plan
            .iter()
            .flat_map(|rewrite| rewrite.range.clone())
            .collect::<HashSet<usize>>();

        // Remove them
        bc.retain(|it| !removed_idxs.contains(&it.0));

        // Add the new instructions
        for rewrite in &plan {
            let mut offset = rewrite.range.start;
            for vcell in &rewrite.instructions {
                bc.push((offset, vcell.clone()));
                offset += 1;
            }
        }

        // Sort by offset
        bc.sort_by(|a, b| a.0.cmp(&b.0));

        // Recalculate jump offsets
        let mut iter = bc.iter().enumerate().peekable();
        let mut jmp_rewrites = vec![];
        while let Some((new_idx, &(_old_idx, ref vcell))) = iter.next() {
            if let VCell::OpCode(OpCode::Jmp | OpCode::Jnt) = vcell {
                // If this is a JMP instruction, take the offset from its operand
                let jmp_offset = if let Some((_, (_, VCell::Ptr(ptr)))) = iter.peek() {
                    *ptr
                } else {
                    panic!("expected JMP instruction to be followed by an offset");
                };

                // Find the instruction that has old_idx = jmp_offset, and remember its
                // old_idx -> new_idx mapping.
                if let Some((new_target_idx, (_, _))) =
                    bc.iter().enumerate().find(|it| it.1.0 == jmp_offset)
                {
                    jmp_rewrites.push((new_idx + 1, new_target_idx));
                } else if let Some((new_target_idx, (_, _))) =
                    bc.iter().enumerate().find(|it| it.1.0 > jmp_offset)
                {
                    jmp_rewrites.push((new_idx + 1, new_target_idx - 2));
                } else {
                    panic!("expected to find original JMP target");
                }
            }
        }

        // Apply the rewrites to bc
        for (ptr_idx, new_jmp_offset) in jmp_rewrites {
            if let (idx, VCell::Ptr(_)) = bc[ptr_idx] {
                bc[ptr_idx] = (idx, VCell::Ptr(new_jmp_offset));
            } else {
                panic!("expected JMP instruction to be followed by an offset");
            }
        }

        // Reconstruct lambda
        lambda.bc = bc.into_iter().map(|it| it.1).collect();
        lambda
    }
}
