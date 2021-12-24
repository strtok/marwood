use crate::vm::node::Node;
use crate::vm::Vm;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OpCode {
    Add,
    Car,
    Cdr,
    Cons,
    EnvGet,
    EnvSet,
    Eq,
    Mul,
    Halt,
    Push,
    Quote,
    Sub,
}

impl Vm {
    pub fn decompile_text(&self, program: &[Node]) -> String {
        let mut text = String::new();
        for it in self.decompile(program) {
            if !it.1.is_empty() && !it.2.is_empty() {
                text.push_str(&format!(
                    "{0: <8} {1: <10} //{2: <10}\n",
                    it.0,
                    it.1.join(","),
                    it.2.join(",")
                ));
            } else if !it.1.is_empty() {
                text.push_str(&format!("{0: <8} {1: <10}\n", it.0, it.1.join(",")));
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
            result.push(match node {
                Node::OpCode(ref op) => match op {
                    OpCode::Add => ("ADD".into(), vec![], vec![]),
                    OpCode::Car => ("CAR".into(), vec![], vec![]),
                    OpCode::Cdr => ("CDR".into(), vec![], vec![]),
                    OpCode::Cons => ("CONS".into(), vec![], vec![]),
                    OpCode::EnvGet => {
                        let arg = cur.next().unwrap();
                        ("ENVGET".into(), vec![arg.to_string()], vec![])
                    }
                    OpCode::EnvSet => {
                        let arg = cur.next().unwrap();
                        ("ENVSET".into(), vec![arg.to_string()], vec![])
                    }
                    OpCode::Eq => ("EQ".into(), vec![], vec![]),
                    OpCode::Halt => ("HALT".into(), vec![], vec![]),
                    OpCode::Mul => ("MUL".into(), vec![], vec![]),
                    OpCode::Push => ("PUSH".into(), vec![], vec![]),
                    OpCode::Quote => {
                        let arg = cur.next().unwrap();
                        (
                            "QUOTE".into(),
                            vec![arg.to_string()],
                            vec![self.heap.get_as_cell(arg).to_string()],
                        )
                    }
                    OpCode::Sub => ("SUB".into(), vec![], vec![]),
                },
                _ => ("UNKNOWN".into(), vec![], vec![]),
            });
        }
        result
    }
}
