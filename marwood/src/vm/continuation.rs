use crate::vm::stack::Stack;
use crate::vm::vcell::VCell;
use crate::vm::Vm;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Continuation {
    stack: Stack,
    ep: usize,
    ip: (usize, usize),
    bp: usize,
}

impl Continuation {
    pub fn stack(&self) -> &Stack {
        &self.stack
    }

    pub fn ip(&self) -> &(usize, usize) {
        &self.ip
    }

    pub fn ep(&self) -> usize {
        self.ep
    }

    pub fn bp(&self) -> usize {
        self.bp
    }
}

impl Vm {
    pub fn to_continuation(&self) -> Continuation {
        Continuation {
            stack: self.stack.to_continuation(),
            ep: self.ep,
            ip: self.ip,
            bp: self.bp,
        }
    }

    pub fn restore_continuation(&mut self, cont: &Continuation) {
        self.stack.restore_continuation(cont.stack());
        self.ep = cont.ep();
        self.ip = *cont.ip();
        self.bp = cont.bp();
        self.acc = VCell::Undefined;
    }
}
