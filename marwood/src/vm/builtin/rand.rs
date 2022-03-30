use crate::vm::builtin::{pop_argc, pop_integer};
use crate::vm::vcell::VCell;
use crate::vm::Error::InvalidSyntax;
use crate::vm::{Error, Vm};
use rand::Rng;

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("random-integer", random_integer);
}
pub fn random_integer(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "random-integer")?;
    let n = pop_integer(vm)?;
    let n = n
        .to_i64()
        .ok_or_else(|| InvalidSyntax(format!("random-integer: {} is too large", n)))?;
    let mut rng = ::rand::thread_rng();
    let result: i64 = rng.gen_range(0..n);
    Ok(VCell::Number(result.into()))
}
