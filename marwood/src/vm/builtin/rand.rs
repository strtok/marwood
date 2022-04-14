use crate::error::Error;
use crate::error::Error::InvalidSyntax;
use crate::vm::builtin::{pop_argc, pop_integer};
use crate::vm::vcell::VCell;
use crate::vm::Vm;
use rand::Rng;

pub fn load_builtins(vm: &mut Vm) {
    vm.load_builtin("random-integer", random_integer);
    vm.load_builtin("random-real", random_real);
    vm.load_builtin("random-signed", random_signed);
}
pub fn random_integer(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 1, Some(1), "random-integer")?;
    let n = pop_integer(vm)?;
    let n = n
        .to_i64()
        .ok_or_else(|| InvalidSyntax(format!("random-integer: {} is out of range", n)))?;
    if n <= 0 {
        return Err(InvalidSyntax(format!(
            "random-integer: {} is out of range",
            n
        )));
    }
    let mut rng = ::rand::thread_rng();
    let result: i64 = rng.gen_range(0..n);
    Ok(VCell::Number(result.into()))
}

pub fn random_real(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 0, Some(0), "random-real")?;
    let mut rng = ::rand::thread_rng();
    let result: f64 = rng.gen();
    Ok(VCell::Number(result.into()))
}

pub fn random_signed(vm: &mut Vm) -> Result<VCell, Error> {
    pop_argc(vm, 0, Some(0), "random-signed")?;
    let mut rng = ::rand::thread_rng();
    let result: i64 = rng.gen();
    Ok(VCell::Number(result.into()))
}
