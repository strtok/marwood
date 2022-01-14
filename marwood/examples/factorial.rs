use marwood::vm::Vm;
use marwood::{lex, parse};

extern crate marwood;

fn main() {
    let mut vm = Vm::new();
    vm.eval(&parse!(
        "(define factorial (lambda (n) (if (eq? n 0) 1 (* n (factorial (- n 1))))))"
    ))
    .unwrap();
    for it in 0..10 {
        println!(
            "the factorial of {} is {}",
            it,
            vm.eval(&parse!(&format!("(factorial {})", it))).unwrap()
        );
    }
}
