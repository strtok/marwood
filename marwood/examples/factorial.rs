use marwood::cell::Cell;
use marwood::vm::Vm;
use marwood::{lex, parse};

extern crate marwood;

fn main() {
    let mut vm = Vm::new();
    let code = r#"
        (define (factorial n)
            (let factorial ([n n] [acc 1])
               (if (zero? n)
                   acc
                   (factorial (- n 1) (* acc n)))))
    "#;

    let (cell, remaining_text): (Cell, Option<&str>) = parse::parse_text(&code).unwrap();
    assert_eq!(remaining_text, None);
    vm.eval(&cell).unwrap();

    for it in 0..10 {
        println!(
            "the factorial of {} is {}",
            it,
            vm.eval(&parse!(&format!("(factorial {})", it))).unwrap()
        );
    }
}
