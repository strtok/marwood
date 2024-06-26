use marwood::vm::Vm;

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

    vm.eval_text(code).unwrap();

    for it in 0..10 {
        let (cell, _) = vm.eval_text(&format!("(factorial {})", it)).unwrap();
        println!("the factorial of {} is {}", it, cell);
    }
}
