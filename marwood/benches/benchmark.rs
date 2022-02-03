use criterion::{black_box, criterion_group, criterion_main, Criterion};
use marwood::cell::Cell;
use marwood::vm::Vm;
use marwood::{lex, parse};

fn factorial(n: u64) -> Cell {
    let mut vm = Vm::new();
    vm.eval(&parse!(
        "(define (factorial n) (if (eq? n 0) 1 (* n (factorial (- n 1)))))"
    ))
    .unwrap();
    vm.eval(&parse!(&format!("(factorial {})", n))).unwrap()
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("factorial 20", |b| b.iter(|| factorial(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
