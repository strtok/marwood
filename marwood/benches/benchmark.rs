use criterion::{black_box, criterion_group, criterion_main, Criterion};
use marwood::cell::Cell;
use marwood::vm::Vm;
use marwood::{cell, lex, parse};

fn sum_of_triangles(n: u64) -> Cell {
    let mut vm = Vm::new();
    vm.eval(&parse!(
        r#"
    (define (sum-of-triangles n)
         (define (triangle n) (/ (* n (+ n 1)) 2))
         (define (sum-of-triangles n acc)
            (cond
               ((zero? n) acc)
               (else (sum-of-triangles (- n 1) (+ acc (triangle n))))))
     (sum-of-triangles n 0))
    "#
    ))
    .unwrap();
    let result = vm
        .eval(&parse!(&format!("(sum-of-triangles {})", n)))
        .unwrap();
    assert_eq!(result, cell![167167000]);
    result
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("sum-of-triangles 1000", |b| {
        b.iter(|| sum_of_triangles(black_box(1000)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
