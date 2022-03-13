use criterion::{black_box, criterion_group, criterion_main, Criterion};
use marwood::cell::Cell;
use marwood::vm::Vm;
use marwood::{cell, lex, parse};

fn sum_of_triangles(n: u64) -> Cell {
    let mut vm = Vm::new();
    vm.eval(&parse!(
        r#"
    (define (sum-of-triangles n)
         (define (triangle n) (quotient (* n (+ n 1)) 2))
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

fn factorial_cps(n: u64) -> Cell {
    let mut vm = Vm::new();
    vm.eval(&parse!(
        r#"
    (define (factorial n)
        (define (factorial n k)
             (cond
              [(zero? n) (k 1)]
         [else
         (factorial (- n 1)
             (λ (v) (k (* v n))))]))
        (factorial n (λ (v) v)))
    "#
    ))
    .unwrap();
    let result = vm.eval(&parse!(&format!("(factorial {})", n))).unwrap();
    assert_eq!(result, cell![3628800]);
    result
}

fn heap_alloc(n: u64) -> Cell {
    let mut vm = Vm::new();
    vm.eval(&parse!(
        r#"
        (define (make-big-vec len)
          (define v (make-vector len))
          (let ~ ((i 0))
            (cond ((< i len)
            (vector-set! v i (vector 1 2 3 4))
            (~ (+ i 1))))))
    "#
    ))
    .unwrap();
    let result = vm.eval(&parse!(&format!("(make-big-vec {})", n))).unwrap();
    assert_eq!(result, Cell::Void);
    result
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("sum-of-triangles 1000", |b| {
        b.iter(|| sum_of_triangles(black_box(1000)))
    });
    c.bench_function("factorial 10", |b| b.iter(|| factorial_cps(black_box(10))));
    c.bench_function("heap-alloc 4000", |b| {
        b.iter(|| heap_alloc(black_box(4000)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
