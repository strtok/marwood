#[allow(unused_macros)]
macro_rules! evals {
    ($($lhs:expr => $rhs:expr),+) => {{
        let mut vm = Vm::new();
         $(
            assert_eq!(vm.eval(&parse!($lhs)), Ok(match $rhs {
                "#<void>" => Cell::Void,
                "inf" => Cell::Number(marwood::number::Number::Float(f64::INFINITY)),
                "NaN" => Cell::Number(marwood::number::Number::Float(f64::NAN)),
                _ => parse!($rhs)
            }));
         )+
    }};
}

#[allow(unused_macros)]
macro_rules! prints {
    ($($lhs:expr => $rhs:expr),+) => {{
        let mut vm = Vm::new();
         $(
            assert_eq!(vm.eval(&parse!($lhs)).unwrap().to_string(), $rhs);
         )+
    }};
}

#[allow(unused_macros)]
macro_rules! fails {
    ($($lhs:expr => $rhs:expr),+) => {{
        let mut vm = Vm::new();
         $(
            assert_eq!(vm.eval(
                &parse!($lhs)
            ), Err($rhs));
         )+
    }};
}
