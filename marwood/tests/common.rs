#[allow(unused_macros)]
macro_rules! evals {
    ($($lhs:expr => $rhs:expr),+) => {{
        let mut vm = Vm::new();
         $(
            assert_eq!(vm.eval(&parse!($lhs)), Ok(match $rhs {
                "#<void>" => Cell::Void,
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
