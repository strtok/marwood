use crate::cell::Cell;
use crate::{lex, parse};

#[derive(thiserror::Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error("{}", .0.iter().map(|it| it.to_string()).collect::<Vec<_>>().join(" "))]
    ErrorSignal(Vec<Cell>),

    #[error("expected {0} but encountered {1}")]
    ExpectedType(&'static str, &'static str),

    #[error("expected pair, but found {0:#}")]
    ExpectedPairButFound(Cell),

    #[error("invalid argument for {0}: expected {1}, but got {2}")]
    InvalidArgs(String, String, String),

    #[error("invalid number of arguments for {0}")]
    InvalidNumArgs(String),

    #[error("invalid bytecode")]
    InvalidBytecode,

    #[error("call of non-procedure: {0:#}")]
    InvalidProcedure(Cell),

    #[error("invalid stack index: {0}")]
    InvalidStackIndex(usize),

    #[error("invalid use of primitive {0}")]
    InvalidUsePrimitive(String),

    #[error("invalid syntax: {0}")]
    InvalidSyntax(String),

    #[error("lambda require at least one expression")]
    LambdaMissingExpression,

    #[error("misplaced macro keyword {0}")]
    MisplacedMacroKeyword(String),

    #[error("{0} is not bound")]
    VariableNotBound(String),

    #[error("invalid syntax: () must be quoted")]
    UnquotedNil,

    #[error("vector index {0} out of range of [0,{1})")]
    InvalidVectorIndex(usize, usize),

    #[error("string index {0} out of range of [0,{1})")]
    InvalidStringIndex(usize, usize),

    #[error(transparent)]
    ParseError(#[from] parse::Error),

    #[error(transparent)]
    LexError(#[from] lex::Error),
}
