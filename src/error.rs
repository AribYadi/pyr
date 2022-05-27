use std::ops::Range;

use thiserror::Error;

use crate::parser::syntax::{
  Expr,
  TokenKind,
};

pub type Span = Range<usize>;
pub type ParseResult<T> = Result<T, ParseError>;
pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParseErrorKind {
  #[error("unknown infix operator `{0}`")]
  UnknownInfixOperator(TokenKind),
  #[error("`{0}` is not a start to any expression")]
  ExpectedExpressionStart(TokenKind),
  #[error("expected `{0}` but got `{1}`")]
  UnexpectedToken(TokenKind, TokenKind),
  #[error("unclosed delimiter `{0}`")]
  UnclosedDelimiter(TokenKind),
  #[error("unexpected indent block")]
  UnexpectedIndentBlock,
  #[error("unmatched else statement")]
  UnmatchedElseStatement,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
  pub kind: ParseErrorKind,
  pub span: Span,
}

impl ParseError {
  pub fn new(kind: ParseErrorKind, span: Span) -> Self { Self { kind, span } }
}

#[derive(Error, Debug, PartialEq)]
pub enum RuntimeErrorKind {
  #[error("cannot apply prefix operator `{1}` using `{0:?}`")]
  CannotApplyPrefix(Expr, TokenKind),
  #[error("cannot apply infix operator `{1}` to {0:?} and {2:?}")]
  CannotApplyInfix(Expr, TokenKind, Expr),
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
  pub kind: RuntimeErrorKind,
  pub span: Span,
}

impl RuntimeError {
  pub fn new(kind: RuntimeErrorKind, span: Span) -> Self { Self { kind, span } }
}
