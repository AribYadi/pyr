use std::ops::Range;

use thiserror::Error;

use crate::parser::syntax::{
  Expr,
  TokenKind,
};

pub type Span = Range<usize>;
pub type ParseResult<T> = Result<T, ParseError>;
pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Error, Debug, PartialEq)]
pub enum ParseErrorKind {
  #[error("unknown infix operator `{0}`")]
  UnknownInfixOperator(TokenKind),
  #[error("expected an expression")]
  ExpectedExpression,
  #[error("expected `{0}` but got `{1}`")]
  UnexpectedToken(TokenKind, TokenKind),
  #[error("unclosed delimiter `{0}`")]
  UnclosedDelimiter(TokenKind),
  #[error("unexpected indent block")]
  UnexpectedIndentBlock,
  #[error("unmatched else statement")]
  UnmatchedElseStatement,
  #[error("`{0}` cannot be assigned to")]
  InvalidAssignment(Expr),
  #[error(transparent)]
  Unescape(#[from] snailquote::UnescapeError),
  #[error("unknown token `{0}`")]
  UnknownToken(String),
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
  pub kind: ParseErrorKind,
  pub span: Span,
}

impl ParseError {
  pub fn new(kind: impl Into<ParseErrorKind>, span: Span) -> Self {
    Self { kind: kind.into(), span }
  }
}

#[derive(Error, Debug, PartialEq)]
pub enum RuntimeErrorKind {
  #[error("cannot apply prefix operator `{1}` using `{0}`")]
  CannotApplyPrefix(Expr, TokenKind),
  #[error("cannot apply infix operator `{1}` to {0} and {2}")]
  CannotApplyInfix(Expr, TokenKind, Expr),
  #[error("undefined variable `{0}` in the current scope")]
  UndefinedVariable(String),
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
  pub kind: RuntimeErrorKind,
  pub span: Span,
}

impl RuntimeError {
  pub fn new(kind: RuntimeErrorKind, span: Span) -> Self { Self { kind, span } }
}
