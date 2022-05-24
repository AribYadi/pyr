use std::ops::Range;

use thiserror::Error;

use crate::parser::syntax::TokenKind;

pub type Span = Range<usize>;
#[allow(dead_code)]
pub type ParseResult<T> = Result<T, ParseError>;

#[allow(dead_code)]
#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParseErrorKind {
  #[error("unknown infix operator `{0}`")]
  UnknownInfixOperator(TokenKind),
  #[error("`{0}` is not a start to any expression")]
  ExpectedExpressionStart(TokenKind),
  #[error("expected `{0}` but got `{1}`")]
  UnexpectedToken(TokenKind, TokenKind),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
  pub kind: ParseErrorKind,
  pub span: Span,
}

#[allow(dead_code)]
impl ParseError {
  pub fn new(kind: ParseErrorKind, span: Span) -> Self { Self { kind, span } }
}
