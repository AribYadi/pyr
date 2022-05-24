use std::ops::Range;

use thiserror::Error;

use crate::parser::syntax::TokenKind;

pub type Span = Range<usize>;
pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Error, Debug, PartialEq)]
pub enum ParseErrorKind {
  #[error("`{0}` is not a start to any expression")]
  ExpectedExpressionStart(TokenKind),
  #[error("expected `{0}` but got `{1}`")]
  UnexpectedToken(TokenKind, TokenKind),
  #[error("unknown token `{0}`")]
  UnknownToken(char),
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
  pub kind: ParseErrorKind,
  pub span: Span,
}

impl ParseError {
  pub fn new(kind: ParseErrorKind, span: Span) -> Self { Self { kind, span } }
}
