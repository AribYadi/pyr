use std::ops::Range;

use thiserror::Error;

use crate::parser::syntax::TokenKind;
use crate::runtime::Literal;

pub type Span = Range<usize>;
pub type ParseResult<T> = Result<T, ParseError>;
pub type InterpretResult<T> = Result<T, InterpretError>;

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

#[derive(Error, Debug, PartialEq, Eq)]
pub enum InterpretErrorKind {
  #[error("cannot apply prefix operator `{1}` using `{0:?}`")]
  CannotApplyPrefix(Literal, TokenKind),
  #[error("cannot apply infix operator `{1}` to {0:?} and {2:?}")]
  CannotApplyInfix(Literal, TokenKind, Literal),
}

#[derive(Debug, PartialEq, Eq)]
pub struct InterpretError {
  pub kind: InterpretErrorKind,
  pub span: Option<Span>,
}

impl InterpretError {
  pub fn new(kind: InterpretErrorKind) -> Self { Self { kind, span: None } }

  pub fn new_with_span(kind: InterpretErrorKind, span: Span) -> Self {
    Self { kind, span: Some(span) }
  }

  pub fn with_location(self, span: Span) -> Self { Self { span: Some(span), ..self } }
}
