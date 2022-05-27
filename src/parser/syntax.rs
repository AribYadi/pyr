use logos::Logos;

use crate::error::Span;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
  Eof,
  #[error]
  #[regex(r"[ \r\f]+", logos::skip)]
  Error,

  // Whitespace
  #[regex(r"\n")]
  Newline,
  #[regex(r"\t")]
  Indent,

  // Literals
  #[regex(r#""(\\"|[^"])*""#)]
  String,
  #[regex("[0-9]+")]
  Integer,
  #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
  Identifier,
  #[token("true")]
  True,
  #[token("false")]
  False,

  // Operators
  #[token("+")]
  Plus,
  #[token("-")]
  Minus,
  #[token("*")]
  Star,
  #[token("/")]
  Slash,
  #[token("!")]
  Bang,

  // Keywords
  // Currently `print` is a keyword rather than a builtin function.
  #[token("print")]
  Print,
  #[token("if")]
  If,
  #[token("else")]
  Else,
  #[token("while")]
  While,

  // Delimiters
  #[token("(")]
  LeftParen,
  #[token(")")]
  RightParen,

  // Syntax
  #[token(":")]
  Colon,
}

impl std::fmt::Display for TokenKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      TokenKind::Eof => write!(f, "end of file"),
      TokenKind::Error => write!(f, "error"),
      TokenKind::Newline => write!(f, "newline"),
      TokenKind::Indent => write!(f, "indent"),
      TokenKind::String => write!(f, "string"),
      TokenKind::Integer => write!(f, "integer"),
      TokenKind::Identifier => write!(f, "identifier"),
      TokenKind::True | TokenKind::False => write!(f, "boolean"),
      TokenKind::Plus => write!(f, "+"),
      TokenKind::Minus => write!(f, "-"),
      TokenKind::Star => write!(f, "*"),
      TokenKind::Slash => write!(f, "/"),
      TokenKind::Bang => write!(f, "!"),
      TokenKind::Print => write!(f, "print"),
      TokenKind::If => write!(f, "if"),
      TokenKind::Else => write!(f, "else"),
      TokenKind::While => write!(f, "while"),
      TokenKind::LeftParen => write!(f, "("),
      TokenKind::RightParen => write!(f, ")"),
      TokenKind::Colon => write!(f, ":"),
    }
  }
}

pub trait Operator {
  fn prefix_bp(&self) -> ((), u8);
  fn infix_bp(&self) -> (u8, u8);
}

impl Operator for TokenKind {
  fn prefix_bp(&self) -> ((), u8) {
    match self {
      TokenKind::Minus | TokenKind::Bang => ((), 51),
      _ => unreachable!("{self} is not a prefix operator"),
    }
  }

  fn infix_bp(&self) -> (u8, u8) {
    match self {
      TokenKind::Plus | TokenKind::Minus => (9, 10),
      TokenKind::Star | TokenKind::Slash => (11, 12),
      _ => unreachable!("{self} is not an infix operator"),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
  String(String),
  Integer(i64),
  Identifier(String),

  PrefixOp { op: TokenKind, right: Box<Expr> },
  InfixOp { op: TokenKind, left: Box<Expr>, right: Box<Expr> },
}

#[derive(Debug, Clone)]
pub struct Expr {
  pub kind: ExprKind,
  pub span: Span,
}

impl std::fmt::Display for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.kind {
      ExprKind::String(s) => write!(f, "{s}"),
      ExprKind::Integer(n) => write!(f, "{n}"),
      ExprKind::Identifier(s) => write!(f, "{s}"),
      ExprKind::PrefixOp { op, right } => write!(f, "{op}{right}"),
      ExprKind::InfixOp { op, left, right } => write!(f, "{left} {op} {right}"),
    }
  }
}

impl PartialEq for Expr {
  fn eq(&self, other: &Self) -> bool { self.kind == other.kind }
}

impl Expr {
  pub fn new(kind: ExprKind, span: Span) -> Self { Self { kind, span } }

  pub fn new_without_span(kind: ExprKind) -> Self { Self { kind, span: 0..0 } }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
  Expression { expr: Expr },
  If { condition: Expr, body: Vec<Stmt>, else_stmt: Vec<Stmt> },
  While { condition: Expr, body: Vec<Stmt> },
  Print { expr: Expr },
}

#[derive(Debug, Clone)]
pub struct Stmt {
  pub kind: StmtKind,
  pub span: Span,
}

impl PartialEq for Stmt {
  fn eq(&self, other: &Self) -> bool { self.kind == other.kind }
}

impl Stmt {
  pub fn new(kind: StmtKind, span: Span) -> Self { Self { kind, span } }

  pub fn new_without_span(kind: StmtKind) -> Self { Self { kind, span: 0..0 } }
}