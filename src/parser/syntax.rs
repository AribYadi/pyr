use std::sync::Mutex;

use logos::internal::LexerInternal;
use logos::{
  Lexer,
  Logos,
};

use crate::error::Span;

lazy_static::lazy_static! {
  pub static ref INDENT_SIZE: Mutex<usize> = Mutex::new(0);
}

enum LexIndent {
  Emit,
  Skip,
}

impl<'s, T: Logos<'s>> logos::internal::CallbackResult<'s, (), T> for LexIndent {
  #[inline]
  fn construct<Constructor>(self, c: Constructor, lex: &mut Lexer<'s, T>)
  where
    Constructor: Fn(()) -> T, {
    match self {
      LexIndent::Emit => lex.set(c(())),
      LexIndent::Skip => {
        lex.trivia();
        T::lex(lex);
      },
    }
  }
}

fn parse_indent(lex: &mut Lexer<TokenKind>) -> LexIndent {
  let mut spaces = " ".to_string();
  for ch in lex.remainder().chars() {
    if ch == ' ' {
      spaces.push(ch);
    } else {
      break;
    }
  }

  if spaces.len() <= 1 {
    return LexIndent::Skip;
  }

  if *INDENT_SIZE.lock().unwrap() == 0 {
    *INDENT_SIZE.lock().unwrap() = spaces.len();
  }

  if spaces.len() < *INDENT_SIZE.lock().unwrap() {
    return LexIndent::Skip;
  }

  lex.bump(*INDENT_SIZE.lock().unwrap() - 1);
  LexIndent::Emit
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
  Eof,
  #[error]
  #[regex(r"[\r\f]+", logos::skip)]
  #[regex(r"#[^\n]*", logos::skip)]
  Error,

  // Whitespace
  #[regex(r"\n")]
  Newline,
  #[token(" ", parse_indent)]
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
  #[token("=")]
  Equal,
  #[token("!")]
  Bang,
  #[token("<")]
  Less,
  #[token("<=")]
  LessEqual,
  #[token(">")]
  Greater,
  #[token(">=")]
  GreaterEqual,
  #[token("==")]
  EqualEqual,
  #[token("!=")]
  BangEqual,
  #[token("^")]
  Caret,
  #[token("%")]
  Percent,

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
  #[token("and")]
  And,
  #[token("or")]
  Or,

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
      TokenKind::Error => write!(f, "unknown token"),
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
      TokenKind::Equal => write!(f, "="),
      TokenKind::Bang => write!(f, "!"),
      TokenKind::Less => write!(f, "<"),
      TokenKind::LessEqual => write!(f, "<="),
      TokenKind::Greater => write!(f, ">"),
      TokenKind::GreaterEqual => write!(f, ">="),
      TokenKind::EqualEqual => write!(f, "=="),
      TokenKind::BangEqual => write!(f, "!="),
      TokenKind::Caret => write!(f, "^"),
      TokenKind::Percent => write!(f, "%"),
      TokenKind::Print => write!(f, "print"),
      TokenKind::If => write!(f, "if"),
      TokenKind::Else => write!(f, "else"),
      TokenKind::While => write!(f, "while"),
      TokenKind::And => write!(f, "and"),
      TokenKind::Or => write!(f, "or"),
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
      TokenKind::Equal => (4, 3),
      TokenKind::And | TokenKind::Or => (5, 6),
      TokenKind::EqualEqual | TokenKind::BangEqual => (7, 7),
      TokenKind::Less | TokenKind::Greater | TokenKind::LessEqual | TokenKind::GreaterEqual => {
        (8, 8)
      },
      TokenKind::Plus | TokenKind::Minus => (9, 10),
      TokenKind::Star | TokenKind::Slash | TokenKind::Percent => (11, 12),
      TokenKind::Caret => (13, 13),

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
  ShortCircuitOp { op: TokenKind, left: Box<Expr>, right: Box<Expr> },
  VarAssign { name: String, expr: Box<Expr> },
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
      ExprKind::ShortCircuitOp { op, left, right } => write!(f, "{left} {op} {right}"),
      ExprKind::VarAssign { name, expr } => write!(f, "{name} = {expr}"),
    }
  }
}

impl PartialEq for Expr {
  fn eq(&self, other: &Self) -> bool { self.kind == other.kind }
}

impl Expr {
  pub fn new(kind: ExprKind, span: Span) -> Self { Self { kind, span } }

  #[cfg(test)]
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

  #[cfg(test)]
  pub fn new_without_span(kind: StmtKind) -> Self { Self { kind, span: 0..0 } }
}