use std::sync::Mutex;

use logos::internal::LexerInternal;
use logos::{
  Lexer,
  Logos,
};

use crate::error::Span;
use crate::runtime::{
  ReturnValue,
  ValueType,
};

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
  #[token("<<")]
  LeftShift,
  #[token(">>")]
  RightShift,
  #[token("&")]
  Ampersand,
  #[token("|")]
  Pipe,

  // Keywords
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
  #[token("func")]
  Func,
  #[token("ret")]
  Ret,

  // Delimiters
  #[token("(")]
  LeftParen,
  #[token(")")]
  RightParen,
  #[token("[")]
  LeftBracket,
  #[token("]")]
  RightBracket,

  // Types
  #[token("int")]
  IntType,
  #[token("string")]
  StringType,

  // Syntax
  #[token(":")]
  Colon,
  #[token(",")]
  Comma,
  #[token("->")]
  Arrow,
  #[token(";")]
  Semicolon,
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
      TokenKind::LeftShift => write!(f, "<<"),
      TokenKind::RightShift => write!(f, ">>"),
      TokenKind::Ampersand => write!(f, "&"),
      TokenKind::Pipe => write!(f, "|"),
      TokenKind::If => write!(f, "if"),
      TokenKind::Else => write!(f, "else"),
      TokenKind::While => write!(f, "while"),
      TokenKind::And => write!(f, "and"),
      TokenKind::Or => write!(f, "or"),
      TokenKind::Func => write!(f, "func"),
      TokenKind::Ret => write!(f, "ret"),
      TokenKind::LeftParen => write!(f, "("),
      TokenKind::RightParen => write!(f, ")"),
      TokenKind::LeftBracket => write!(f, "["),
      TokenKind::RightBracket => write!(f, "]"),
      TokenKind::IntType => write!(f, "type `int`"),
      TokenKind::StringType => write!(f, "type `string`"),
      TokenKind::Colon => write!(f, ":"),
      TokenKind::Comma => write!(f, ","),
      TokenKind::Arrow => write!(f, "->"),
      TokenKind::Semicolon => write!(f, ";"),
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
      TokenKind::Minus | TokenKind::Bang => ((), 12),
      _ => unreachable!("{self} is not a prefix operator"),
    }
  }

  fn infix_bp(&self) -> (u8, u8) {
    match self {
      TokenKind::Equal => (2, 1),
      TokenKind::And | TokenKind::Or => (3, 4),
      TokenKind::Pipe => (5, 6),
      TokenKind::Ampersand => (7, 8),
      TokenKind::EqualEqual | TokenKind::BangEqual => (9, 9),
      TokenKind::Less | TokenKind::Greater | TokenKind::LessEqual | TokenKind::GreaterEqual => {
        (10, 10)
      },
      TokenKind::LeftShift | TokenKind::RightShift => (11, 12),
      TokenKind::Plus | TokenKind::Minus => (13, 14),
      TokenKind::Star | TokenKind::Slash | TokenKind::Percent => (15, 16),
      TokenKind::Caret => (18, 17),

      _ => unreachable!("{self} is not an infix operator"),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
  String(String),
  Integer(i64),
  Identifier(String),
  Array(ValueType, Vec<Expr>, usize),

  PrefixOp { op: TokenKind, right: Box<Expr> },
  InfixOp { op: TokenKind, left: Box<Expr>, right: Box<Expr> },
  ShortCircuitOp { op: TokenKind, left: Box<Expr>, right: Box<Expr> },
  VarAssign { name: String, expr: Box<Expr> },
  FuncCall { name: String, params: Vec<Expr> },
  Index { array: Box<Expr>, index: Box<Expr> },
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
      ExprKind::Array(ty, elems, len) => write!(
        f,
        "{ty}[{elems}{len}]",
        elems = elems.iter().map(ToString::to_string).collect::<Vec<_>>().join(", "),
        len = if *len == elems.len() { "".to_string() } else { format!("; {len}") }
      ),
      ExprKind::PrefixOp { op, right } => write!(f, "{op}{right}"),
      ExprKind::InfixOp { op, left, right } => write!(f, "{left} {op} {right}"),
      ExprKind::ShortCircuitOp { op, left, right } => write!(f, "{left} {op} {right}"),
      ExprKind::VarAssign { name, expr } => write!(f, "{name} = {expr}"),
      ExprKind::FuncCall { name, params } => write!(
        f,
        "{name}({args})",
        args = params.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
      ),
      ExprKind::Index { array, index } => write!(f, "{array}[{index}]"),
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
  Expression {
    expr: Expr,
  },
  If {
    condition: Expr,
    body: Vec<Stmt>,
    else_stmt: Vec<Stmt>,
  },
  While {
    condition: Expr,
    body: Vec<Stmt>,
  },
  FuncDef {
    name: String,
    args: Vec<(String, ValueType)>,
    body: Vec<Stmt>,
    ret_ty: ReturnValue<ValueType>,
  },
  Ret {
    expr: Option<Expr>,
  },
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
