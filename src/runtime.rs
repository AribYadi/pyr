use std::collections::HashMap;

mod impl_arithmetics;

pub type IndentLevel = usize;
pub type Variables<T> = HashMap<String, (IndentLevel, T)>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  String(String),
  Integer(i64),
}

impl std::fmt::Display for Literal {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Literal::String(s) => write!(f, "{s}"),
      Literal::Integer(n) => write!(f, "{n}"),
    }
  }
}

impl Literal {
  pub fn is_truthy(&self) -> bool {
    match self {
      Literal::Integer(n) => *n == 1,
      Literal::String(s) => !s.is_empty(),
    }
  }

  pub fn is_same_variant(&self, other: &Self) -> bool {
    matches!(
      (self, other),
      (Literal::String(_), Literal::String(_)) | (Literal::Integer(_), Literal::Integer(_))
    )
  }
}