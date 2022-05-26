mod impl_arithmetics;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  String(String),
  Integer(i64),
}

impl std::fmt::Display for Literal {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Literal::String(s) => write!(f, "\"{}\"", s),
      Literal::Integer(n) => write!(f, "{}", n),
    }
  }
}

impl Literal {
  pub fn as_string(&self) -> &str {
    match self {
      Literal::String(s) => s,
      _ => unreachable!("{self} is not a string"),
    }
  }

  pub fn as_integer(&self) -> i64 {
    match self {
      Literal::Integer(n) => *n,
      _ => unreachable!("{self} is not an integer"),
    }
  }

  fn is_truthy(&self) -> bool {
    match self {
      Literal::Integer(n) => *n != 0,
      Literal::String(s) => !s.is_empty(),
    }
  }
}
