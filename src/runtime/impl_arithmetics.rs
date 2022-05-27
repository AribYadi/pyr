use std::ops::{
  Add,
  Div,
  Mul,
  Neg,
  Not,
  Sub,
};

use super::Literal;

impl Add for Literal {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
    match (self, rhs.clone()) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l + r),
      (Literal::String(s), _) | (_, Literal::String(s)) => Literal::String(format!("{s}{rhs}")),
      #[allow(unreachable_patterns)]
      _ => {
        unreachable!("Resolver didn't type check infix operator `+`");
      },
    }
  }
}

impl Sub for Literal {
  type Output = Self;

  fn sub(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l - r),
      _ => {
        unreachable!("Resolver didn't type check infix operator `-`");
      },
    }
  }
}

impl Mul for Literal {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l * r),
      (Literal::String(s), Literal::Integer(n)) | (Literal::Integer(n), Literal::String(s)) => {
        let mut string = String::new();
        for _ in 0..n {
          string.push_str(&s);
        }
        Literal::String(string)
      },
      _ => {
        unreachable!("Resolver didn't type check infix operator `*`");
      },
    }
  }
}

impl Div for Literal {
  type Output = Self;

  fn div(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l / r),
      _ => {
        unreachable!("Resolver didn't type check infix operator `/`");
      },
    }
  }
}

impl Neg for Literal {
  type Output = Self;

  fn neg(self) -> Self::Output {
    match self {
      Literal::Integer(n) => Literal::Integer(-n),
      _ => unreachable!("Resolver didn't type check prefix operator `-`"),
    }
  }
}

impl Not for Literal {
  type Output = Self;

  fn not(self) -> Self::Output { Literal::Integer(!self.is_truthy() as i64) }
}