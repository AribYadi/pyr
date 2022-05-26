use std::ops::{
  Add,
  Div,
  Mul,
  Neg,
  Not,
  Sub,
};

use crate::error::{
  InterpretError,
  InterpretErrorKind,
  InterpretResult,
};
use crate::parser::syntax::TokenKind;

use super::Literal;

impl Add for Literal {
  type Output = InterpretResult<Self>;

  fn add(self, rhs: Self) -> Self::Output {
    match (self.clone(), rhs.clone()) {
      (Literal::Integer(l), Literal::Integer(r)) => Ok(Literal::Integer(l + r)),
      (Literal::String(l), Literal::String(r)) => Ok(Literal::String(l + &r)),
      (Literal::String(s), Literal::Integer(n)) | (Literal::Integer(n), Literal::String(s)) => {
        Ok(Literal::String(format!("{}{}", s, n)))
      },
      #[allow(unreachable_patterns)]
      _ => {
        Err(InterpretError::new(InterpretErrorKind::CannotApplyInfix(self, TokenKind::Plus, rhs)))
      },
    }
  }
}

impl Sub for Literal {
  type Output = InterpretResult<Self>;

  fn sub(self, rhs: Self) -> Self::Output {
    match (self.clone(), rhs.clone()) {
      (Literal::Integer(l), Literal::Integer(r)) => Ok(Literal::Integer(l - r)),
      _ => {
        Err(InterpretError::new(InterpretErrorKind::CannotApplyInfix(self, TokenKind::Minus, rhs)))
      },
    }
  }
}

impl Mul for Literal {
  type Output = InterpretResult<Self>;

  fn mul(self, rhs: Self) -> Self::Output {
    match (self.clone(), rhs.clone()) {
      (Literal::Integer(l), Literal::Integer(r)) => Ok(Literal::Integer(l * r)),
      (Literal::String(s), Literal::Integer(n)) | (Literal::Integer(n), Literal::String(s)) => {
        let mut string = String::new();
        for _ in 0..n {
          string.push_str(&s);
        }
        Ok(Literal::String(string))
      },
      _ => {
        Err(InterpretError::new(InterpretErrorKind::CannotApplyInfix(self, TokenKind::Star, rhs)))
      },
    }
  }
}

impl Div for Literal {
  type Output = InterpretResult<Self>;

  fn div(self, rhs: Self) -> Self::Output {
    match (self.clone(), rhs.clone()) {
      (Literal::Integer(l), Literal::Integer(r)) => Ok(Literal::Integer(l / r)),
      _ => {
        Err(InterpretError::new(InterpretErrorKind::CannotApplyInfix(self, TokenKind::Slash, rhs)))
      },
    }
  }
}

impl Neg for Literal {
  type Output = InterpretResult<Self>;

  fn neg(self) -> Self::Output {
    match self {
      Literal::Integer(n) => Ok(Literal::Integer(-n)),
      _ => Err(InterpretError::new(InterpretErrorKind::CannotApplyPrefix(self, TokenKind::Minus))),
    }
  }
}

impl Not for Literal {
  type Output = InterpretResult<Self>;

  fn not(self) -> Self::Output {
    match self {
      Literal::Integer(n) if n <= 1 => Ok(Literal::Integer(1 - n)),
      _ => Err(InterpretError::new(InterpretErrorKind::CannotApplyPrefix(self, TokenKind::Bang))),
    }
  }
}
