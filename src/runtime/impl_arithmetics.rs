use std::ops::{
  Add,
  BitAnd,
  BitOr,
  Div,
  Mul,
  Neg,
  Not,
  Shl,
  Shr,
  Sub,
};

use super::Literal;

impl Add for Literal {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
    match (self.clone(), rhs.clone()) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l + r),
      (Literal::String(s), _) => Literal::String([s, rhs.to_string()].concat()),
      (_, Literal::String(s)) => Literal::String([self.to_string(), s].concat()),
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

impl Literal {
  pub fn lt(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer((l < r) as i64),
      _ => {
        unreachable!("Resolver didn't type check infix operator `<`");
      },
    }
  }

  pub fn le(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer((l <= r) as i64),
      _ => {
        unreachable!("Resolver didn't type check infix operator `<=`");
      },
    }
  }

  pub fn gt(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer((l > r) as i64),
      _ => {
        unreachable!("Resolver didn't type check infix operator `>`");
      },
    }
  }

  pub fn ge(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer((l >= r) as i64),
      _ => {
        unreachable!("Resolver didn't type check infix operator `>=`");
      },
    }
  }

  pub fn eq(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer((l == r) as i64),
      (Literal::String(l), Literal::String(r)) => Literal::Integer((l == r) as i64),
      _ => Literal::Integer(0),
    }
  }

  pub fn ne(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer((l != r) as i64),
      (Literal::String(l), Literal::String(r)) => Literal::Integer((l != r) as i64),
      _ => Literal::Integer(1),
    }
  }

  pub fn pow(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l.pow(r as u32)),
      _ => {
        unreachable!("Resolver didn't type check infix operator `^`");
      },
    }
  }

  pub fn mod_(self, rhs: Self) -> Self {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l % r),
      _ => {
        unreachable!("Resolver didn't type check infix operator `%`");
      },
    }
  }
}

impl Shl for Literal {
  type Output = Self;

  fn shl(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l << r),
      _ => {
        unreachable!("Resolver didn't type check infix operator `<<`");
      },
    }
  }
}

impl Shr for Literal {
  type Output = Self;

  fn shr(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l >> r),
      _ => {
        unreachable!("Resolver didn't type check infix operator `>>`");
      },
    }
  }
}

impl BitAnd for Literal {
  type Output = Self;

  fn bitand(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l & r),
      _ => {
        unreachable!("Resolver didn't type check infix operator `&`");
      },
    }
  }
}

impl BitOr for Literal {
  type Output = Self;

  fn bitor(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Literal::Integer(l), Literal::Integer(r)) => Literal::Integer(l | r),
      _ => {
        unreachable!("Resolver didn't type check infix operator `|`");
      },
    }
  }
}
