use crate::error::InterpretResult as Result;
use crate::parser::syntax::{
  Expr,
  TokenKind,
};
use crate::runtime::Literal;

pub struct Interpreter {
  lines: Vec<String>,
  curr_line: usize,
}

impl Interpreter {
  pub fn new(source: &str) -> Self {
    Self { lines: source.lines().map(str::to_string).collect(), curr_line: 0 }
  }

  pub(crate) fn interpret_expr(&self, expr: &Expr) -> Result<Literal> {
    match expr {
      Expr::String(s) => Ok(Literal::String(s.clone())),
      Expr::Integer(n) => Ok(Literal::Integer(*n)),
      Expr::Identifier(_) => todo!(),

      Expr::PrefixOp { op, right } => {
        let right = self.interpret_expr(right)?;
        self.interpret_prefix_op(op, right)
      },
      Expr::InfixOp { op, left, right } => {
        let left = self.interpret_expr(left)?;
        let right = self.interpret_expr(right)?;
        self.interpret_infix_op(op, left, right)
      },
    }
  }

  fn interpret_prefix_op(&self, op: &TokenKind, right: Literal) -> Result<Literal> {
    match op {
      TokenKind::Minus => Ok((-right)?),
      TokenKind::Bang => Ok((!right)?),

      _ => unreachable!("{op} is not a prefix operator"),
    }
  }

  fn interpret_infix_op(&self, op: &TokenKind, left: Literal, right: Literal) -> Result<Literal> {
    match op {
      TokenKind::Plus => Ok((left + right)?),
      TokenKind::Minus => Ok((left - right)?),
      TokenKind::Star => Ok((left * right)?),
      TokenKind::Slash => Ok((left / right)?),

      _ => unreachable!("{op} is not an infix operator"),
    }
  }
}

#[cfg(test)]
mod tests;
