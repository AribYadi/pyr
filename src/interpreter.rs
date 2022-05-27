use std::io;

use crate::error::RuntimeResult as Result;
use crate::parser::syntax::{
  Expr,
  ExprKind,
  Stmt,
  StmtKind,
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

  pub(crate) fn interpret_stmt(&self, stmt: &Stmt) -> Result<()> {
    (|| match &stmt.kind {
      StmtKind::Expression { expr } => self.interpret_expr(expr).map(|_| ()),
      StmtKind::If { condition, body, else_stmt } => {
        let condition = self.interpret_expr(condition)?;
        if condition.is_truthy() {
          for stmt in body {
            self.interpret_stmt(stmt)?;
          }
        } else {
          for stmt in else_stmt {
            self.interpret_stmt(stmt)?;
          }
        }
        Ok(())
      },
      StmtKind::While { condition, body } => {
        let mut condition_lit = self.interpret_expr(condition)?;
        while condition_lit.is_truthy() {
          for stmt in body {
            self.interpret_stmt(stmt)?;
          }
          condition_lit = self.interpret_expr(condition)?;
        }
        Ok(())
      },
      // Print statement is in a different function for testing purposes
      StmtKind::Print { expr } => self.interpret_print(expr, &mut io::stdout()),
    })()
  }

  pub(crate) fn interpret_print(&self, expr: &Expr, output: &mut impl io::Write) -> Result<()> {
    let value = self.interpret_expr(expr)?;
    let _ = write!(output, "{value}");
    Ok(())
  }

  pub(crate) fn interpret_expr(&self, expr: &Expr) -> Result<Literal> {
    match &expr.kind {
      ExprKind::String(s) => Ok(Literal::String(s.clone())),
      ExprKind::Integer(n) => Ok(Literal::Integer(*n)),
      ExprKind::Identifier(_) => unimplemented!("Variables not implemented yet"),

      ExprKind::PrefixOp { op, right } => {
        let right = self.interpret_expr(right)?;
        self.interpret_prefix_op(op, right)
      },
      ExprKind::InfixOp { op, left, right } => {
        let left = self.interpret_expr(left)?;
        let right = self.interpret_expr(right)?;
        self.interpret_infix_op(op, left, right)
      },
    }
  }

  fn interpret_prefix_op(&self, op: &TokenKind, right: Literal) -> Result<Literal> {
    match op {
      TokenKind::Minus => Ok(-right),
      TokenKind::Bang => Ok(!right),

      _ => unreachable!("{op} is not a prefix operator"),
    }
  }

  fn interpret_infix_op(&self, op: &TokenKind, left: Literal, right: Literal) -> Result<Literal> {
    match op {
      TokenKind::Plus => Ok(left + right),
      TokenKind::Minus => Ok(left - right),
      TokenKind::Star => Ok(left * right),
      TokenKind::Slash => Ok(left / right),

      _ => unreachable!("{op} is not an infix operator"),
    }
  }
}

#[cfg(test)]
mod tests;
