use std::io;

use crate::error::{
  RuntimeError,
  RuntimeResult as Result,
};
use crate::parser::syntax::{
  Expr,
  ExprKind,
  Stmt,
  StmtKind,
  TokenKind,
};
use crate::runtime::{
  IndentLevel,
  Literal,
  Variables,
};

#[derive(Clone)]
pub struct Interpreter {
  variables: Variables<Literal>,
  indent_level: IndentLevel,
}

impl Interpreter {
  pub fn new() -> Self { Self { variables: Variables::new(), indent_level: 0 } }

  fn start_block(&mut self) { self.indent_level += 1; }

  fn end_block(&mut self) {
    self.variables.retain(|_, (level, _)| *level != self.indent_level);
    self.indent_level -= 1;
  }

  pub fn interpret(&mut self, stmts: &[Stmt]) -> std::result::Result<(), RuntimeError> {
    for stmt in stmts {
      self.interpret_stmt(stmt)?;
    }
    Ok(())
  }

  pub(crate) fn interpret_stmt(&mut self, stmt: &Stmt) -> Result<()> {
    match &stmt.kind {
      StmtKind::Expression { expr } => self.interpret_expr(expr).map(|_| ()),
      StmtKind::If { condition, body, else_stmt } => {
        let condition = self.interpret_expr(condition)?;
        self.start_block();
        if condition.is_truthy() {
          for stmt in body {
            self.interpret_stmt(stmt)?;
          }
        } else {
          for stmt in else_stmt {
            self.interpret_stmt(stmt)?;
          }
        }
        self.end_block();
        Ok(())
      },
      StmtKind::While { condition, body } => {
        let mut condition_lit = self.interpret_expr(condition)?;
        while condition_lit.is_truthy() {
          self.start_block();
          for stmt in body {
            self.interpret_stmt(stmt)?;
          }
          condition_lit = self.interpret_expr(condition)?;
          self.end_block();
        }
        Ok(())
      },
      // Print statement is in a different function for testing purposes
      StmtKind::Print { expr } => self.interpret_print(expr, &mut io::stdout()),
    }
  }

  pub(crate) fn interpret_print(&mut self, expr: &Expr, output: &mut impl io::Write) -> Result<()> {
    let value = self.interpret_expr(expr)?;
    let _ = write!(output, "{value}");
    Ok(())
  }

  pub(crate) fn interpret_expr(&mut self, expr: &Expr) -> Result<Literal> {
    match &expr.kind {
      ExprKind::String(s) => Ok(Literal::String(s.to_string())),
      ExprKind::Integer(n) => Ok(Literal::Integer(*n)),
      ExprKind::Identifier(name) => match self.variables.get(name) {
        Some((_, val)) => Ok(val.clone()),
        None => unreachable!("Resolver didn't resolve variable correctly"),
      },

      ExprKind::PrefixOp { op, right } => {
        let right = self.interpret_expr(right)?;
        self.interpret_prefix_op(op, right)
      },
      ExprKind::InfixOp { op, left, right } => {
        let left = self.interpret_expr(left)?;
        let right = self.interpret_expr(right)?;
        self.interpret_infix_op(op, left, right)
      },
      ExprKind::ShortCircuitOp { op, left, right } => {
        self.interpret_short_circuit_op(op, *left.clone(), *right.clone())
      },
      ExprKind::VarAssign { name, expr } => {
        let value = self.interpret_expr(expr)?;
        self.variables.entry(name.to_string()).or_insert((self.indent_level, value.clone())).1 =
          value.clone();
        Ok(value)
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
      TokenKind::Less => Ok(left.lt(right)),
      TokenKind::LessEqual => Ok(left.le(right)),
      TokenKind::Greater => Ok(left.gt(right)),
      TokenKind::GreaterEqual => Ok(left.ge(right)),
      TokenKind::EqualEqual => Ok(left.eq(right)),
      TokenKind::BangEqual => Ok(left.ne(right)),
      TokenKind::Caret => Ok(left.pow(right)),

      _ => unreachable!("{op} is not an infix operator"),
    }
  }

  fn interpret_short_circuit_op(
    &mut self,
    op: &TokenKind,
    left: Expr,
    right: Expr,
  ) -> Result<Literal> {
    let left = self.interpret_expr(&left)?;
    let right_lit = self.clone().interpret_expr(&right)?;
    match op {
      TokenKind::And if right_lit.is_same_variant(&left) => {
        if !left.is_truthy() {
          return Ok(left);
        }
        self.interpret_expr(&right)
      },
      TokenKind::And if left.is_truthy() => {
        Ok(Literal::Integer(self.interpret_expr(&right)?.is_truthy() as i64))
      },
      TokenKind::And => Ok(Literal::Integer(0)),
      TokenKind::Or if right_lit.is_same_variant(&left) => {
        if left.is_truthy() {
          return Ok(left);
        }
        self.interpret_expr(&right)
      },
      TokenKind::Or if left.is_truthy() => Ok(Literal::Integer(1)),
      TokenKind::Or => Ok(Literal::Integer(self.interpret_expr(&right)?.is_truthy() as i64)),

      _ => unreachable!("{op} is not a short circuit operator"),
    }
  }
}

#[cfg(test)]
mod tests;
