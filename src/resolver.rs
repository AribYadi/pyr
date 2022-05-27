use crate::error::{
  RuntimeError,
  RuntimeErrorKind,
  RuntimeResult as Result,
};
use crate::parser::syntax::{
  Expr,
  ExprKind,
  TokenKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValType {
  Integer,
  String,
}

// Resolves variables and type checks before interpreting.
pub struct Resolver {}

impl Resolver {
  pub fn new() -> Self { Self {} }

  pub(crate) fn resolve_expr(&self, expr: &Expr) -> Result<ValType> {
    match &expr.kind {
      ExprKind::Integer(_) => Ok(ValType::Integer),
      ExprKind::String(_) => Ok(ValType::String),
      ExprKind::Identifier(_) => unimplemented!("Variables not implemented yet"),

      ExprKind::PrefixOp { op, right } => self.resolve_prefix_op(op, right),
      ExprKind::InfixOp { op, left, right } => self.resolve_infix_op(op, left, right),
    }
    .map_err(|e| RuntimeError::new(e.kind, expr.span.clone()))
  }

  fn resolve_prefix_op(&self, op: &TokenKind, right: &Expr) -> Result<ValType> {
    let right_type = self.resolve_expr(right)?;

    match op {
      TokenKind::Minus if right_type == ValType::Integer => Ok(ValType::Integer),
      TokenKind::Bang => Ok(ValType::Integer),

      _ => Err(RuntimeError::new(RuntimeErrorKind::CannotApplyPrefix(right.clone(), *op), 0..0)),
    }
  }

  fn resolve_infix_op(&self, op: &TokenKind, left: &Expr, right: &Expr) -> Result<ValType> {
    let left_type = self.resolve_expr(left)?;
    let right_type = self.resolve_expr(right)?;

    match op {
      TokenKind::Plus if left_type == ValType::Integer && right_type == ValType::Integer => {
        Ok(ValType::Integer)
      },
      TokenKind::Plus if left_type == ValType::String || right_type == ValType::String => {
        Ok(ValType::String)
      },
      TokenKind::Minus if left_type == ValType::Integer && right_type == ValType::Integer => {
        Ok(ValType::Integer)
      },
      TokenKind::Star if left_type == ValType::Integer && right_type == ValType::Integer => {
        Ok(ValType::Integer)
      },
      TokenKind::Star
        if (left_type == ValType::String && right_type == ValType::Integer)
          || (right_type == ValType::String && left_type == ValType::Integer) =>
      {
        Ok(ValType::String)
      },
      TokenKind::Slash if left_type == ValType::Integer && right_type == ValType::Integer => {
        Ok(ValType::Integer)
      },

      _ => Err(RuntimeError::new(
        RuntimeErrorKind::CannotApplyInfix(left.clone(), *op, right.clone()),
        0..0,
      )),
    }
  }
}
