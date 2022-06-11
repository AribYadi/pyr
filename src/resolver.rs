use crate::error::{
  RuntimeError,
  RuntimeErrorKind,
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
  Variables,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValType {
  Integer,
  String,
}

// Resolves variables and type checks before interpreting.
pub struct Resolver {
  variables: Variables<ValType>,
  indent_level: IndentLevel,
}

impl Resolver {
  pub fn new() -> Self { Self { variables: Variables::new(), indent_level: 0 } }

  fn start_block(&mut self) { self.indent_level += 1 }

  fn end_block(&mut self) {
    if self.indent_level == 0 {
      unreachable!("end_block() called without start_block()");
    }
    self.variables.retain(|_, (level, _)| *level != self.indent_level);
    self.indent_level -= 1;
  }

  pub fn resolve(&mut self, stmts: &[Stmt]) -> std::result::Result<(), Vec<RuntimeError>> {
    let mut errors = Vec::new();
    for stmt in stmts {
      if let Err(err) = self.resolve_stmt(stmt) {
        errors.push(err);
      }
    }

    if !errors.is_empty() {
      Err(errors)
    } else {
      Ok(())
    }
  }

  pub(crate) fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<()> {
    match &stmt.kind {
      StmtKind::Expression { expr } => {
        self.resolve_expr(expr)?;
      },
      StmtKind::If { condition, body, else_stmt } => {
        self.resolve_expr(condition)?;
        self.start_block();
        for stmt in body {
          self.resolve_stmt(stmt)?;
        }
        self.end_block();
        self.start_block();
        for stmt in else_stmt {
          self.resolve_stmt(stmt)?;
        }
        self.end_block();
      },
      StmtKind::While { condition, body } => {
        self.resolve_expr(condition)?;
        self.start_block();
        for stmt in body {
          self.resolve_stmt(stmt)?;
        }
        self.end_block();
      },
      StmtKind::Print { expr } => {
        self.resolve_expr(expr)?;
      },
    }

    Ok(())
  }

  pub(crate) fn resolve_expr(&mut self, expr: &Expr) -> Result<ValType> {
    match &expr.kind {
      ExprKind::Integer(_) => Ok(ValType::Integer),
      ExprKind::String(_) => Ok(ValType::String),
      ExprKind::Identifier(name) => {
        self.variables.get(name).map(|(_, val_type)| *val_type).ok_or_else(|| {
          RuntimeError::new(
            RuntimeErrorKind::UndefinedVariable(name.to_string()),
            expr.span.clone(),
          )
        })
      },

      ExprKind::PrefixOp { op, right } => self.resolve_prefix_op(op, right),
      ExprKind::InfixOp { op, left, right } => self.resolve_infix_op(op, left, right),
      ExprKind::ShortCircuitOp { op, left, right } => {
        self.resolve_short_circuit_op(op, left, right)
      },
      ExprKind::VarAssign { name, expr } => {
        let val_type = self.resolve_expr(expr)?;
        self.variables.entry(name.to_string()).or_insert((self.indent_level, val_type)).1 =
          val_type;
        Ok(val_type)
      },
    }
    .map_err(|e| RuntimeError::new(e.kind, expr.span.clone()))
  }

  fn resolve_prefix_op(&mut self, op: &TokenKind, right: &Expr) -> Result<ValType> {
    let right_type = self.resolve_expr(right)?;

    match op {
      TokenKind::Minus if right_type == ValType::Integer => Ok(ValType::Integer),
      TokenKind::Bang => Ok(ValType::Integer),

      _ => Err(RuntimeError::new(RuntimeErrorKind::CannotApplyPrefix(right.clone(), *op), 0..0)),
    }
  }

  fn resolve_infix_op(&mut self, op: &TokenKind, left: &Expr, right: &Expr) -> Result<ValType> {
    let left_type = self.resolve_expr(left)?;
    let right_type = self.resolve_expr(right)?;

    match op {
      TokenKind::Plus if left_type == ValType::String || right_type == ValType::String => {
        Ok(ValType::String)
      },
      TokenKind::Star
        if (left_type == ValType::String && right_type == ValType::Integer) ||
          (right_type == ValType::String && left_type == ValType::Integer) =>
      {
        Ok(ValType::String)
      },
      TokenKind::Plus |
      TokenKind::Minus |
      TokenKind::Star |
      TokenKind::Slash |
      TokenKind::Less |
      TokenKind::Greater |
      TokenKind::LessEqual |
      TokenKind::GreaterEqual |
      TokenKind::Caret |
      TokenKind::Percent
        if left_type == ValType::Integer && right_type == ValType::Integer =>
      {
        Ok(ValType::Integer)
      },
      TokenKind::EqualEqual | TokenKind::BangEqual => Ok(ValType::Integer),

      _ => Err(RuntimeError::new(
        RuntimeErrorKind::CannotApplyInfix(left.clone(), *op, right.clone()),
        0..0,
      )),
    }
  }

  fn resolve_short_circuit_op(
    &mut self,
    op: &TokenKind,
    left: &Expr,
    right: &Expr,
  ) -> Result<ValType> {
    let left_type = self.resolve_expr(left)?;
    let right_type = self.resolve_expr(right)?;

    match op {
      // If left and right have the same type, then return that type
      TokenKind::And | TokenKind::Or if left_type == right_type => Ok(left_type),
      // Otherwise return a boolean
      TokenKind::And | TokenKind::Or => Ok(ValType::Integer),

      _ => Err(RuntimeError::new(
        RuntimeErrorKind::CannotApplyInfix(left.clone(), *op, right.clone()),
        0..0,
      )),
    }
  }
}