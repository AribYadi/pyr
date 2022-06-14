use crate::error::{
  RuntimeError,
  RuntimeErrorKind,
  RuntimeResult as Result,
};
use crate::ignore_return;
use crate::parser::syntax::{
  Expr,
  ExprKind,
  Stmt,
  StmtKind,
  TokenKind,
};
use crate::runtime::{
  func_name,
  IndentLevel,
  ReturnValue,
  ValueType,
  Variables,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
  TopLevel,
  Function,
}

// Resolves variables, functions, and type checks before interpreting.
pub struct Resolver {
  variables: Variables<ValueType>,
  // Functions can return something, so we need to store the return type.
  pub functions: Variables<(Vec<ValueType>, ReturnValue<ValueType>)>,
  indent_level: IndentLevel,

  // Since functions can return nothing, we need to track whether are we ignoring the return value.
  ignore_return: bool,
  return_value: ReturnValue<ValueType>,

  state: State,
}

impl Resolver {
  pub fn new() -> Self {
    Self {
      variables: Variables::new(),
      functions: Variables::new(),
      indent_level: 0,
      ignore_return: false,
      return_value: None,
      state: State::TopLevel,
    }
  }

  fn start_block(&mut self) { self.indent_level += 1 }

  fn end_block(&mut self) {
    if self.indent_level == 0 {
      unreachable!("end_block() called without start_block()");
    }
    self.variables.remove_all_with_indent(self.indent_level);
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
        ignore_return!(self, expr, self.resolve_expr(expr)?);
      },
      StmtKind::If { condition, body, else_stmt } => {
        ignore_return!(NEVER; self, condition, self.resolve_expr(condition)?);
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
        ignore_return!(NEVER; self, condition, self.resolve_expr(condition)?);
        self.start_block();
        for stmt in body {
          self.resolve_stmt(stmt)?;
        }
        self.end_block();
      },
      StmtKind::Print { expr } => {
        ignore_return!(NEVER; self, expr, self.resolve_expr(expr)?);
      },
      StmtKind::FuncDef { name, args, body, ret_ty } => {
        // Clear return value first
        self.return_value = None;

        let mut arg_types = Vec::new();
        self.start_block();
        for (arg, ty) in args {
          self.variables.declare(arg, self.indent_level, *ty);
          arg_types.push(*ty);
        }
        self.functions.declare(
          &func_name(name, &arg_types),
          self.indent_level,
          (arg_types, *ret_ty),
        );

        let prev_state = self.state;
        self.state = State::Function;

        for stmt in body {
          // Check if return value is some and if it is match it to the return type.
          if self.return_value.is_some() && ret_ty.is_some() && self.return_value != *ret_ty {
            return Err(RuntimeError::new(
              RuntimeErrorKind::ReturnTypeMismatch(
                name.to_string(),
                ValueType::to_tok_void(self.return_value),
                ValueType::to_tok_void(*ret_ty),
              ),
              stmt.span.clone(),
            ));
          }

          self.resolve_stmt(stmt)?;
        }

        self.state = prev_state;

        // Check again if return value is of the correct type
        if self.return_value != *ret_ty {
          return Err(RuntimeError::new(
            RuntimeErrorKind::ReturnTypeMismatch(
              name.to_string(),
              ValueType::to_tok_void(self.return_value),
              ValueType::to_tok_void(*ret_ty),
            ),
            stmt.span.clone(),
          ));
        }

        self.end_block();
      },
      StmtKind::Ret { expr } => {
        if self.state != State::Function {
          return Err(RuntimeError::new(
            RuntimeErrorKind::ReturnOutsideFunction,
            stmt.span.clone(),
          ));
        }

        if let Some(expr) = expr {
          self.return_value =
            Some(ignore_return!(NEVER_WITH_RET; self, expr, self.resolve_expr(expr)?));
          return Ok(());
        }

        self.return_value = None;
      },
    }

    Ok(())
  }

  pub(crate) fn resolve_expr(&mut self, expr: &Expr) -> Result<ValueType> {
    match &expr.kind {
      ExprKind::Integer(_) => Ok(ValueType::Integer),
      ExprKind::String(_) => Ok(ValueType::String),
      ExprKind::Identifier(name) => self.variables.get(name).ok_or_else(|| {
        RuntimeError::new(RuntimeErrorKind::UndefinedVariable(name.to_string()), expr.span.clone())
      }),

      ExprKind::PrefixOp { op, right } => self.resolve_prefix_op(op, right),
      ExprKind::InfixOp { op, left, right } => self.resolve_infix_op(op, left, right),
      ExprKind::ShortCircuitOp { op, left, right } => {
        self.resolve_short_circuit_op(op, left, right)
      },
      ExprKind::VarAssign { name, expr } => {
        let val_type = self.resolve_expr(expr)?;
        Ok(self.variables.assign_or_declare(name, self.indent_level, val_type))
      },
      ExprKind::FuncCall { name, params } => {
        let param_types =
          params.iter().map(|expr| self.resolve_expr(expr)).collect::<Result<Vec<_>>>()?;
        let (arg_types, ret_ty) =
          self.functions.get(&func_name(name, &param_types)).ok_or_else(|| {
            RuntimeError::new(
              RuntimeErrorKind::UndefinedFunctionWithParams(name.to_string(), param_types.clone()),
              expr.span.clone(),
            )
          })?;
        if ret_ty.is_none() && !self.ignore_return {
          return Err(RuntimeError::new(
            RuntimeErrorKind::FunctionReturnsNothing(name.to_string()),
            expr.span.clone(),
          ));
        }

        if params.len() != arg_types.len() {
          return Err(RuntimeError::new(
            RuntimeErrorKind::FunctionArgumentCountMismatch(
              name.to_string(),
              arg_types.len(),
              params.len(),
            ),
            expr.span.clone(),
          ));
        }

        for (i, (param_ty, arg_ty)) in param_types.iter().zip(arg_types).enumerate() {
          if *param_ty != arg_ty {
            return Err(RuntimeError::new(
              RuntimeErrorKind::FunctionArgumentTypeMismatch(
                name.to_string(),
                i,
                param_ty.to_tok(),
                arg_ty.to_tok(),
              ),
              expr.span.clone(),
            ));
          }
        }

        if self.ignore_return {
          Ok(ValueType::Integer)
        } else {
          Ok(ret_ty.unwrap())
        }
      },
    }
    .map_err(|e| RuntimeError::new(e.kind, expr.span.clone()))
  }

  fn resolve_prefix_op(&mut self, op: &TokenKind, right: &Expr) -> Result<ValueType> {
    let right_type = self.resolve_expr(right)?;

    match op {
      TokenKind::Minus if right_type == ValueType::Integer => Ok(ValueType::Integer),
      TokenKind::Bang => Ok(ValueType::Integer),

      _ => Err(RuntimeError::new(RuntimeErrorKind::CannotApplyPrefix(right.clone(), *op), 0..0)),
    }
  }

  fn resolve_infix_op(&mut self, op: &TokenKind, left: &Expr, right: &Expr) -> Result<ValueType> {
    let left_type = self.resolve_expr(left)?;
    let right_type = self.resolve_expr(right)?;

    match op {
      TokenKind::Plus if left_type == ValueType::String || right_type == ValueType::String => {
        Ok(ValueType::String)
      },
      TokenKind::Star
        if (left_type == ValueType::String && right_type == ValueType::Integer) ||
          (right_type == ValueType::String && left_type == ValueType::Integer) =>
      {
        Ok(ValueType::String)
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
        if left_type == ValueType::Integer && right_type == ValueType::Integer =>
      {
        Ok(ValueType::Integer)
      },
      TokenKind::EqualEqual | TokenKind::BangEqual => Ok(ValueType::Integer),

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
  ) -> Result<ValueType> {
    let left_type = self.resolve_expr(left)?;
    let right_type = self.resolve_expr(right)?;

    match op {
      // If left and right have the same type, then return that type
      TokenKind::And | TokenKind::Or if left_type == right_type => Ok(left_type),
      // Otherwise return a boolean
      TokenKind::And | TokenKind::Or => Ok(ValueType::Integer),

      _ => Err(RuntimeError::new(
        RuntimeErrorKind::CannotApplyInfix(left.clone(), *op, right.clone()),
        0..0,
      )),
    }
  }
}
