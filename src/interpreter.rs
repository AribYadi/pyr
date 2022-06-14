use std::io;

use crate::error::{
  RuntimeError,
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
  Function,
  IndentLevel,
  Literal,
  ReturnValue,
  Variables,
};

#[derive(Clone)]
pub struct Interpreter {
  variables: Variables<Literal>,
  // Functions can return something, so we need to store the return type.
  functions: Variables<Function>,
  indent_level: IndentLevel,

  // Since functions can return nothing, we need to track whether are we ignoring the return value.
  ignore_return: bool,
  return_value: ReturnValue<Literal>,
  return_: bool,
}

impl Interpreter {
  pub fn new() -> Self {
    Self {
      variables: Variables::new(),
      functions: Variables::new(),
      indent_level: 0,
      ignore_return: false,
      return_value: None,
      return_: false,
    }
  }

  fn start_block(&mut self) { self.indent_level += 1; }

  fn end_block(&mut self) {
    self.variables.remove_all_with_indent(self.indent_level);
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
      StmtKind::Expression { expr } => {
        ignore_return!(self, expr, self.interpret_expr(expr)?);
        Ok(())
      },
      StmtKind::If { condition, body, else_stmt } => {
        let condition =
          ignore_return!(NEVER_WITH_RET; self, condition, self.interpret_expr(condition)?);
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
        let mut condition_lit =
          ignore_return!(NEVER_WITH_RET; self, condition, self.interpret_expr(condition)?);
        while condition_lit.is_truthy() {
          self.start_block();
          for stmt in body {
            self.interpret_stmt(stmt)?;
          }
          condition_lit =
            ignore_return!(NEVER_WITH_RET; self, condition, self.interpret_expr(condition)?);
          self.end_block();
        }
        Ok(())
      },
      // Print statement is in a different function for testing purposes
      StmtKind::Print { expr } => self.interpret_print(expr, &mut io::stdout()),
      StmtKind::FuncDef { name, args, body, ret_ty } => {
        // Clear return value first
        self.return_value = None;

        let arg_types: Vec<_> = args.iter().map(|(_, ty)| *ty).collect();
        let args = args.iter().map(|(arg, _)| arg.clone()).collect();
        self.functions.declare(
          &func_name(name, &arg_types),
          self.indent_level,
          Function::UserDefined(args, body.to_vec(), *ret_ty),
        );
        Ok(())
      },
      StmtKind::Ret { expr } => {
        if let Some(expr) = expr {
          self.return_value =
            Some(ignore_return!(NEVER_WITH_RET; self, expr, self.interpret_expr(expr)?));
        } else {
          self.return_value = None;
        }

        self.return_ = true;
        Ok(())
      },
    }
  }

  pub(crate) fn interpret_print(&mut self, expr: &Expr, output: &mut impl io::Write) -> Result<()> {
    let value = ignore_return!(NEVER_WITH_RET; self, expr, self.interpret_expr(expr)?);
    let _ = write!(output, "{value}");
    Ok(())
  }

  pub(crate) fn interpret_expr(&mut self, expr: &Expr) -> Result<Literal> {
    match &expr.kind {
      ExprKind::String(s) => Ok(Literal::String(s.to_string())),
      ExprKind::Integer(n) => Ok(Literal::Integer(*n)),
      ExprKind::Identifier(name) => match self.variables.get(name) {
        Some(val) => Ok(val),
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
        self.variables.assign_or_declare(name, self.indent_level, value.clone());
        Ok(value)
      },
      ExprKind::FuncCall { name, params } => {
        let params =
          params.iter().map(|param| self.interpret_expr(param)).collect::<Result<Vec<_>>>()?;
        let param_types: Vec<_> = params.iter().map(|param| param.get_type()).collect();
        let func = match self.functions.get(&func_name(name, &param_types)) {
          Some(func) => func,
          None => unreachable!("Resolver didn't resolve function correctly"),
        };

        match func {
          Function::Native(func, arg_len) => {
            if params.len() != arg_len {
              unreachable!("Native function called with wrong number of arguments");
            }
            match func(self, params) {
              Some(value) => Ok(value),
              None if !self.ignore_return => unreachable!("Native function returned nothing"),
              None => Ok(Literal::Integer(1656)),
            }
          },
          Function::UserDefined(args, body, _) => {
            if params.len() != args.len() {
              unreachable!("User defined function called with wrong number of arguments");
            }

            self.start_block();
            for (arg, param) in args.iter().zip(params) {
              self.variables.declare(arg, self.indent_level, param);
            }

            for stmt in body {
              if self.return_ {
                break;
              }
              self.interpret_stmt(&stmt)?;
            }
            self.end_block();

            let return_value = self.return_value.clone();

            self.return_value = None;
            self.return_ = false;

            match return_value {
              Some(value) => Ok(value),
              None if self.ignore_return => Ok(Literal::Integer(1656)),
              None => unreachable!("User defined function returned nothing"),
            }
          },
        }
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
      TokenKind::Percent => Ok(left.mod_(right)),

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
