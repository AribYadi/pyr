use std::process;

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
  func_name,
  Function,
  IndentLevel,
  Literal,
  Params,
  ReturnValue,
  State,
  StateStack,
  Variables,
};
use crate::{
  ignore_return,
  info,
  utils,
};

pub struct Interpreter {
  variables: Variables<Literal>,
  // Functions can return something, so we need to store the return type.
  functions: Variables<Function>,
  indent_level: IndentLevel,

  // Since functions can return nothing, we need to track whether are we ignoring the return value.
  ignore_return: bool,
  return_value: ReturnValue<Literal>,

  state_stack: StateStack,

  shared_libraries: Vec<crate::libloading::Library>,
}

impl Interpreter {
  pub fn define_std(&mut self) {
    let print_func = |_: &mut Self, params: Params| {
      let expr = params[0].to_string();
      print!("{expr}");
      None
    };
    self.functions.declare("print.string", 0, Function::Native(print_func, 1));
    self.functions.declare("print.int", 0, Function::Native(print_func, 1));

    self.functions.declare(
      "sqrt.int",
      0,
      Function::Native(
        |_, params| {
          let param = match params[0] {
            Literal::Integer(n) => n,
            _ => info!(INTR_ERR, "Resolver didn't resolve function parameters correctly"),
          };
          Some(Literal::Integer(f64::sqrt(param as f64) as i64))
        },
        1,
      ),
    );

    self.functions.declare(
      "to_int.string",
      0,
      Function::Native(
        |_, params| {
          let param = match &params[0] {
            Literal::String(s) => s,
            _ => info!(INTR_ERR, "Resolver didn't resolve function parameters correctly"),
          };
          let as_int = match param.parse() {
            Ok(n) => n,
            Err(_) => {
              info!(ERR, "`{param:?}` is not able to be converted into an integer.");
              process::exit(1);
            },
          };
          Some(Literal::Integer(as_int))
        },
        1,
      ),
    );
  }

  pub fn new(shared_libraries: &[String]) -> Self {
    let shared_libraries = shared_libraries
      .iter()
      .map(|path| unsafe { crate::libloading::Library::new(path).unwrap() })
      .collect();

    Self {
      variables: Variables::new(),
      functions: Variables::new(),
      indent_level: 0,
      ignore_return: false,
      return_value: None,
      state_stack: StateStack::new(),
      shared_libraries,
    }
  }

  fn start_block(&mut self) { self.indent_level += 1; }

  fn end_block(&mut self) {
    self.variables.remove_all_with_indent(self.indent_level);
    self.functions.remove_all_with_indent(self.indent_level);
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
        self.start_block();

        self.state_stack.push(State::Loop);
        'while_loop: while condition_lit.is_truthy() {
          for stmt in body {
            // We don't care if its a `ret` or `break`
            if self.state_stack.last_rewind.is_some() {
              break 'while_loop;
            }
            self.interpret_stmt(stmt)?;
          }
          condition_lit =
            ignore_return!(NEVER_WITH_RET; self, condition, self.interpret_expr(condition)?);
        }
        self.state_stack.pop(State::Loop);

        self.end_block();
        Ok(())
      },
      StmtKind::FuncDef { name, args, body, ret_ty } => {
        // Clear return value first
        self.return_value = None;

        let (new_args, arg_types): (_, Vec<_>) = args.iter().cloned().unzip();
        self.functions.declare(
          &func_name(name, &arg_types),
          self.indent_level,
          Function::UserDefined(new_args, body.clone(), ret_ty.clone()),
        );
        Ok(())
      },
      StmtKind::FuncExtern { name, args, ret_ty } => {
        let arg_types: Vec<_> = args.iter().map(|(_, ty)| ty.clone()).collect();
        self.functions.declare(
          &func_name(name, &arg_types),
          self.indent_level,
          Function::External(
            unsafe { utils::get_sym_from_multiple_so(&self.shared_libraries, name) }
              .unwrap_or_else(|| {
                info!(ERR, "External function `{name}` not found!");
                process::exit(1);
              }),
            arg_types.len(),
            ret_ty.clone(),
          ),
        );
        Ok(())
      },
      StmtKind::Ret { expr } => {
        if !self.state_stack.contains(State::Function) {
          info!(INTR_ERR, "Resolver didn't catch return outside of function");
        }

        if let Some(expr) = expr {
          self.return_value =
            Some(ignore_return!(NEVER_WITH_RET; self, expr, self.interpret_expr(expr)?));
        } else {
          self.return_value = None;
        }

        self.state_stack.pop_until(State::Function);
        Ok(())
      },
      StmtKind::Break => {
        if !self.state_stack.contains(State::Loop) {
          info!(INTR_ERR, "Resolver didn't catch break outside of loop");
        }

        self.state_stack.pop_until(State::Loop);
        Ok(())
      },
      StmtKind::Block { stmts } => self.interpret(stmts),
    }
  }

  pub(crate) fn interpret_expr(&mut self, expr: &Expr) -> Result<Literal> {
    match &expr.kind {
      ExprKind::String(s) => Ok(Literal::String(s.to_string())),
      ExprKind::Integer(n) => Ok(Literal::Integer(*n)),
      ExprKind::Identifier(name) => match self.variables.get(name) {
        Some(val) => Ok(val),
        None => info!(INTR_ERR, "Resolver didn't resolve variable getting correctly"),
      },
      ExprKind::Array(_, elems, len) => {
        let mut values = Vec::new();
        let len = len.clone().map_or_else(
          || Ok(Literal::Integer(elems.len() as i64)),
          |expr| self.interpret_expr(expr.as_ref()),
        )?;

        if let Literal::Integer(len) = len {
          for expr in elems {
            values.push(self.interpret_expr(expr)?);
          }

          for i in elems.len()..len as usize {
            let val = values[i % elems.len()].clone();
            values.push(val);
          }
        } else {
          info!(INTR_ERR, "Resolver didn't resolve array length type");
        }

        Ok(Literal::Array(values))
      },

      ExprKind::PrefixOp { op, right } => {
        let right = self.interpret_expr(right)?;
        self.interpret_prefix_op(op, right)
      },
      ExprKind::InfixOp { op, left, right } => {
        if *op == TokenKind::Equal {
          return self.interpret_assignment(left, right);
        }
        let left = self.interpret_expr(left)?;
        let right = self.interpret_expr(right)?;
        self.interpret_infix_op(op, left, right)
      },
      ExprKind::ShortCircuitOp { op, left, right } => {
        self.interpret_short_circuit_op(op, left.as_ref().clone(), right.as_ref().clone())
      },
      ExprKind::FuncCall { name, params } => {
        let (param_types, params): (Vec<_>, Vec<_>) =
          params.iter().try_fold((Vec::new(), Vec::new()), |(mut types, mut exprs), expr| {
            let expr = self.interpret_expr(expr)?;
            let ty = expr.get_type();
            types.push(ty);
            exprs.push(expr);
            Ok((types, exprs))
          })?;
        let func = match self.functions.get(&func_name(name, &param_types)) {
          Some(func) => func,
          None => info!(INTR_ERR, "Resolver didn't resolve function getting correctly"),
        };

        match func {
          Function::Native(func, arg_len) => {
            if params.len() != arg_len {
              info!(INTR_ERR, "Resolver didn't resolve the length of parameters for functions");
            }
            match func(self, params) {
              Some(value) => Ok(value),
              None if !self.ignore_return => info!(
                INTR_ERR,
                "Resolver didn't resolve the return type as void of functions correctly"
              ),
              None => Ok(Literal::Integer(1656)),
            }
          },
          Function::UserDefined(args, body, _) => {
            if params.len() != args.len() {
              info!(INTR_ERR, "Resolver didn't resolve the length of parameters for functions");
            }

            self.start_block();
            self.state_stack.push(State::Function);
            for (arg, param) in args.iter().zip(params) {
              self.variables.declare(arg, self.indent_level, param);
            }

            for stmt in body {
              if self.state_stack.last_rewind == Some(State::Function) {
                break;
              }
              self.interpret_stmt(&stmt)?;
            }

            self.state_stack.pop(State::Function);
            self.end_block();

            let return_value = self.return_value.clone();

            self.return_value = None;

            match return_value {
              Some(value) => Ok(value),
              None if self.ignore_return => Ok(Literal::Integer(1656)),
              None => info!(
                INTR_ERR,
                "Resolver didn't resolve the return type as void of functions correctly"
              ),
            }
          },
          Function::External(func, arg_len, ret_ty) => {
            if params.len() != arg_len {
              info!(INTR_ERR, "Resolver didn't resolve the length of parameters for functions");
            }

            match unsafe { utils::call_func_sym(self, func, params, ret_ty) } {
              Some(value) => Ok(value),
              None if !self.ignore_return => info!(
                INTR_ERR,
                "Resolver didn't resolve the return type as void of functions correctly"
              ),
              None => Ok(Literal::Integer(1656)),
            }
          },
        }
      },
      ExprKind::Index { array, index } => {
        let array = self.interpret_expr(array)?;
        let index = self.interpret_expr(index)?;
        self.interpret_index(array, index)
      },
    }
  }

  fn interpret_prefix_op(&self, op: &TokenKind, right: Literal) -> Result<Literal> {
    match op {
      TokenKind::Minus => Ok(-right),
      TokenKind::Bang => Ok(!right),

      _ => info!(INTR_ERR, "`{op}` is not a prefix operator"),
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
      TokenKind::LeftShift => Ok(left << right),
      TokenKind::RightShift => Ok(left >> right),
      TokenKind::Ampersand => Ok(left & right),
      TokenKind::Pipe => Ok(left | right),

      _ => info!(INTR_ERR, "`{op}` is not an infix operator"),
    }
  }

  fn interpret_short_circuit_op(
    &mut self,
    op: &TokenKind,
    left: Expr,
    right: Expr,
  ) -> Result<Literal> {
    let left = self.interpret_expr(&left)?;

    // Since the only thing that can change (at this time) when interpreting an expr
    // is variables we clone only the variables to act like a short circuit op
    let prev_variables = self.variables.clone();
    let right_lit = self.interpret_expr(&right)?;
    self.variables = prev_variables;

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

      _ => info!(INTR_ERR, "`{op}` is not a short circuit operator"),
    }
  }

  fn interpret_index(&self, array: Literal, index: Literal) -> Result<Literal> {
    match (array, index) {
      (Literal::Array(array), Literal::Integer(index)) => {
        if index < 0 || index >= array.len() as i64 {
          eprintln!("Index out of bounds.");
          process::exit(1);
        }
        Ok(array[index as usize].clone())
      },
      (Literal::String(string), Literal::Integer(index)) => {
        if index < 0 || index >= string.len() as i64 {
          eprintln!("Index out of bounds.");
          process::exit(1);
        }
        Ok(Literal::String(string[index as usize..index as usize + 1].to_string()))
      },

      _ => info!(INTR_ERR, "Resolver didn't resolve immutable indexing correctly"),
    }
  }

  fn interpret_assignment(&mut self, left: &Expr, right: &Expr) -> Result<Literal> {
    let right = self.interpret_expr(right)?;

    match &left.kind {
      ExprKind::Identifier(name) => {
        Ok(self.variables.assign_or_declare(name, self.indent_level, right))
      },
      ExprKind::Index { array, index } => {
        if let ExprKind::Identifier(name) = &array.kind {
          let index = self.interpret_expr(index)?;
          let array = match self.variables.get_mut(name) {
            Some(value) => value,
            None => info!(INTR_ERR, "Resolver didn't resolve indexing's variable correctly"),
          };

          match (array, index) {
            (Literal::Array(array), Literal::Integer(index)) => {
              if index < 0 || index >= array.len() as i64 {
                info!(ERR, "Index out of bounds.");
                process::exit(1);
              }
              *array.get_mut(index as usize).unwrap() = right.clone();
            },
            (Literal::String(string), Literal::Integer(index)) => {
              if index < 0 || index >= string.len() as i64 {
                info!(ERR, "Index out of bounds.");
                process::exit(1);
              }
              string.replace_range(
                index as usize..index as usize + right.to_string().len(),
                right.to_string().as_str(),
              );
            },

            _ => info!(INTR_ERR, "Resolver didn't resolve mutable indexing correctly"),
          }
        }

        Ok(right)
      },

      _ => info!(INTR_ERR, "Resolver didn't resolve assignment correctly"),
    }
  }
}

#[cfg(test)]
mod tests;
