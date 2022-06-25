use rayon::prelude::*;

use crate::parser::syntax::{
  Expr,
  ExprKind,
  Stmt,
  StmtKind,
  TokenKind,
};

pub struct OptimizerOptions {
  // ignore expression statements
  pub ignore_expr_stmts: bool,
  // pre-calculates arithmetic and equality operators for constants
  pub precalc_constant_ops: bool,
}

pub struct Optimizer {
  pub options: OptimizerOptions,
}

impl Optimizer {
  pub fn new(options: OptimizerOptions) -> Self { Self { options } }

  pub fn optimize(&self, stmts: &[Stmt]) -> Vec<Stmt> { self.optimize_stmts(stmts) }

  fn optimize_stmts(&self, stmts: &[Stmt]) -> Vec<Stmt> {
    stmts.par_iter().filter_map(|stmt| self.optimize_stmt(stmt)).collect()
  }

  fn optimize_stmt(&self, stmt: &Stmt) -> Option<Stmt> {
    Some(Stmt {
      kind: match &stmt.kind {
        StmtKind::Expression { expr } => {
          let expr = self.optimize_expr(expr);
          match &expr.kind {
            ExprKind::InfixOp { op: TokenKind::Equal, .. } | ExprKind::FuncCall { .. } => {
              StmtKind::Expression { expr }
            },
            _ if self.options.ignore_expr_stmts => return None,
            _ => StmtKind::Expression { expr },
          }
        },
        StmtKind::If { condition, body, else_stmt } => {
          let condition = self.optimize_expr(condition);
          let body = self.optimize_stmts(body);
          let else_stmt = self.optimize_stmts(else_stmt);

          StmtKind::If { condition, body, else_stmt }
        },
        StmtKind::While { condition, body } => {
          let condition = self.optimize_expr(condition);
          let body = self.optimize_stmts(body);

          StmtKind::While { condition, body }
        },
        StmtKind::FuncDef { name, args, body, ret_ty } => {
          let body = self.optimize_stmts(body);

          StmtKind::FuncDef {
            name: name.to_string(),
            args: args.clone(),
            body,
            ret_ty: ret_ty.clone(),
          }
        },
        StmtKind::Ret { expr } => {
          let expr = expr.clone().map(|expr| self.optimize_expr(&expr));

          StmtKind::Ret { expr }
        },

        _ => stmt.kind.clone(),
      },
      span: stmt.span.clone(),
    })
  }

  fn optimize_expr(&self, expr: &Expr) -> Expr {
    Expr {
      kind: match &expr.kind {
        ExprKind::Array(ty, elems, len) => {
          let mut opt_elems = Vec::new();
          elems
            .into_par_iter()
            .map(|expr| self.optimize_expr(expr))
            .collect_into_vec(&mut opt_elems);

          ExprKind::Array(ty.clone(), opt_elems, *len)
        },

        ExprKind::PrefixOp { op, right } => {
          let right = self.optimize_expr(right);

          if self.options.precalc_constant_ops {
            self.optimize_prefix(op, right)
          } else {
            ExprKind::PrefixOp { op: *op, right: bx!(right) }
          }
        },
        ExprKind::InfixOp { op, left, right } | ExprKind::ShortCircuitOp { op, left, right } => {
          let left = self.optimize_expr(left);
          let right = self.optimize_expr(right);

          match &expr.kind {
            ExprKind::InfixOp { .. } if self.options.precalc_constant_ops => {
              self.optimize_infix(op, left, right)
            },
            ExprKind::ShortCircuitOp { .. } if self.options.precalc_constant_ops => todo!(),
            ExprKind::InfixOp { .. } => {
              ExprKind::InfixOp { op: *op, left: bx!(left), right: bx!(right) }
            },
            ExprKind::ShortCircuitOp { .. } => {
              ExprKind::ShortCircuitOp { op: *op, left: bx!(left), right: bx!(right) }
            },

            _ => unreachable!(),
          }
        },
        ExprKind::FuncCall { name, params } => {
          let mut opt_params = Vec::new();
          params
            .into_par_iter()
            .map(|expr| self.optimize_expr(expr))
            .collect_into_vec(&mut opt_params);

          ExprKind::FuncCall { name: name.to_string(), params: opt_params }
        },
        ExprKind::Index { array, index } => {
          let array = self.optimize_expr(array);
          let index = self.optimize_expr(index);

          ExprKind::Index { array: bx!(array), index: bx!(index) }
        },

        _ => expr.kind.clone(),
      },
      span: expr.span.clone(),
    }
  }

  fn optimize_prefix(&self, op: &TokenKind, right: Expr) -> ExprKind {
    match (op, &right.kind) {
      (TokenKind::Minus, ExprKind::Integer(r)) => ExprKind::Integer(-r),
      (TokenKind::Bang, _) => match self.is_truthy(&right) {
        Some(is_truthy) => ExprKind::Integer(is_truthy as i64),
        None => ExprKind::PrefixOp { op: *op, right: Box::new(right) },
      },

      _ => ExprKind::PrefixOp { op: *op, right: Box::new(right) },
    }
  }

  fn optimize_infix(&self, op: &TokenKind, left: Expr, right: Expr) -> ExprKind {
    match (op, &left.kind, &right.kind) {
      (TokenKind::Plus, ExprKind::Integer(l), ExprKind::Integer(r)) => ExprKind::Integer(l + r),
      (TokenKind::Plus, ExprKind::String(s), kind) if self.is_const(kind) => {
        let string = [s.to_string(), kind.to_string()].concat();
        ExprKind::String(string)
      },
      (TokenKind::Plus, kind, ExprKind::String(s)) if self.is_const(kind) => {
        let string = [kind.to_string(), s.to_string()].concat();
        ExprKind::String(string)
      },

      (TokenKind::Minus, ExprKind::Integer(l), ExprKind::Integer(r)) => ExprKind::Integer(l - r),

      (TokenKind::Star, ExprKind::Integer(l), ExprKind::Integer(r)) => ExprKind::Integer(l * r),
      (TokenKind::Star, ExprKind::String(s), ExprKind::Integer(n)) |
      (TokenKind::Star, ExprKind::Integer(n), ExprKind::String(s)) => {
        ExprKind::String(s.repeat(*n as usize))
      },

      (TokenKind::Slash, ExprKind::Integer(l), ExprKind::Integer(r)) => ExprKind::Integer(l / r),

      (TokenKind::Less, ExprKind::Integer(l), ExprKind::Integer(r)) => {
        ExprKind::Integer((l < r) as i64)
      },
      (TokenKind::LessEqual, ExprKind::Integer(l), ExprKind::Integer(r)) => {
        ExprKind::Integer((l <= r) as i64)
      },
      (TokenKind::Greater, ExprKind::Integer(l), ExprKind::Integer(r)) => {
        ExprKind::Integer((l > r) as i64)
      },
      (TokenKind::GreaterEqual, ExprKind::Integer(l), ExprKind::Integer(r)) => {
        ExprKind::Integer((l >= r) as i64)
      },

      (TokenKind::Equal, kind1, kind2) if self.is_const(kind1) && self.is_const(kind2) => {
        ExprKind::Integer(!self.is_equal(kind1, kind2).unwrap() as i64)
      },
      (TokenKind::BangEqual, kind1, kind2) if self.is_const(kind1) && self.is_const(kind2) => {
        ExprKind::Integer(self.is_equal(kind1, kind2).unwrap() as i64)
      },

      (TokenKind::Caret, ExprKind::Integer(l), ExprKind::Integer(r)) => {
        ExprKind::Integer(f64::powi(*l as f64, *r as i32) as i64)
      },

      (TokenKind::Percent, ExprKind::Integer(l), ExprKind::Integer(r)) => ExprKind::Integer(l % r),

      (TokenKind::LeftShift, ExprKind::Integer(l), ExprKind::Integer(r)) => {
        ExprKind::Integer(l << r)
      },
      (TokenKind::RightShift, ExprKind::Integer(l), ExprKind::Integer(r)) => {
        ExprKind::Integer(l >> r)
      },
      (TokenKind::Ampersand, ExprKind::Integer(l), ExprKind::Integer(r)) => {
        ExprKind::Integer(l & r)
      },
      (TokenKind::Pipe, ExprKind::Integer(l), ExprKind::Integer(r)) => ExprKind::Integer(l | r),

      _ => ExprKind::InfixOp { op: *op, left: Box::new(left), right: Box::new(right) },
    }
  }

  fn is_truthy(&self, expr: &Expr) -> Option<bool> {
    Some(match &expr.kind {
      ExprKind::Integer(n) => *n == 1,
      ExprKind::String(s) => !s.is_empty(),
      ExprKind::Array(_, _, len) => *len != 0,

      _ => return None,
    })
  }

  fn is_const(&self, kind: &ExprKind) -> bool {
    matches!(kind, ExprKind::Integer(_) | ExprKind::String(_) | ExprKind::Array(_, _, _))
  }

  fn is_equal(&self, kind1: &ExprKind, kind2: &ExprKind) -> Option<bool> {
    Some(match (&kind1, &kind2) {
      (ExprKind::Integer(l), ExprKind::Integer(r)) => l == r,
      (ExprKind::String(l), ExprKind::String(r)) => l == r,
      (ExprKind::Array(lty, lelems, llen), ExprKind::Array(rty, relems, rlen)) => {
        lty == rty && llen == rlen && lelems == relems
      },

      _ => return None,
    })
  }
}
