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

          ExprKind::PrefixOp { op: *op, right: bx!(right) }
        },
        ExprKind::InfixOp { op, left, right } | ExprKind::ShortCircuitOp { op, left, right } => {
          let left = self.optimize_expr(left);
          let right = self.optimize_expr(right);

          match &expr.kind {
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
}
