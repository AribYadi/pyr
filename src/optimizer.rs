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
    match &stmt.kind {
      StmtKind::Expression { expr } => {
        let expr = self.optimize_expr(expr);
        match &expr.kind {
          ExprKind::InfixOp { op: TokenKind::Equal, .. } | ExprKind::FuncCall { .. } => {
            Some(Stmt { kind: StmtKind::Expression { expr }, span: stmt.span.clone() })
          },
          _ if self.options.ignore_expr_stmts => None,
          _ => Some(Stmt { kind: StmtKind::Expression { expr }, span: stmt.span.clone() }),
        }
      },

      _ => Some(stmt.clone()),
    }
  }

  fn optimize_expr(&self, expr: &Expr) -> Expr {
    match &expr.kind {
      _ => expr.clone(),
    }
  }
}
