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
    stmts.iter().filter_map(|stmt| self.optimize_stmt(stmt)).collect()
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
          let opt_elems = elems.iter().map(|expr| self.optimize_expr(expr)).collect();
          let len = len.as_ref().map(|expr| rc!(self.optimize_expr(expr.as_ref())));

          ExprKind::Array(ty.clone(), opt_elems, len)
        },

        ExprKind::PrefixOp { op, right } => {
          let right = self.optimize_expr(right);

          if self.options.precalc_constant_ops {
            self.optimize_prefix(op, right)
          } else {
            ExprKind::PrefixOp { op: *op, right: rc!(right) }
          }
        },
        ExprKind::InfixOp { op, left, right } | ExprKind::ShortCircuitOp { op, left, right } => {
          let left = self.optimize_expr(left);
          let right = self.optimize_expr(right);

          match &expr.kind {
            ExprKind::InfixOp { .. } if self.options.precalc_constant_ops => {
              self.optimize_infix(op, left, right)
            },
            ExprKind::ShortCircuitOp { .. } if self.options.precalc_constant_ops => {
              self.optimize_short_circuit(op, left, right)
            },
            ExprKind::InfixOp { .. } => {
              ExprKind::InfixOp { op: *op, left: rc!(left), right: rc!(right) }
            },
            ExprKind::ShortCircuitOp { .. } => {
              ExprKind::ShortCircuitOp { op: *op, left: rc!(left), right: rc!(right) }
            },

            _ => unreachable!(),
          }
        },
        ExprKind::FuncCall { name, params } => {
          let opt_params = params.iter().map(|expr| self.optimize_expr(expr)).collect();

          ExprKind::FuncCall { name: name.to_string(), params: opt_params }
        },
        ExprKind::Index { array, index } => {
          let array = self.optimize_expr(array);
          let index = self.optimize_expr(index);

          ExprKind::Index { array: rc!(array), index: rc!(index) }
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
        None => ExprKind::PrefixOp { op: *op, right: rc!(right) },
      },

      _ => ExprKind::PrefixOp { op: *op, right: rc!(right) },
    }
  }

  fn optimize_infix(&self, op: &TokenKind, left: Expr, right: Expr) -> ExprKind {
    match (op, &left.kind, &right.kind) {
      (TokenKind::Plus, ExprKind::Integer(l), ExprKind::Integer(r)) => ExprKind::Integer(l + r),
      (TokenKind::Plus, ExprKind::String(s), kind) if self.is_const(kind) => {
        let kind_as_str = kind.to_string();
        let kind_as_str = if kind_as_str.starts_with('"') {
          kind_as_str[1..kind_as_str.len() - 1].to_string()
        } else {
          kind_as_str
        };
        let kind_as_str = if kind_as_str.contains('\\') {
          crate::utils::unescape(kind_as_str).unwrap()
        } else {
          kind_as_str
        };

        let string = [s.to_string(), kind_as_str].concat();
        ExprKind::String(string)
      },
      (TokenKind::Plus, kind, ExprKind::String(s)) if self.is_const(kind) => {
        let kind_as_str = kind.to_string();
        let kind_as_str = if kind_as_str.starts_with('"') {
          kind_as_str[1..kind_as_str.len() - 1].to_string()
        } else {
          kind_as_str
        };
        let kind_as_str = if kind_as_str.contains('\\') {
          crate::utils::unescape(kind_as_str).unwrap()
        } else {
          kind_as_str
        };

        let string = [kind_as_str, s.to_string()].concat();
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
        ExprKind::Integer(self.is_equal(kind1, kind2).unwrap() as i64)
      },
      (TokenKind::BangEqual, kind1, kind2) if self.is_const(kind1) && self.is_const(kind2) => {
        ExprKind::Integer(!self.is_equal(kind1, kind2).unwrap() as i64)
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

      _ => ExprKind::InfixOp { op: *op, left: rc!(left), right: rc!(right) },
    }
  }

  fn optimize_short_circuit(&self, op: &TokenKind, left: Expr, right: Expr) -> ExprKind {
    if !self.is_const(&left.kind) || !self.is_const(&right.kind) {
      return ExprKind::ShortCircuitOp { op: *op, left: rc!(left), right: rc!(right) };
    }

    match op {
      TokenKind::And if self.is_const_same_variant(&left.kind, &right.kind) => {
        if !self.is_truthy(&left).unwrap() {
          return left.kind;
        }
        right.kind
      },
      TokenKind::And if self.is_truthy(&left).unwrap() => {
        ExprKind::Integer(self.is_truthy(&right).unwrap() as i64)
      },
      TokenKind::And => ExprKind::Integer(0),
      TokenKind::Or if self.is_const_same_variant(&left.kind, &right.kind) => {
        if self.is_truthy(&left).unwrap() {
          return left.kind;
        }
        right.kind
      },
      TokenKind::Or if self.is_truthy(&left).unwrap() => ExprKind::Integer(1),
      TokenKind::Or => ExprKind::Integer(self.is_truthy(&right).unwrap() as i64),

      _ => unreachable!(),
    }
  }

  fn is_truthy(&self, expr: &Expr) -> Option<bool> {
    Some(match &expr.kind {
      ExprKind::Integer(n) => *n == 1,
      ExprKind::String(s) => !s.is_empty(),
      ExprKind::Array(_, elems, len) => {
        !elems.is_empty() &&
          !matches!(
            len.as_ref().map(|expr| expr.as_ref().clone()),
            Some(Expr { kind: ExprKind::Integer(0), .. })
          )
      },

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

  fn is_const_same_variant(&self, kind1: &ExprKind, kind2: &ExprKind) -> bool {
    use ExprKind::*;

    matches!(
      (kind1, kind2),
      (Integer(_), Integer(_)) | (String(_), String(_)) | (Array(_, _, _), Array(_, _, _))
    )
  }
}
