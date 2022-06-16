use unindent::unindent;

use super::*;

#[test]
fn parse_expr() {
  fn parse(input: &str) -> Result<Expr> {
    let mut parser = Parser::new(input);
    parser.expression()
  }

  let expr = parse(" 32");
  assert_eq!(expr, Ok(Expr::new_without_span(ExprKind::Integer(32))));
  let expr = parse("\"foobar\"");
  assert_eq!(expr, Ok(Expr::new_without_span(ExprKind::String("foobar".to_string()))));
  let expr = parse("foobar");
  assert_eq!(expr, Ok(Expr::new_without_span(ExprKind::Identifier("foobar".to_string()))));
  let expr = parse("true");
  assert_eq!(expr, Ok(Expr::new_without_span(ExprKind::Integer(1))));
  let expr = parse("false");
  assert_eq!(expr, Ok(Expr::new_without_span(ExprKind::Integer(0))));

  let expr = parse("-32");
  assert_eq!(
    expr,
    Ok(Expr::new_without_span(ExprKind::PrefixOp {
      op: Tok::Minus,
      right: Box::new(Expr::new_without_span(ExprKind::Integer(32)))
    }))
  );
  let expr = parse("! wrong");
  assert_eq!(
    expr,
    Ok(Expr::new_without_span(ExprKind::PrefixOp {
      op: Tok::Bang,
      right: Box::new(Expr::new_without_span(ExprKind::Identifier("wrong".to_string())))
    }))
  );

  let expr = parse("-");
  assert!(expr.is_err());
}

#[test]
fn parse_binary_expr() {
  fn parse(input: &str) -> Result<Expr> {
    let mut parser = Parser::new(input);
    parser.expression()
  }

  fn check_precedence(expr: Result<Expr>, expected: &str) {
    fn to_string(expr: Expr) -> String {
      match expr.kind {
        ExprKind::Integer(i) => i.to_string(),
        ExprKind::String(s) => format!("\"{s}\""),
        ExprKind::Identifier(s) => s,
        ExprKind::PrefixOp { op, right } => format!("({op} {})", to_string(*right)),
        ExprKind::InfixOp { left, op, right } => {
          format!("({} {op} {})", to_string(*left), to_string(*right))
        },
        ExprKind::ShortCircuitOp { op, left, right } => {
          format!("({} {op} {})", to_string(*left), to_string(*right))
        },
        ExprKind::VarAssign { name, expr } => format!("({} = {})", name, to_string(*expr)),
        ExprKind::FuncCall { name, params: args } => {
          format!(
            "({name}({args}))",
            args = args.iter().map(|arg| to_string(arg.clone())).collect::<Vec<_>>().join(", ")
          )
        },
      }
    }
    assert!(expr.is_ok());
    assert_eq!(to_string(expr.unwrap()), expected);
  }

  let expr = parse("4 + 3 * 2");
  check_precedence(expr, "(4 + (3 * 2))");

  let expr = parse("4 * 3 + 2");
  check_precedence(expr, "((4 * 3) + 2)");

  let expr = parse("4 - 3 - 2");
  check_precedence(expr, "((4 - 3) - 2)");

  let expr = parse("4 * (3 * 2)");
  check_precedence(expr, "(4 * (3 * 2))");

  let expr = parse("a = b = c");
  check_precedence(expr, "(a = (b = c))");

  let expr = parse("a and b or c");
  check_precedence(expr, "((a and b) or c)");

  let expr = parse("2 + 4 ^ 2");
  check_precedence(expr, "(2 + (4 ^ 2))");

  let expr = parse("2 + 4 % 2");
  check_precedence(expr, "(2 + (4 % 2))");

  let expr = parse("2 >> 2 << 2 & 3 | 4");
  check_precedence(expr, "((((2 >> 2) << 2) & 3) | 4)");

  let expr = parse("1 > 0 < 2");
  assert_eq!(
    expr,
    Err(ParseError::new(ParseErrorKind::InvalidChainOperator(Tok::Less, Tok::Greater), 6..7))
  );

  let expr = parse("1 == true != 0");
  assert_eq!(
    expr,
    Err(ParseError::new(
      ParseErrorKind::InvalidChainOperator(Tok::BangEqual, Tok::EqualEqual),
      10..12
    ))
  );
}

#[test]
fn parse_stmt() {
  fn parse(input: &str) -> Result<Stmt> {
    let mut parser = Parser::new(input);
    parser.statement()
  }

  let stmt = parse("1 + 2\n");
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::Expression {
      expr: Expr::new_without_span(ExprKind::InfixOp {
        left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
        op: Tok::Plus,
        right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
      })
    }))
  );

  let stmt = parse(&unindent(
    "
      if true:
      \t1 + 2
      \t3 + 4
      ",
  ));
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::If {
      condition: Expr::new_without_span(ExprKind::Integer(1)),
      body: vec![
        Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
            op: Tok::Plus,
            right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
          }),
        }),
        Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: Box::new(Expr::new_without_span(ExprKind::Integer(3))),
            op: Tok::Plus,
            right: Box::new(Expr::new_without_span(ExprKind::Integer(4))),
          }),
        }),
      ],
      else_stmt: vec![]
    }))
  );

  let stmt = parse(&unindent(
    "
      if true:
      \tif 1:
      \t\t1 + 2
      \tfoobar
      ",
  ));
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::If {
      condition: Expr::new_without_span(ExprKind::Integer(1)),
      body: vec![
        Stmt::new_without_span(StmtKind::If {
          condition: Expr::new_without_span(ExprKind::Integer(1)),
          body: vec![Stmt::new_without_span(StmtKind::Expression {
            expr: Expr::new_without_span(ExprKind::InfixOp {
              left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
              op: Tok::Plus,
              right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
            }),
          }),],
          else_stmt: vec![],
        }),
        Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::Identifier("foobar".to_string())),
        }),
      ],
      else_stmt: vec![],
    }))
  );

  let stmt = parse(&unindent(
    "

      if true:
      \tif 1:
      \t\t1 + 2
      \telse:
      \t\t3 + 4
      else if false:
      \t5 + 6
      ",
  ));
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::If {
      condition: Expr::new_without_span(ExprKind::Integer(1)),
      body: vec![Stmt::new_without_span(StmtKind::If {
        condition: Expr::new_without_span(ExprKind::Integer(1)),
        body: vec![Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
            op: Tok::Plus,
            right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
          }),
        }),],
        else_stmt: vec![Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: Box::new(Expr::new_without_span(ExprKind::Integer(3))),
            op: Tok::Plus,
            right: Box::new(Expr::new_without_span(ExprKind::Integer(4))),
          })
        })],
      }),],
      else_stmt: vec![Stmt::new_without_span(StmtKind::If {
        condition: Expr::new_without_span(ExprKind::Integer(0)),
        body: vec![Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: Box::new(Expr::new_without_span(ExprKind::Integer(5))),
            op: Tok::Plus,
            right: Box::new(Expr::new_without_span(ExprKind::Integer(6))),
          }),
        }),],
        else_stmt: vec![],
      }),],
    }))
  );

  let stmt = parse(&unindent(
    "
      while false:
      \t1 + 2
      \tif true:
      \t\t3 + 4
      \telse:
      \t\t5 + 6
      ",
  ));
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::While {
      condition: Expr::new_without_span(ExprKind::Integer(0)),
      body: vec![
        Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
            op: Tok::Plus,
            right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
          }),
        }),
        Stmt::new_without_span(StmtKind::If {
          condition: Expr::new_without_span(ExprKind::Integer(1)),
          body: vec![Stmt::new_without_span(StmtKind::Expression {
            expr: Expr::new_without_span(ExprKind::InfixOp {
              left: Box::new(Expr::new_without_span(ExprKind::Integer(3))),
              op: Tok::Plus,
              right: Box::new(Expr::new_without_span(ExprKind::Integer(4))),
            }),
          }),],
          else_stmt: vec![Stmt::new_without_span(StmtKind::Expression {
            expr: Expr::new_without_span(ExprKind::InfixOp {
              left: Box::new(Expr::new_without_span(ExprKind::Integer(5))),
              op: Tok::Plus,
              right: Box::new(Expr::new_without_span(ExprKind::Integer(6))),
            }),
          }),],
        }),
      ],
    }))
  );

  let stmt = parse("if 1: 1 + 2\n");
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::If {
      condition: Expr::new_without_span(ExprKind::Integer(1)),
      body: vec![Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
          op: Tok::Plus,
          right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
        }),
      }),],
      else_stmt: vec![],
    }))
  );
}

#[test]
fn parse_stmt_errors() {
  fn parse(input: &str) -> Result<Stmt> {
    let mut parser = Parser::new(input);
    parser.statement()
  }

  let stmt = parse("1 + 2");
  assert_eq!(
    stmt,
    Err(ParseError::new(ParseErrorKind::UnexpectedToken(Tok::Newline, Tok::Eof), 5..5))
  );
  let stmt = parse("else:\n\t1 + 2");
  assert_eq!(stmt, Err(ParseError::new(ParseErrorKind::UnmatchedElseStatement, 0..4)));
}

#[test]
fn parse_block() {
  fn parse(input: &str) -> Result<Vec<Stmt>> {
    let mut parser = Parser::new(input);
    parser.parse_block()
  }

  let block = parse(
    "\n\t1 + 2
\t3 + 4
",
  );
  assert_eq!(
    block,
    Ok(vec![
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
          op: Tok::Plus,
          right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
        }),
      }),
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: Box::new(Expr::new_without_span(ExprKind::Integer(3))),
          op: Tok::Plus,
          right: Box::new(Expr::new_without_span(ExprKind::Integer(4))),
        }),
      }),
    ])
  );

  let block = parse(
    "\n\t1 + 2
\t
\t
\t3 + 4
",
  );
  assert_eq!(
    block,
    Ok(vec![
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
          op: Tok::Plus,
          right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
        }),
      }),
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: Box::new(Expr::new_without_span(ExprKind::Integer(3))),
          op: Tok::Plus,
          right: Box::new(Expr::new_without_span(ExprKind::Integer(4))),
        }),
      }),
    ])
  );

  let block = parse(
    "\n\t1

\t3",
  );
  assert_ne!(
    block,
    Ok(vec![
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: Box::new(Expr::new_without_span(ExprKind::Integer(1))),
          op: Tok::Plus,
          right: Box::new(Expr::new_without_span(ExprKind::Integer(2))),
        }),
      }),
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: Box::new(Expr::new_without_span(ExprKind::Integer(3))),
          op: Tok::Plus,
          right: Box::new(Expr::new_without_span(ExprKind::Integer(4))),
        }),
      }),
    ])
  );
}

#[test]
fn parse_function() {
  fn parse_expr(input: &str) -> Result<Expr> {
    let mut parser = Parser::new(input);
    parser.expression()
  }
  fn parse_stmt(input: &str) -> Result<Stmt> {
    let mut parser = Parser::new(input);
    parser.statement()
  }

  let stmt = parse_stmt("func foo():\n\tprint(1)\n");
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::FuncDef {
      name: "foo".to_string(),
      args: vec![],
      body: vec![Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::FuncCall {
          name: "print".to_string(),
          params: vec![Expr::new_without_span(ExprKind::Integer(1))],
        }),
      })],
      ret_ty: None,
    }))
  );
  let stmt = parse_stmt("func foo(a: int, b: string):\n\tprint(a)\n");
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::FuncDef {
      name: "foo".to_string(),
      args: vec![("a".to_string(), ValueType::Integer), ("b".to_string(), ValueType::String)],
      body: vec![Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::FuncCall {
          name: "print".to_string(),
          params: vec![Expr::new_without_span(ExprKind::Identifier("a".to_string()))],
        }),
      })],
      ret_ty: None,
    }))
  );

  let expr = parse_expr("foo(123, \"abc\")");
  assert_eq!(
    expr,
    Ok(Expr::new_without_span(ExprKind::FuncCall {
      name: "foo".to_string(),
      params: vec![
        Expr::new_without_span(ExprKind::Integer(123)),
        Expr::new_without_span(ExprKind::String("abc".to_string())),
      ],
    }))
  );

  let stmt = parse_stmt("func foo(a: int, b: string) -> int:\n\tret a + b\n");
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::FuncDef {
      name: "foo".to_string(),
      args: vec![("a".to_string(), ValueType::Integer), ("b".to_string(), ValueType::String)],
      body: vec![Stmt::new_without_span(StmtKind::Ret {
        expr: Some(Expr::new_without_span(ExprKind::InfixOp {
          left: Box::new(Expr::new_without_span(ExprKind::Identifier("a".to_string()))),
          op: Tok::Plus,
          right: Box::new(Expr::new_without_span(ExprKind::Identifier("b".to_string()))),
        }))
      })],
      ret_ty: Some(ValueType::Integer),
    }))
  );
}
