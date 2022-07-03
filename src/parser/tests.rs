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
      right: rc!(Expr::new_without_span(ExprKind::Integer(32)))
    }))
  );
  let expr = parse("! wrong");
  assert_eq!(
    expr,
    Ok(Expr::new_without_span(ExprKind::PrefixOp {
      op: Tok::Bang,
      right: rc!(Expr::new_without_span(ExprKind::Identifier("wrong".to_string())))
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
        ExprKind::Array(ty, exprs, len) => {
          format!(
            "{ty}[{exprs}{len}]",
            exprs = exprs.into_iter().map(to_string).collect::<Vec<_>>().join(", "),
            len = if let Some(len) = len { format!("; {len}") } else { "".to_string() }
          )
        },
        ExprKind::PrefixOp { op, right } => format!("({op} {})", to_string(right.as_ref().clone())),
        ExprKind::InfixOp { left, op, right } => {
          format!(
            "({} {op} {})",
            to_string(left.as_ref().clone()),
            to_string(right.as_ref().clone())
          )
        },
        ExprKind::ShortCircuitOp { op, left, right } => {
          format!(
            "({} {op} {})",
            to_string(left.as_ref().clone()),
            to_string(right.as_ref().clone())
          )
        },
        ExprKind::FuncCall { name, params } => {
          format!(
            "({name}({params}))",
            params = params.into_iter().map(to_string).collect::<Vec<_>>().join(", ")
          )
        },
        ExprKind::Index { array, index } => {
          format!(
            "({array}[{index}])",
            array = to_string(array.as_ref().clone()),
            index = to_string(index.as_ref().clone())
          )
        },
      }
    }

    assert!(expr.is_ok(), "{err:?}", err = expr.unwrap_err());
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

  let expr = parse("2 ^ 2 ^ 2");
  check_precedence(expr, "(2 ^ (2 ^ 2))");

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
        left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
        op: Tok::Plus,
        right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
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
            left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
            op: Tok::Plus,
            right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
          }),
        }),
        Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: rc!(Expr::new_without_span(ExprKind::Integer(3))),
            op: Tok::Plus,
            right: rc!(Expr::new_without_span(ExprKind::Integer(4))),
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
              left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
              op: Tok::Plus,
              right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
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
            left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
            op: Tok::Plus,
            right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
          }),
        }),],
        else_stmt: vec![Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: rc!(Expr::new_without_span(ExprKind::Integer(3))),
            op: Tok::Plus,
            right: rc!(Expr::new_without_span(ExprKind::Integer(4))),
          })
        })],
      }),],
      else_stmt: vec![Stmt::new_without_span(StmtKind::If {
        condition: Expr::new_without_span(ExprKind::Integer(0)),
        body: vec![Stmt::new_without_span(StmtKind::Expression {
          expr: Expr::new_without_span(ExprKind::InfixOp {
            left: rc!(Expr::new_without_span(ExprKind::Integer(5))),
            op: Tok::Plus,
            right: rc!(Expr::new_without_span(ExprKind::Integer(6))),
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
            left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
            op: Tok::Plus,
            right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
          }),
        }),
        Stmt::new_without_span(StmtKind::If {
          condition: Expr::new_without_span(ExprKind::Integer(1)),
          body: vec![Stmt::new_without_span(StmtKind::Expression {
            expr: Expr::new_without_span(ExprKind::InfixOp {
              left: rc!(Expr::new_without_span(ExprKind::Integer(3))),
              op: Tok::Plus,
              right: rc!(Expr::new_without_span(ExprKind::Integer(4))),
            }),
          }),],
          else_stmt: vec![Stmt::new_without_span(StmtKind::Expression {
            expr: Expr::new_without_span(ExprKind::InfixOp {
              left: rc!(Expr::new_without_span(ExprKind::Integer(5))),
              op: Tok::Plus,
              right: rc!(Expr::new_without_span(ExprKind::Integer(6))),
            }),
          }),],
        }),
      ],
    }))
  );

  let stmt = parse(&unindent(
    "
    while true:
    \tbreak
    ",
  ));
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::While {
      condition: Expr::new_without_span(ExprKind::Integer(1)),
      body: vec![Stmt::new_without_span(StmtKind::Break),],
    }))
  );

  let stmt = parse("if 1: 1 + 2\n");
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::If {
      condition: Expr::new_without_span(ExprKind::Integer(1)),
      body: vec![Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
          op: Tok::Plus,
          right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
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
    parser.parse_block_or_line()
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
          left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
          op: Tok::Plus,
          right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
        }),
      }),
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: rc!(Expr::new_without_span(ExprKind::Integer(3))),
          op: Tok::Plus,
          right: rc!(Expr::new_without_span(ExprKind::Integer(4))),
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
          left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
          op: Tok::Plus,
          right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
        }),
      }),
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: rc!(Expr::new_without_span(ExprKind::Integer(3))),
          op: Tok::Plus,
          right: rc!(Expr::new_without_span(ExprKind::Integer(4))),
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
          left: rc!(Expr::new_without_span(ExprKind::Integer(1))),
          op: Tok::Plus,
          right: rc!(Expr::new_without_span(ExprKind::Integer(2))),
        }),
      }),
      Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::InfixOp {
          left: rc!(Expr::new_without_span(ExprKind::Integer(3))),
          op: Tok::Plus,
          right: rc!(Expr::new_without_span(ExprKind::Integer(4))),
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
          left: rc!(Expr::new_without_span(ExprKind::Identifier("a".to_string()))),
          op: Tok::Plus,
          right: rc!(Expr::new_without_span(ExprKind::Identifier("b".to_string()))),
        }))
      })],
      ret_ty: Some(ValueType::Integer),
    }))
  );

  let stmt = parse_stmt("extern func add(a: int, b: int) -> int\n");
  assert_eq!(
    stmt,
    Ok(Stmt::new_without_span(StmtKind::FuncExtern {
      name: "add".to_string(),
      args: vec![("a".to_string(), ValueType::Integer), ("b".to_string(), ValueType::Integer)],
      ret_ty: Some(ValueType::Integer),
    }))
  );

  let stmt = parse_stmt("func many_args(arg0: int, arg1: int, arg2: int, arg3: int, arg4: int, arg5: int, arg6: int, arg7: int, arg8: int, arg9: int, arg10: int, arg11: int, arg12: int, arg13: int, arg14: int, arg15: int, arg16: int, arg17: int, arg18: int, arg19: int, arg20: int, arg21: int, arg22: int, arg23: int, arg24: int, arg25: int, arg26: int, arg27: int, arg28: int, arg29: int, arg30: int, arg31: int, arg32: int, arg33: int, arg34: int, arg35: int, arg36: int, arg37: int, arg38: int, arg39: int, arg40: int, arg41: int, arg42: int, arg43: int, arg44: int, arg45: int, arg46: int, arg47: int, arg48: int, arg49: int, arg50: int, arg51: int, arg52: int, arg53: int, arg54: int, arg55: int, arg56: int, arg57: int, arg58: int, arg59: int, arg60: int, arg61: int, arg62: int, arg63: int, arg64: int, arg65: int, arg66: int, arg67: int, arg68: int, arg69: int, arg70: int, arg71: int, arg72: int, arg73: int, arg74: int, arg75: int, arg76: int, arg77: int, arg78: int, arg79: int, arg80: int, arg81: int, arg82: int, arg83: int, arg84: int, arg85: int, arg86: int, arg87: int, arg88: int, arg89: int, arg90: int, arg91: int, arg92: int, arg93: int, arg94: int, arg95: int, arg96: int, arg97: int, arg98: int, arg99: int, arg100: int, arg101: int, arg102: int, arg103: int, arg104: int, arg105: int, arg106: int, arg107: int, arg108: int, arg109: int, arg110: int, arg111: int, arg112: int, arg113: int, arg114: int, arg115: int, arg116: int, arg117: int, arg118: int, arg119: int, arg120: int, arg121: int, arg122: int, arg123: int, arg124: int, arg125: int, arg126: int, arg127: int, arg128: int, arg129: int, arg130: int, arg131: int, arg132: int, arg133: int, arg134: int, arg135: int, arg136: int, arg137: int, arg138: int, arg139: int, arg140: int, arg141: int, arg142: int, arg143: int, arg144: int, arg145: int, arg146: int, arg147: int, arg148: int, arg149: int, arg150: int, arg151: int, arg152: int, arg153: int, arg154: int, arg155: int, arg156: int, arg157: int, arg158: int, arg159: int, arg160: int, arg161: int, arg162: int, arg163: int, arg164: int, arg165: int, arg166: int, arg167: int, arg168: int, arg169: int, arg170: int, arg171: int, arg172: int, arg173: int, arg174: int, arg175: int, arg176: int, arg177: int, arg178: int, arg179: int, arg180: int, arg181: int, arg182: int, arg183: int, arg184: int, arg185: int, arg186: int, arg187: int, arg188: int, arg189: int, arg190: int, arg191: int, arg192: int, arg193: int, arg194: int, arg195: int, arg196: int, arg197: int, arg198: int, arg199: int, arg200: int, arg201: int, arg202: int, arg203: int, arg204: int, arg205: int, arg206: int, arg207: int, arg208: int, arg209: int, arg210: int, arg211: int, arg212: int, arg213: int, arg214: int, arg215: int, arg216: int, arg217: int, arg218: int, arg219: int, arg220: int, arg221: int, arg222: int, arg223: int, arg224: int, arg225: int, arg226: int, arg227: int, arg228: int, arg229: int, arg230: int, arg231: int, arg232: int, arg233: int, arg234: int, arg235: int, arg236: int, arg237: int, arg238: int, arg239: int, arg240: int, arg241: int, arg242: int, arg243: int, arg244: int, arg245: int, arg246: int, arg247: int, arg248: int, arg249: int, arg250: int, arg251: int, arg252: int, arg253: int, arg254: int, arg255: int): print(\"There is too many arguments\")");
  assert_eq!(
    stmt,
    Err(ParseError::new(
      ParseErrorKind::DeclWithTooManyArgs("many_args".to_string(), 256),
      14..3233
    ))
  );

  let stmt = parse_stmt("many_params(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255)");
  assert_eq!(
    stmt,
    Err(ParseError::new(
      ParseErrorKind::CallWithTooManyArgs("many_params".to_string(), 256),
      11..1181
    ))
  );
}
