use unindent::unindent;

use crate::error::{
  RuntimeError,
  RuntimeErrorKind,
};
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::runtime::ValueType;

use super::*;

#[test]
fn interpret_expr() {
  fn interpret(input: &str) -> Result<Literal> {
    let mut parser = Parser::new(input);
    let expr = parser.expression().unwrap();
    let mut resolver = Resolver::new();
    resolver.resolve_expr(&expr)?;
    let mut interpreter = Interpreter::new(&[]);
    interpreter.interpret_expr(&expr)
  }

  let result = interpret("1");
  assert_eq!(result, Ok(Literal::Integer(1)));
  let result = interpret("1 + 2");
  assert_eq!(result, Ok(Literal::Integer(3)));
  let result = interpret("1 + 2 * 3");
  assert_eq!(result, Ok(Literal::Integer(7)));
  let result = interpret("!false");
  assert_eq!(result, Ok(Literal::Integer(1)));

  let result = interpret("\"hello\"");
  assert_eq!(result, Ok(Literal::String("hello".to_string())));
  let result = interpret("\"hello\" + \"world\"");
  assert_eq!(result, Ok(Literal::String("helloworld".to_string())));
  let result = interpret("\"hello\" * 2");
  assert_eq!(result, Ok(Literal::String("hellohello".to_string())));

  let result = interpret("-\"hello\"");
  assert_eq!(
    result,
    Err(RuntimeError::new(
      RuntimeErrorKind::CannotApplyPrefix(
        Expr::new_without_span(ExprKind::String("hello".to_string())),
        TokenKind::Minus
      ),
      0..8
    ))
  );
  let result = interpret("\"hello\" - \"world\"");
  assert_eq!(
    result,
    Err(RuntimeError::new(
      RuntimeErrorKind::CannotApplyInfix(
        Expr::new_without_span(ExprKind::String("hello".to_string())),
        TokenKind::Minus,
        Expr::new_without_span(ExprKind::String("world".to_string()))
      ),
      0..17
    ))
  );

  let result = interpret("1 > 2");
  assert_eq!(result, Ok(Literal::Integer(0)));
  let result = interpret("1 < 2");
  assert_eq!(result, Ok(Literal::Integer(1)));
  let result = interpret("\"hello\" == \"world\"");
  assert_eq!(result, Ok(Literal::Integer(0)));
  let result = interpret("\"hello\" != \"world\"");
  assert_eq!(result, Ok(Literal::Integer(1)));
  let result = interpret("2 ^ 3");
  assert_eq!(result, Ok(Literal::Integer(8)));
  let result = interpret("4 % 2");
  assert_eq!(result, Ok(Literal::Integer(0)));
  let result = interpret("4 % 3");
  assert_eq!(result, Ok(Literal::Integer(1)));
  let result = interpret("4 % -3");
  assert_eq!(result, Ok(Literal::Integer(1)));
  let result = interpret("2 << 3");
  assert_eq!(result, Ok(Literal::Integer(16)));
  let result = interpret("2 >> 1");
  assert_eq!(result, Ok(Literal::Integer(1)));
  let result = interpret("2 & 3");
  assert_eq!(result, Ok(Literal::Integer(2)));
  let result = interpret("4 | 3");
  assert_eq!(result, Ok(Literal::Integer(7)));

  let result = interpret("\"hello\" > \"world\"");
  assert_eq!(
    result,
    Err(RuntimeError::new(
      RuntimeErrorKind::CannotApplyInfix(
        Expr::new_without_span(ExprKind::String("hello".to_string())),
        TokenKind::Greater,
        Expr::new_without_span(ExprKind::String("world".to_string()))
      ),
      0..17
    ))
  );
}

#[test]
fn interpret_stmt() {
  fn interpret(input: &str) -> Result<()> {
    let mut parser = Parser::new(input);
    let stmt = parser.statement();
    assert!(stmt.is_ok(), "input: {input:?}, err: {stmt}", stmt = stmt.unwrap_err().kind);
    let mut interpreter = Interpreter::new(&[]);
    interpreter.interpret_stmt(&stmt.unwrap())
  }

  let result = interpret("1\n");
  assert_eq!(result, Ok(()));

  let result = interpret(&unindent(
    "
    if true:
    \t1
    else if true:
    \t2
    else:
    \t3
    ",
  ));
  assert_eq!(result, Ok(()));

  let result = interpret(&unindent(
    "
    while false:
    \t1
    ",
  ));
  assert_eq!(result, Ok(()));
}

// #[test]
// fn interpret_print() {
//   fn interpret_and_assert(input: &str, expected: &str) {
//     let mut buf = Vec::new();

//     let mut parser = Parser::new(input);
//     let expr = parser.expression().unwrap();
//     let mut interpreter = Interpreter::new(&[]);
//     interpreter.interpret_print(&expr, &mut buf).unwrap();
//     assert_eq!(buf, expected.as_bytes());
//   }

//   interpret_and_assert("1", "1");
//   interpret_and_assert("\"hello\"", "hello");
//   interpret_and_assert("true", "1");
//   interpret_and_assert(r#""new\nline\"""#, "new\nline\"");
// }

#[test]
fn interpret_variables() {
  let mut resolver = Resolver::new();
  resolver.define_std();
  let mut interpreter = Interpreter::new(&[]);
  interpreter.define_std();

  macro_rules! interpret_expr {
    ($input:expr) => {{
      let input = $input;
      let mut parser = Parser::new(&input);
      let expr = parser.expression().unwrap();
      if let Err(e) = resolver.resolve_expr(&expr) {
        Err(e)
      } else {
        interpreter.interpret_expr(&expr)
      }
    }};
  }

  macro_rules! interpret_stmt {
    ($input:expr) => {{
      let input = $input;
      let mut parser = Parser::new(&input);
      let stmt = parser.statement().unwrap();
      if let Err(e) = resolver.resolve_stmt(&stmt) {
        Err(e)
      } else {
        interpreter.interpret_stmt(&stmt)
      }
    }};
  }

  let result = interpret_expr!("a");
  assert_eq!(
    result,
    Err(RuntimeError::new(RuntimeErrorKind::UndefinedVariable("a".to_string()), 0..1))
  );

  let result = interpret_expr!("a = 1");
  assert_eq!(result, Ok(Literal::Integer(1)));
  let result = interpret_expr!("a");
  assert_eq!(result, Ok(Literal::Integer(1)));

  resolver = Resolver::new();
  resolver.define_std();
  interpreter = Interpreter::new(&[]);
  interpreter.define_std();

  let result = interpret_stmt!(unindent(
    "
    if false:
    \ta = 1
    \tprint(a)
    else:
    \ta = 2
    \tprint(a)
    ",
  ));
  assert_eq!(result, Ok(()));
  let result = interpret_expr!("a");
  assert_eq!(
    result,
    Err(RuntimeError::new(RuntimeErrorKind::UndefinedVariable("a".to_string()), 0..1))
  );
}

#[test]
fn interpret_function() {
  let mut resolver = Resolver::new();
  resolver.define_std();
  let mut interpreter = Interpreter::new(&[]);
  interpreter.define_std();

  macro_rules! interpret_stmt {
    ($input:expr) => {{
      let input = $input;
      let mut parser = Parser::new(&input);
      let stmt = parser.statement().unwrap();
      if let Err(e) = resolver.resolve_stmt(&stmt) {
        Err(e)
      } else {
        interpreter.interpret_stmt(&stmt)
      }
    }};
  }

  let result = interpret_stmt!("func foo(x: int):\n\tprint(x + \"\n\")\n");
  assert_eq!(result, Ok(()));
  assert_eq!(resolver.functions.get("foo.int"), Some((vec![ValueType::Integer], None)));
  assert_eq!(
    interpreter.functions.get("foo.int"),
    Some(Function::UserDefined(
      vec!["x".to_string()],
      vec![Stmt::new_without_span(StmtKind::Expression {
        expr: Expr::new_without_span(ExprKind::FuncCall {
          name: "print".to_string(),
          params: vec![Expr::new_without_span(ExprKind::InfixOp {
            left: rc!(Expr::new_without_span(ExprKind::Identifier("x".to_string()))),
            op: TokenKind::Plus,
            right: rc!(Expr::new_without_span(ExprKind::String("\n".to_string())))
          })],
        }),
      })],
      None
    ))
  );

  let result = interpret_stmt!("foo(1)\n");
  assert_eq!(result, Ok(()));
}

#[test]
fn interpret_array() {
  let mut resolver = Resolver::new();
  resolver.define_std();
  let mut interpreter = Interpreter::new(&[]);
  interpreter.define_std();

  macro_rules! interpret_expr {
    ($input:expr) => {{
      let input = $input;
      let mut parser = Parser::new(&input);
      let expr = parser.expression().unwrap();
      if let Err(e) = resolver.resolve_expr(&expr) {
        Err(e)
      } else {
        interpreter.interpret_expr(&expr)
      }
    }};
  }

  let expr = interpret_expr!("int[1, 2, 3]");
  assert_eq!(
    expr,
    Ok(Literal::Array(vec![Literal::Integer(1), Literal::Integer(2), Literal::Integer(3)]))
  );

  let expr = interpret_expr!("int[1, 2, 3; 9]");
  assert_eq!(
    expr,
    Ok(Literal::Array(vec![
      Literal::Integer(1),
      Literal::Integer(2),
      Literal::Integer(3),
      Literal::Integer(1),
      Literal::Integer(2),
      Literal::Integer(3),
      Literal::Integer(1),
      Literal::Integer(2),
      Literal::Integer(3)
    ]))
  );

  let expr = interpret_expr!("string[1]");
  assert_eq!(
    expr,
    Err(RuntimeError::new(
      RuntimeErrorKind::ArrayTypeMismatch(ValueType::Integer, ValueType::String),
      7..8
    ))
  );

  let expr = interpret_expr!("int[1, 2, 3][0]");
  assert_eq!(expr, Ok(Literal::Integer(1)));

  let expr = interpret_expr!("\"abc\"[0]");
  assert_eq!(expr, Ok(Literal::String("a".to_string())));
}
