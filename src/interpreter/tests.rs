use unindent::unindent;

use crate::error::{
  RuntimeError,
  RuntimeErrorKind,
};
use crate::parser::Parser;
use crate::resolver::Resolver;

use super::*;

#[test]
fn interpret_expr() {
  fn interpret(input: &str) -> Result<Literal> {
    let mut parser = Parser::new(input);
    let expr = parser.expression().unwrap();
    let resolver = Resolver::new();
    resolver.resolve_expr(&expr)?;
    let interpreter = Interpreter::new(input);
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
}

#[test]
fn interpret_stmt() {
  fn interpret(input: &str) -> Result<()> {
    let mut parser = Parser::new(input);
    let stmt = parser.statement().unwrap();
    let interpreter = Interpreter::new(input);
    interpreter.interpret_stmt(&stmt)
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

#[test]
fn interpret_print() {
  fn interpret_and_assert(input: &str, expected: &str) {
    let mut buf = Vec::new();

    let mut parser = Parser::new(input);
    let expr = parser.expression().unwrap();
    let interpreter = Interpreter::new(input);
    interpreter.interpret_print(&expr, &mut buf).unwrap();
    assert_eq!(buf, expected.as_bytes());
  }

  interpret_and_assert("1", "1");
  interpret_and_assert("\"hello\"", "hello");
  interpret_and_assert("true", "1");
}
