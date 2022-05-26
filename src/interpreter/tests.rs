use crate::error::{
  InterpretError,
  InterpretErrorKind,
};
use crate::parser::Parser;

use super::*;

#[test]
fn interpret_expr() {
  fn interpret(input: &str) -> Result<Literal> {
    let mut parser = Parser::new(input);
    let expr = parser.expression().unwrap();
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
    Err(InterpretError::new_with_span(
      InterpretErrorKind::CannotApplyPrefix(Literal::String("hello".to_string()), TokenKind::Minus),
      0..8
    ))
  );
  let result = interpret("\"hello\" - \"world\"");
  assert_eq!(
    result,
    Err(InterpretError::new_with_span(
      InterpretErrorKind::CannotApplyInfix(
        Literal::String("hello".to_string()),
        TokenKind::Minus,
        Literal::String("world".to_string())
      ),
      0..17
    ))
  );
  let result = interpret("!256");
  assert_eq!(
    result,
    Err(InterpretError::new_with_span(
      InterpretErrorKind::CannotApplyPrefix(Literal::Integer(256), TokenKind::Bang),
      0..4
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
  assert!(result.is_ok());
}
