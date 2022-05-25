use logos::{
  Lexer,
  Logos,
};

use crate::error::{
  ParseError,
  ParseErrorKind,
  ParseResult as Result,
};

use self::syntax::{
  Expr,
  Operator,
  Stmt,
  TokenKind as Tok,
};

pub mod syntax;

#[allow(dead_code)]
pub struct Parser<'a> {
  lexer: Lexer<'a, Tok>,
  peek: Tok,
  curr: Tok,
}

#[allow(dead_code)]
impl Parser<'_> {
  pub fn new(input: &str) -> Parser {
    let mut lexer = Tok::lexer(input);
    Parser { peek: lexer.next().unwrap_or(Tok::Eof), curr: Tok::Eof, lexer }
  }

  fn lexeme(&self) -> String { self.lexer.slice().to_string() }

  fn next(&mut self) -> Tok {
    self.curr = self.peek;
    self.peek = self.lexer.next().unwrap_or(Tok::Eof);
    self.curr
  }

  fn consume(&mut self, expected: Tok) -> Result<()> {
    if self.peek == expected {
      self.next();
      Ok(())
    } else {
      Err(ParseError::new(ParseErrorKind::UnexpectedToken(expected, self.peek), self.lexer.span()))
    }
  }

  fn consume_delimiter(&mut self, expected: Tok) -> Result<()> {
    if self.peek == expected {
      self.next();
      Ok(())
    } else {
      Err(ParseError::new(ParseErrorKind::UnclosedDelimiter(expected), self.lexer.span()))
    }
  }

  pub fn parse(&mut self) -> Vec<Stmt> {
    let mut errors = Vec::new();
    while self.peek != Tok::Eof {
      if self.curr != Tok::Newline {
        todo!("Synchronize the parser");
      }
      match self.statement() {
        Ok(_) => todo!("Do something with the stmt"),
        Err(err) => errors.push(err),
      };
    }
    todo!()
  }

  pub(super) fn statement(&mut self) -> Result<Stmt> {
    match self.peek {
      Tok::Newline => {
        self.consume(Tok::Newline)?;
        self.statement()
      },
      _ => {
        let expr = self.expression()?;
        self.consume(Tok::Newline)?;
        Ok(Stmt::Expression { expr })
      },
    }
  }

  pub(super) fn expression(&mut self) -> Result<Expr> { self.parse_expr(0) }

  fn parse_expr(&mut self, bp: u8) -> Result<Expr> {
    let mut lhs = match self.peek {
      literal @ Tok::String
      | literal @ Tok::Integer
      | literal @ Tok::Identifier
      | literal @ Tok::True
      | literal @ Tok::False => {
        let text = self.lexeme();
        self.consume(literal)?;
        match literal {
          Tok::String => Ok(Expr::String(text[1..text.len() - 1].to_string())),
          Tok::Integer => Ok(Expr::Integer(text.parse().unwrap())),
          Tok::Identifier => Ok(Expr::Identifier(text)),
          Tok::True => Ok(Expr::Integer(1)),
          Tok::False => Ok(Expr::Integer(0)),
          _ => unreachable!(),
        }
      },
      Tok::LeftParen => {
        self.consume(Tok::LeftParen)?;
        let expr = self.parse_expr(0)?;
        self.consume_delimiter(Tok::RightParen)?;
        Ok(expr)
      },
      op @ Tok::Minus | op @ Tok::Bang => {
        self.consume(op)?;
        let ((), rbp) = op.prefix_bp();
        let right = self.parse_expr(rbp)?;
        Ok(Expr::PrefixOp { op, right: Box::new(right) })
      },
      tok => Err(ParseError::new(ParseErrorKind::ExpectedExpressionStart(tok), self.lexer.span())),
    }?;
    while self.peek != Tok::Eof {
      match self.peek {
        op @ Tok::Plus | op @ Tok::Minus | op @ Tok::Star | op @ Tok::Slash => {
          let (lbp, rbp) = op.infix_bp();
          if lbp < bp {
            break;
          }

          self.consume(op)?;
          let right = self.parse_expr(rbp)?;
          lhs = Expr::InfixOp { left: Box::new(lhs), op, right: Box::new(right) };
        },
        Tok::Newline | Tok::RightParen => break,
        tok => {
          return Err(ParseError::new(ParseErrorKind::UnknownInfixOperator(tok), self.lexer.span()))
        },
      }
    }

    Ok(lhs)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_expr() {
    fn parse(input: &str) -> Result<Expr> {
      let mut parser = Parser::new(input);
      parser.expression()
    }

    let expr = parse("  32");
    assert_eq!(expr, Ok(Expr::Integer(32)));
    let expr = parse("\"foobar\"");
    assert_eq!(expr, Ok(Expr::String("foobar".to_string())));
    let expr = parse("foobar");
    assert_eq!(expr, Ok(Expr::Identifier("foobar".to_string())));
    let expr = parse("true");
    assert_eq!(expr, Ok(Expr::Integer(1)));
    let expr = parse("false");
    assert_eq!(expr, Ok(Expr::Integer(0)));

    let expr = parse("-32");
    assert_eq!(expr, Ok(Expr::PrefixOp { op: Tok::Minus, right: Box::new(Expr::Integer(32)) }));
    let expr = parse("!   wrong");
    assert_eq!(
      expr,
      Ok(Expr::PrefixOp { op: Tok::Bang, right: Box::new(Expr::Identifier("wrong".to_string())) })
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
        match expr {
          Expr::Integer(i) => i.to_string(),
          Expr::String(s) => format!("\"{s}\""),
          Expr::Identifier(s) => s,
          Expr::PrefixOp { op, right } => format!("({op} {})", to_string(*right)),
          Expr::InfixOp { left, op, right } => {
            format!("({} {op} {})", to_string(*left), to_string(*right))
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
      Ok(Stmt::Expression {
        expr: Expr::InfixOp {
          left: Box::new(Expr::Integer(1)),
          op: Tok::Plus,
          right: Box::new(Expr::Integer(2)),
        }
      })
    );
  }
}
