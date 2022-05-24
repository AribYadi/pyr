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
  TokenKind as Tok,
};

pub mod syntax;

pub struct Parser<'a> {
  lexer: Lexer<'a, Tok>,
  peek: Tok,
  curr: Tok,
}

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

  pub fn parse(&mut self) -> Expr {
    let mut errors = Vec::new();
    while self.peek != Tok::Eof {
      if self.curr != Tok::Newline {
        todo!("Synchronize the parser");
      }
      match self.parse_expr() {
        Ok(_) => todo!("Do something with the expr"),
        Err(err) => errors.push(err),
      };
    }
    todo!()
  }

  pub(super) fn parse_expr(&mut self) -> Result<Expr> {
    match self.peek {
      literal @ Tok::String | literal @ Tok::Integer | literal @ Tok::Identifier => {
        let text = self.lexeme();
        self.consume(literal)?;
        match literal {
          Tok::String => Ok(Expr::String(text[1..text.len() - 1].to_string())),
          Tok::Integer => Ok(Expr::Integer(text.parse().unwrap())),
          Tok::Identifier => Ok(Expr::Identifier(text)),
          _ => unreachable!(),
        }
      },
      op @ Tok::Minus | op @ Tok::Bang => {
        self.consume(op)?;
        let right = self.parse_expr()?;
        Ok(Expr::PrefixOp { op, right: Box::new(right) })
      },
      tok => Err(ParseError::new(ParseErrorKind::ExpectedExpressionStart(tok), self.lexer.span())),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_expr() {
    fn parse(input: &str) -> Result<Expr> {
      let mut parser = Parser::new(input);
      parser.parse_expr()
    }

    let expr = parse("  32");
    assert_eq!(expr, Ok(Expr::Integer(32)));
    let expr = parse("\"foobar\"");
    assert_eq!(expr, Ok(Expr::String("foobar".to_string())));
    let expr = parse("foobar");
    assert_eq!(expr, Ok(Expr::Identifier("foobar".to_string())));

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
}
