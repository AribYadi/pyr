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

pub struct Parser<'a> {
  lexer: Lexer<'a, Tok>,
  peek: Tok,
  curr: Tok,
  indent_level: usize,
}

impl Parser<'_> {
  pub fn new(input: &str) -> Parser {
    let mut lexer = Tok::lexer(input);
    Parser { peek: lexer.next().unwrap_or(Tok::Eof), curr: Tok::Eof, lexer, indent_level: 0 }
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

  fn consume_indents(&mut self) -> Result<bool> {
    if (0..self.indent_level - 1)
      .rev()
      .any(|i| self.lexer.clone().nth(i).unwrap_or(Tok::Eof) != Tok::Indent)
      || self.peek != Tok::Indent
    {
      return Ok(false);
    }
    for _ in 0..self.indent_level {
      self.consume(Tok::Indent)?;
    }

    Ok(true)
  }

  fn after_indents(&self) -> Tok {
    if self.indent_level == 0 {
      self.peek
    } else {
      self.lexer.clone().nth(self.indent_level - 1).unwrap_or(Tok::Eof)
    }
  }

  fn check_curr(&mut self, expected: Tok) -> Result<()> {
    if self.curr == expected {
      Ok(())
    } else {
      Err(ParseError::new(ParseErrorKind::UnexpectedToken(expected, self.curr), self.lexer.span()))
    }
  }

  pub fn parse(&mut self) -> Vec<Stmt> {
    let mut errors = Vec::new();
    while self.peek != Tok::Eof {
      match self.statement() {
        Ok(_) => todo!("Do something with the stmt"),
        Err(err) => errors.push(err),
      };
      if let Err(err) = self.consume(Tok::Newline) {
        errors.push(err);
        todo!("Synchronize the parser");
      }
    }
    todo!()
  }

  pub(super) fn statement(&mut self) -> Result<Stmt> {
    match self.peek {
      Tok::Newline => {
        self.consume(Tok::Newline)?;
        if self.indent_level > 0 {
          self.consume_indents()?;
        }
        self.statement()
      },
      Tok::If => {
        self.consume(Tok::If)?;
        let condition = self.expression()?;
        self.consume(Tok::Colon)?;
        self.consume(Tok::Newline)?;
        if self.peek != Tok::Indent {
          return Err(ParseError::new(
            ParseErrorKind::UnexpectedToken(Tok::Indent, self.peek),
            self.lexer.span(),
          ));
        }

        let body = self.parse_block()?;
        self.check_curr(Tok::Newline)?;

        let mut else_stmt = vec![];
        if self.after_indents() == Tok::Else {
          // In case of else statement inside a block
          if self.indent_level > 0 {
            self.consume_indents()?;
          }
          self.parse_else(&mut else_stmt)?;
        }

        Ok(Stmt::If { condition, body, else_stmt })
      },
      Tok::While => {
        self.consume(Tok::While)?;
        let condition = self.expression()?;
        self.consume(Tok::Colon)?;
        self.consume(Tok::Newline)?;
        if self.peek != Tok::Indent {
          return Err(ParseError::new(
            ParseErrorKind::UnexpectedToken(Tok::Indent, self.peek),
            self.lexer.span(),
          ));
        }

        let body = self.parse_block()?;
        self.check_curr(Tok::Newline)?;

        Ok(Stmt::While { condition, body })
      },
      Tok::Print => {
        self.consume(Tok::Print)?;
        let expr = self.expression()?;
        self.consume(Tok::Newline)?;
        Ok(Stmt::Print { expr })
      },

      Tok::Indent => Err(ParseError::new(ParseErrorKind::UnexpectedIndentBlock, self.lexer.span())),
      Tok::Else => Err(ParseError::new(ParseErrorKind::UnmatchedElseStatement, self.lexer.span())),

      _ => {
        let expr = self.expression()?;
        self.consume(Tok::Newline)?;
        Ok(Stmt::Expression { expr })
      },
    }
  }

  fn parse_block(&mut self) -> Result<Vec<Stmt>> {
    self.indent_level += 1;
    let mut stmts = Vec::new();
    while self.peek != Tok::Eof {
      if !self.consume_indents()? {
        break;
      };
      stmts.push(self.statement()?);
    }
    self.indent_level -= 1;
    Ok(stmts)
  }

  fn parse_else(&mut self, else_stmt: &mut Vec<Stmt>) -> Result<()> {
    self.consume(Tok::Else)?;
    if self.peek == Tok::If {
      else_stmt.push(self.statement()?);
    } else {
      self.consume(Tok::Colon)?;
      self.consume(Tok::Newline)?;
      if self.peek != Tok::Indent {
        return Err(ParseError::new(
          ParseErrorKind::UnexpectedToken(Tok::Indent, self.peek),
          self.lexer.span(),
        ));
      }

      let new_else_stmt = self.parse_block()?;
      self.check_curr(Tok::Newline)?;

      *else_stmt = new_else_stmt;
    }

    Ok(())
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
        Tok::Newline | Tok::Colon | Tok::RightParen => break,
        tok => {
          return Err(ParseError::new(ParseErrorKind::UnknownInfixOperator(tok), self.lexer.span()))
        },
      }
    }

    Ok(lhs)
  }
}

#[cfg(test)]
mod tests;
