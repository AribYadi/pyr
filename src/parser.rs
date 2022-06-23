use logos::{
  Lexer,
  Logos,
};

use crate::error::{
  ParseError,
  ParseErrorKind,
  ParseResult as Result,
};
use crate::runtime::ValueType;

use self::syntax::{
  Expr,
  ExprKind,
  Operator,
  Stmt,
  StmtKind,
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
    if self.indent_level > 0 &&
      (0..self.indent_level - 1)
        .rev()
        .any(|i| self.lexer.clone().nth(i).unwrap_or(Tok::Eof) != Tok::Indent) ||
      self.peek != Tok::Indent
    {
      return Ok(false);
    }
    for _ in 0..self.indent_level {
      self.consume(Tok::Indent)?;
    }

    Ok(true)
  }

  fn consume_type(&mut self) -> Result<ValueType> {
    match self.next() {
      Tok::IntType => Ok(ValueType::Integer),
      Tok::StringType => Ok(ValueType::String),
      Tok::LeftBracket => {
        let ty = self.consume_type()?;

        self.consume(Tok::Semicolon)?;
        let lexeme = self.lexeme();
        self.consume(Tok::Integer)?;

        self.consume_delimiter(Tok::RightBracket)?;

        let len = lexeme.parse().unwrap();

        Ok(ValueType::Array(Box::new(ty), len))
      },

      _ => Err(ParseError::new(ParseErrorKind::ExpectedType(self.curr), self.lexer.span())),
    }
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

  fn synchronize(&mut self) {
    while self.peek != Tok::Eof {
      self.next();
      if let Tok::Else | Tok::Indent = self.peek {
        continue;
      }
      if self.curr == Tok::Newline {
        return;
      }
    }
  }

  pub fn parse(&mut self) -> std::result::Result<Vec<Stmt>, Vec<ParseError>> {
    let mut stmts = Vec::new();
    let mut errors = Vec::new();
    while self.peek != Tok::Eof {
      match self.statement() {
        Ok(stmt) => stmts.push(stmt),
        Err(ParseError { kind: ParseErrorKind::Eof, .. }) => break,
        Err(err) => {
          errors.push(err);
          self.synchronize();
        },
      };
      if self.peek != Tok::Eof {
        if let Err(err) = self.check_curr(Tok::Newline) {
          errors.push(err);
          self.synchronize();
        }
      }
    }

    if !errors.is_empty() {
      Err(errors)
    } else {
      Ok(stmts)
    }
  }

  pub(crate) fn statement(&mut self) -> Result<Stmt> {
    match self.peek {
      Tok::If => {
        let span_start = self.lexer.span().start;

        self.consume(Tok::If)?;
        let condition = self.expression()?;
        self.consume(Tok::Colon)?;

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

        let span_end = self.lexer.span().end;

        Ok(Stmt::new(StmtKind::If { condition, body, else_stmt }, span_start..span_end))
      },
      Tok::While => {
        let span_start = self.lexer.span().start;

        self.consume(Tok::While)?;
        let condition = self.expression()?;
        self.consume(Tok::Colon)?;

        let body = self.parse_block()?;
        self.check_curr(Tok::Newline)?;

        let span_end = self.lexer.span().end;

        Ok(Stmt::new(StmtKind::While { condition, body }, span_start..span_end))
      },
      Tok::Func => {
        let span_start = self.lexer.span().start;

        self.consume(Tok::Func)?;
        let name = self.lexeme();
        self.consume(Tok::Identifier)?;

        self.consume(Tok::LeftParen)?;
        let mut args = Vec::new();
        while let Tok::Identifier = self.peek {
          let name = self.lexeme();
          self.consume(Tok::Identifier)?;

          self.consume(Tok::Colon)?;
          let ty = self.consume_type()?;

          args.push((name, ty));

          if self.peek != Tok::RightParen {
            self.consume(Tok::Comma)?;
          }
        }
        self.consume(Tok::RightParen)?;

        let ret_ty = if self.peek == Tok::Arrow {
          self.consume(Tok::Arrow)?;
          Some(self.consume_type()?)
        } else {
          None
        };

        self.consume(Tok::Colon)?;
        let body = self.parse_block()?;

        let span_end = self.lexer.span().end;

        Ok(Stmt::new(StmtKind::FuncDef { name, args, body, ret_ty }, span_start..span_end))
      },
      Tok::Ret => {
        let span_start = self.lexer.span().start;

        self.consume(Tok::Ret)?;
        let expr = if self.peek != Tok::Newline { Some(self.expression()?) } else { None };
        self.consume(Tok::Newline)?;

        let span_end = self.lexer.span().end;

        Ok(Stmt::new(StmtKind::Ret { expr }, span_start..span_end))
      },
      Tok::Break => {
        let span_start = self.lexer.span().start;

        self.consume(Tok::Break)?;
        self.consume(Tok::Newline)?;

        let span_end = self.lexer.span().end;

        Ok(Stmt::new(StmtKind::Break, span_start..span_end))
      },

      // Ignore any lone block
      ws @ Tok::Indent | ws @ Tok::Newline => {
        self.consume(ws)?;
        while let Tok::Newline | Tok::Indent = self.peek {
          self.next();
        }
        self.statement()
      },
      Tok::Else => Err(ParseError::new(ParseErrorKind::UnmatchedElseStatement, self.lexer.span())),
      Tok::Eof => Err(ParseError::new(ParseErrorKind::Eof, self.lexer.span())),

      _ => {
        let span_start = self.lexer.span().start;

        let expr = self.expression()?;
        self.consume(Tok::Newline)?;

        let span_end = self.lexer.span().end;

        Ok(Stmt::new(StmtKind::Expression { expr }, span_start..span_end))
      },
    }
  }

  fn parse_block(&mut self) -> Result<Vec<Stmt>> {
    self.indent_level += 1;
    let mut stmts = Vec::new();
    if self.consume(Tok::Newline).is_err() {
      stmts.push(self.statement()?);
      self.indent_level -= 1;
      return Ok(stmts);
    }
    if self.peek != Tok::Indent {
      return Err(ParseError::new(
        ParseErrorKind::UnexpectedToken(Tok::Indent, self.peek),
        self.lexer.span(),
      ));
    }
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

      let new_else_stmt = self.parse_block()?;
      self.check_curr(Tok::Newline)?;

      *else_stmt = new_else_stmt;
    }

    Ok(())
  }

  pub(crate) fn expression(&mut self) -> Result<Expr> { self.parse_expr(0, Tok::Eof) }

  fn parse_expr(&mut self, bp: u8, last_op: Tok) -> Result<Expr> {
    let mut lhs = match self.peek {
      literal @ Tok::String |
      literal @ Tok::Integer |
      literal @ Tok::Identifier |
      literal @ Tok::True |
      literal @ Tok::False => {
        let text = self.lexeme();
        let span = self.lexer.span();
        self.consume(literal)?;
        match literal {
          Tok::String => {
            let text = if text.contains('\\') {
              snailquote::unescape(&text).map_err(|e| ParseError::new(e, span.clone()))?
            } else {
              text[1..text.len() - 1].to_string()
            };
            Ok(Expr::new(ExprKind::String(text), span))
          },
          Tok::Integer => Ok(Expr::new(ExprKind::Integer(text.parse().unwrap()), span)),
          Tok::Identifier => Ok(Expr::new(ExprKind::Identifier(text), span)),
          Tok::True => Ok(Expr::new(ExprKind::Integer(1), span)),
          Tok::False => Ok(Expr::new(ExprKind::Integer(0), span)),
          _ => unreachable!(),
        }
      },
      Tok::LeftParen => {
        self.consume(Tok::LeftParen)?;
        let expr = self.parse_expr(0, Tok::Eof)?;
        self.consume_delimiter(Tok::RightParen)?;
        Ok(expr)
      },
      op @ Tok::Minus | op @ Tok::Bang => {
        let span_start = self.lexer.span().start;

        self.consume(op)?;
        let ((), rbp) = op.prefix_bp();
        let right = self.parse_expr(rbp, op)?;

        let span_end = self.lexer.span().end;

        Ok(Expr::new(ExprKind::PrefixOp { op, right: Box::new(right) }, span_start..span_end))
      },
      // Arrays start with the type of the elements
      Tok::IntType | Tok::StringType => {
        let span_start = self.lexer.span().start;

        let ty = self.consume_type()?;
        self.consume(Tok::LeftBracket)?;

        let mut len = 0;
        let mut exprs = Vec::new();
        while self.peek != Tok::RightBracket {
          exprs.push(self.parse_expr(0, Tok::Eof)?);
          if self.peek != Tok::Comma {
            if self.peek == Tok::Semicolon {
              self.consume(Tok::Semicolon)?;
              let lexeme = self.lexeme();
              self.consume(Tok::Integer)?;
              len = lexeme.parse().unwrap();
            }
            self.consume_delimiter(Tok::RightBracket)?;
            break;
          }
          self.consume(Tok::Comma)?;
        }
        if len == 0 {
          len = exprs.len();
        }

        let span_end = self.lexer.span().end;

        Ok(Expr::new(ExprKind::Array(ty, exprs, len), span_start..span_end))
      },

      Tok::Error => {
        Err(ParseError::new(ParseErrorKind::UnknownToken(self.lexeme()), self.lexer.span()))
      },

      _ => Err(ParseError::new(ParseErrorKind::ExpectedExpression, self.lexer.span())),
    }?;
    while self.peek != Tok::Eof {
      match self.peek {
        op @ Tok::Plus |
        op @ Tok::Minus |
        op @ Tok::Star |
        op @ Tok::Slash |
        op @ Tok::Less |
        op @ Tok::Greater |
        op @ Tok::LessEqual |
        op @ Tok::GreaterEqual |
        op @ Tok::EqualEqual |
        op @ Tok::BangEqual |
        op @ Tok::Caret |
        op @ Tok::Percent |
        op @ Tok::LeftShift |
        op @ Tok::RightShift |
        op @ Tok::Ampersand |
        op @ Tok::Pipe |
        op @ Tok::Equal => {
          let (lbp, rbp) = op.infix_bp();
          if lbp < bp {
            break;
          }

          if (lbp == bp || rbp == bp) && lbp <= rbp {
            return Err(ParseError::new(
              ParseErrorKind::InvalidChainOperator(op, last_op),
              self.lexer.span(),
            ));
          }

          self.consume(op)?;
          let right = self.parse_expr(rbp, op)?;
          let span_start = lhs.span.start;
          let span_end = right.span.end;
          lhs = Expr::new(
            ExprKind::InfixOp { left: Box::new(lhs), op, right: Box::new(right) },
            span_start..span_end,
          );
        },
        op @ Tok::And | op @ Tok::Or => {
          let (lbp, rbp) = op.infix_bp();
          if lbp < bp {
            break;
          }
          if lbp == bp || rbp == bp {
            return Err(ParseError::new(
              ParseErrorKind::InvalidChainOperator(op, last_op),
              self.lexer.span(),
            ));
          }

          self.consume(op)?;
          let right = self.parse_expr(rbp, op)?;
          let span_start = lhs.span.start;
          let span_end = right.span.end;
          lhs = Expr::new(
            ExprKind::ShortCircuitOp { left: Box::new(lhs), op, right: Box::new(right) },
            span_start..span_end,
          );
        },
        op @ Tok::LeftParen => {
          // TODO: make anything be callable
          if let ExprKind::Identifier(ref name) = lhs.kind {
            self.consume(op)?;

            let mut args = Vec::new();
            while self.peek != Tok::RightParen {
              args.push(self.parse_expr(0, Tok::Eof)?);
              if self.peek != Tok::Comma {
                break;
              }
              self.consume(Tok::Comma)?;
            }

            self.consume_delimiter(Tok::RightParen)?;
            let span_start = lhs.span.start;
            let span_end = self.lexer.span().end;
            lhs = Expr::new(
              ExprKind::FuncCall { name: name.to_string(), params: args },
              span_start..span_end,
            );
          } else {
            return Err(ParseError::new(ParseErrorKind::NotCallable(lhs.clone()), lhs.span));
          }
        },
        left_bracket @ Tok::LeftBracket => {
          let span_start = lhs.span.start;

          self.consume(left_bracket)?;
          let index = self.parse_expr(0, Tok::Eof)?;
          self.consume_delimiter(Tok::RightBracket)?;

          let span_end = self.lexer.span().end;
          lhs = Expr::new(
            ExprKind::Index { array: Box::new(lhs), index: Box::new(index) },
            span_start..span_end,
          );
        },

        Tok::Newline |
        Tok::Colon |
        Tok::RightParen |
        Tok::Comma |
        Tok::Semicolon |
        Tok::RightBracket => break,
        Tok::Error => {
          return Err(ParseError::new(
            ParseErrorKind::UnknownToken(self.lexeme()),
            self.lexer.span(),
          ));
        },
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
