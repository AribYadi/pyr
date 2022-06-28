use thiserror::Error;

macro_rules! bx {
  ($val:expr) => {
    Box::new($val)
  };
}

macro_rules! rc {
  ($val:expr) => {
    std::rc::Rc::new($val)
  };
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum UnescapeError {
  #[error("expected something after `\\`")]
  LoneEscape,
  #[error("not enough number")]
  NotEnoughNum,
  #[error("invalid hex in hex escape")]
  InvalidHexInHexEscape,
  #[error("invalid octal number")]
  InvalidOctal,
  #[error("invalid hex in unicode escape")]
  InvalidHexInUnicodeEscape,
  #[error("invalid escape character")]
  InvalidEscape,
  #[error("unicode character code is out of bounds")]
  UnicodeOutOfBounds,
  #[error("invalid unicode character code")]
  InvalidUnicode,
}

pub fn unescape(s: impl Into<String>) -> Result<String, UnescapeError> {
  let s = s.into();
  let mut chars = s.chars().peekable();
  let mut out = String::new();

  while let Some(ch) = chars.next() {
    if ch == '\\' {
      let next_ch = chars.next().ok_or(UnescapeError::LoneEscape)?;
      let esc_ch = match next_ch {
        // To allow escaping as
        // "line1 \
        //  line2 \
        //  line3"
        '\n' => '\n',

        '\'' => '\'',
        '"' => '"',
        '\\' => '\\',
        'a' => '\x07',
        'b' => '\x08',
        'f' => '\x0c',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        'v' => '\x0b',

        '0'..='7' if matches!(chars.peek(), Some('0'..='7')) => {
          let first = next_ch.to_digit(8).ok_or(UnescapeError::InvalidOctal)?;

          let second = chars.next().ok_or(UnescapeError::NotEnoughNum)?;
          let second = second.to_digit(8).ok_or(UnescapeError::InvalidOctal)?;

          let third = chars.next().ok_or(UnescapeError::NotEnoughNum)?;
          let third = third.to_digit(8).ok_or(UnescapeError::InvalidOctal)?;

          let value = (first * 64 + second * 8 + third) as u8;
          value as char
        },
        '0' => '\0',

        'x' => {
          let first = chars.next().ok_or(UnescapeError::NotEnoughNum)?;
          let first = first.to_digit(16).ok_or(UnescapeError::InvalidHexInHexEscape)?;

          let second = chars.next().ok_or(UnescapeError::NotEnoughNum)?;
          let second = second.to_digit(16).ok_or(UnescapeError::InvalidHexInHexEscape)?;

          let value = (first * 16 + second) as u8;
          value as char
        },

        'u' => {
          let mut value = 0;

          for _ in 0..4 {
            let digit = chars
              .next()
              .ok_or(UnescapeError::NotEnoughNum)?
              .to_digit(16)
              .ok_or(UnescapeError::InvalidHexInUnicodeEscape)?;
            value = value * 16 + digit;
          }

          let next_ch = chars.peek().map(|ch| ch.to_digit(16));
          if matches!(next_ch, Some(Some(_))) {
            for _ in 0..2 {
              let digit = chars
                .next()
                .ok_or(UnescapeError::NotEnoughNum)?
                .to_digit(16)
                .ok_or(UnescapeError::InvalidHexInUnicodeEscape)?;
              value = value * 16 + digit;
            }
          }

          std::char::from_u32(value).ok_or({
            if value > 0x10FFFF {
              UnescapeError::UnicodeOutOfBounds
            } else {
              UnescapeError::InvalidUnicode
            }
          })?
        },

        _ => return Err(UnescapeError::InvalidEscape),
      };

      out.push(esc_ch);
      continue;
    }

    out.push(ch);
  }

  Ok(out)
}

#[test]
fn test_unescape() {
  macro_rules! assert_unescape {
    ($input:expr => UnescapeError::$expected:tt) => {
      assert_eq!(unescape($input), Err(UnescapeError::$expected))
    };
    ($input:expr => $expected:expr) => {
      assert_eq!(
        unescape($input),
        Ok($expected.to_string()),
        "{}",
        unescape($input).unwrap().as_bytes()[0] as char as u8
      )
    };
  }

  assert_unescape!("\\\n" => "\n");
  assert_unescape!("\\'" => "'");
  assert_unescape!("\\\"" => "\"");
  assert_unescape!("\\\\" => "\\");
  assert_unescape!("\\a" => "\x07");
  assert_unescape!("\\b" => "\x08");
  assert_unescape!("\\f" => "\x0c");
  assert_unescape!("\\n" => "\n");
  assert_unescape!("\\r" => "\r");
  assert_unescape!("\\t" => "\t");
  assert_unescape!("\\v" => "\x0b");
  assert_unescape!("\\012" => "\n");
  assert_unescape!("\\x0A" => "\n");
  assert_unescape!("\\u2764" => "\u{2764}");

  assert_unescape!("\\" => UnescapeError::LoneEscape);
  assert_unescape!("\\x1" => UnescapeError::NotEnoughNum);
  assert_unescape!("\\xgO" => UnescapeError::InvalidHexInHexEscape);
  assert_unescape!("\\038" => UnescapeError::InvalidOctal);
  assert_unescape!("\\u001G" => UnescapeError::InvalidHexInUnicodeEscape);
  assert_unescape!("\\p" => UnescapeError::InvalidEscape);
  assert_unescape!("\\uFFFFFF" => UnescapeError::UnicodeOutOfBounds);
  assert_unescape!("\\uDFFF" => UnescapeError::InvalidUnicode);
}
