use std::process;

use thiserror::Error;

use crate::info;
use crate::interpreter::Interpreter;
use crate::runtime::{
  ExternalFunction,
  Literal,
  ReturnValue,
  ValueType,
};

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

pub unsafe fn get_sym_from_multiple_so(
  shared_libraries: &[crate::libloading::Library],
  name: &str,
) -> Option<ExternalFunction> {
  shared_libraries.iter().find_map(|so| get_sym_from_so(so, name))
}

pub unsafe fn get_sym_from_so(
  so: &crate::libloading::Library,
  name: &str,
) -> Option<ExternalFunction> {
  so.get(name.as_bytes()).ok()
}

pub unsafe fn call_func_sym(
  interpreter: &mut Interpreter,
  sym: ExternalFunction,
  params: Vec<Literal>,
  ret_ty: ReturnValue<ValueType>,
) -> Option<Literal> {
  use std::ffi::{
    c_void,
    CStr,
    CString,
  };
  use std::os::raw::c_char;

  use seq_macro::seq;

  type VoidPtr = *const c_void;

  if sym.clone().into_raw().is_null() {
    info!(INTR_ERR, "`call_func_sym`'s `sym` is null");
    process::exit(1);
  }

  let mut cstrs = Vec::new();
  let mut arrs = Vec::new();

  fn to_void_ptr(cstrs: &mut Vec<CString>, arrs: &mut Vec<Vec<VoidPtr>>, lit: &Literal) -> VoidPtr {
    match lit {
      Literal::String(s) => {
        let cstring = CString::new(s.to_string()).unwrap();
        let ptr = cstring.as_ptr();
        cstrs.push(cstring);
        ptr as VoidPtr
      },
      Literal::Integer(n) => *n as VoidPtr,
      Literal::Array(elems) => {
        let mut arr: Vec<VoidPtr> =
          elems.iter().map(|elem| to_void_ptr(cstrs, arrs, elem)).collect();
        let ptr = arr.as_mut_ptr();
        arrs.push(arr);
        ptr as VoidPtr
      },
    }
  }

  unsafe fn from_void_ptr(interpreter: &mut Interpreter, ty: ValueType, ptr: VoidPtr) -> Literal {
    match ty {
      ValueType::Void => unreachable!("ValueType::Void are not supposed to be created"),
      ValueType::String => {
        let ptr = ptr as *const c_char;
        let cstr = CStr::from_ptr(ptr);
        let s = cstr.to_string_lossy().into_owned();
        Literal::String(s)
      },
      ValueType::Integer => {
        let n = ptr as i64;
        Literal::Integer(n)
      },
      ValueType::Array(elems_ty, len) => {
        let len = match interpreter.interpret_expr(&len).unwrap() {
          Literal::Integer(len) => len,
          _ => unreachable!("Resolver didn't resolve the len of an array type"),
        };
        let c_vec = c_vec::CVec::new(ptr as *mut *const c_void, len as usize);
        let elems = c_vec
          .iter()
          .map(|ptr| from_void_ptr(interpreter, elems_ty.as_ref().clone(), *ptr))
          .collect();
        Literal::Array(elems)
      },
    }
  }

  let mut params_as_ptr = params.iter().map(|param| to_void_ptr(&mut cstrs, &mut arrs, param));

  macro_rules! gen_match {
    ($limit:literal) => {
      seq!(I in 1..$limit {
        match params.len() {
          0 => {
            let ptr = sym(std::ptr::null());
            Some(from_void_ptr(interpreter, ret_ty?, ptr))
          },
          #(
            I => {
              let ptr =
                seq!(_ in 0..I {
                  sym(
                    #(params_as_ptr.next().unwrap(),)*
                  )
                });
              Some(from_void_ptr(interpreter, ret_ty?, ptr))
            },
          )*
          _ => unreachable!()
        }
      })
    };
  }

  // Done because clippy obviously takes a very long time to lint more than 255!
  // line of code
  #[cfg(feature = "clippy")]
  {
    let _ = &mut params_as_ptr;
    gen_match!(1);
    info!(INTR_ERR, "pyr has been built with feature \"clippy\"");
    process::exit(1);
  }

  #[cfg(not(feature = "clippy"))]
  crate::max_params_len!(gen_match)
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
