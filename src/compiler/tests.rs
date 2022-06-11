use crate::parser::Parser;
use crate::resolver::Resolver;

use super::*;

#[test]
fn compile_expr() {
  fn compile(s: &str) -> &str {
    let mut parser = Parser::new(s);
    let expr = parser.expression().unwrap();
    let mut resolver = Resolver::new();
    resolver.resolve_expr(&expr).unwrap();
    unsafe {
      let mut compiler = Compiler::new("");
      let value = compiler.compile_expr(&expr);

      let ptr = LLVMPrintValueToString(value.v);
      CStr::from_ptr(ptr).to_str().unwrap()
    }
  }

  assert_eq!(compile("1"), "i64 1");
  assert_eq!(compile("!1"), "i64 0");
  assert_eq!(compile("!0"), "i64 1");
  assert_eq!(compile("-1"), "i64 -1");

  assert_eq!(compile("\"hello\""), r#"[6 x i8] c"hello\00""#);
  assert_eq!(compile("!\"hello\""), "i64 0");
  assert_eq!(compile("!\"\""), "i64 1");

  assert_eq!(compile("1 + 2"), "i64 3");
  assert_eq!(compile("1 - 2"), "i64 -1");
  assert_eq!(compile("1 * 2"), "i64 2");
  assert_eq!(compile("1 / 2"), "i64 0");

  assert_eq!(compile("\"1\" + 2"), r#"[3 x i8] c"12\00""#);
  assert_eq!(compile("1 + \"1\""), r#"[3 x i8] c"11\00""#);
  assert_eq!(compile("\"Hello\" + \" \" + \"World\""), r#"[12 x i8] c"Hello World\00""#);
  assert_eq!(compile("3 * \"triple\""), r#"[19 x i8] c"tripletripletriple\00""#);
  assert_eq!(compile("\"double\" * 2 + \"one\""), r#"[16 x i8] c"doubledoubleone\00""#);

  assert_eq!(compile("1 > 2"), "i64 0");
  assert_eq!(compile("1 < 2"), "i64 1");
  assert_eq!(compile("1 >= 2"), "i64 0");
  assert_eq!(compile("2 <= 2"), "i64 1");
  assert_eq!(compile("\"hello\" == 2"), "i64 0");
  assert_eq!(compile("\"hello\" != \"hello\""), "i64 0");

  assert_eq!(compile("2 ^ 3"), "i64 8");
  assert_eq!(compile("-2 ^ 2"), "i64 4");
}

// Commented out because variables are now pointers
// #[test]
// fn compile_variables() {
//   unsafe {
//     let mut compiler = Compiler::new("");
//     let mut resolver = Resolver::new();

//     macro_rules! compile_expr {
//       ($s:expr) => {{
//         let mut parser = Parser::new($s);
//         let expr = parser.expression().unwrap();
//         match resolver.resolve_expr(&expr) {
//           Ok(_) => Ok(compiler.compile_expr(&expr)),
//           Err(e) => Err(e),
//         }
//       }};
//     }

//     let value = compile_expr!("x");
//     assert_eq!(
//       value,
//       Err(RuntimeError::new(RuntimeErrorKind::UndefinedVariable("x".
// to_string()), 0..1))     );

//     let value = compile_expr!("x = 1");
//     assert_eq!(value, Ok(ValueWrapper::new_integer(&mut compiler, 1)));
//     let value = compile_expr!("x");
//     assert_eq!(value, Ok(ValueWrapper::new_integer(&mut compiler, 1)));

//     let value = compile_expr!("x = x - 1");
//     assert_eq!(value, Ok(ValueWrapper::new_integer(&mut compiler, 0)));
//     let value = compile_expr!("x = x - 1");
//     assert_eq!(value, Ok(ValueWrapper::new_integer(&mut compiler, -1)));
//     let value = compile_expr!("x + 10");
//     assert_eq!(value, Ok(ValueWrapper::new_integer(&mut compiler, 9)));
//   }
// }
