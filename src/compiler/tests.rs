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

      let ptr = LLVMPrintValueToString(value);
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
}
