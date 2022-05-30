use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{
  CStr,
  CString,
};

use llvm_sys as llvm;

use llvm::core::*;
use llvm::prelude::*;

use crate::parser::syntax::{
  Expr,
  ExprKind,
  Stmt,
  TokenKind,
};

mod utils {
  use super::*;

  pub fn ptr_to_str<'a>(ptr: *const i8, length: usize) -> Cow<'a, str> {
    if length < 2 {
      return "".into();
    }
    unsafe { CStr::from_ptr(ptr).to_string_lossy() }
  }

  pub fn get_str<'a>(value: LLVMValueRef) -> Cow<'a, str> {
    let mut length = 0;
    let ptr = unsafe { LLVMGetAsString(value, &mut length) };
    ptr_to_str(ptr, length)
  }

  pub fn is_const_integer(value: LLVMValueRef) -> bool {
    unsafe { !LLVMIsAConstantInt(value).is_null() }
  }

  pub fn is_const_string(value: LLVMValueRef) -> bool {
    unsafe { LLVMIsConstantString(value) == 1 }
  }

  pub fn is_truthy(value: LLVMValueRef) -> bool {
    unsafe {
      if is_const_integer(value) {
        let value = LLVMConstIntGetZExtValue(value);
        return value == 1;
      }
      if is_const_string(value) {
        let string = get_str(value);
        return !string.is_empty();
      }

      unreachable!()
    }
  }

  pub fn const_string(self_: &mut Compiler, s: &str) -> LLVMValueRef {
    unsafe {
      let ptr = self_.cstring(s);
      let length = s.len() as u32;
      LLVMConstStringInContext(self_.ctx, ptr, length, 0)
    }
  }

  pub fn get_to_string_of(value: LLVMValueRef) -> String {
    if is_const_integer(value) {
      unsafe {
        let value = LLVMConstIntGetZExtValue(value);
        return value.to_string();
      }
    }
    if is_const_string(value) {
      let s2 = get_str(value);
      return s2.to_string();
    }

    unreachable!("Exhaustive match in `get_to_string_of`");
  }

  pub fn add_to_string(
    self_: &mut Compiler,
    left: LLVMValueRef,
    right: LLVMValueRef,
  ) -> LLVMValueRef {
    assert!(is_const_string(left));
    let mut s = get_str(left).to_string();
    s.push_str(&get_to_string_of(right));

    const_string(self_, &s)
  }

  pub fn mul_string(self_: &mut Compiler, left: LLVMValueRef, right: LLVMValueRef) -> LLVMValueRef {
    assert!(is_const_string(left));
    assert!(is_const_integer(right));
    unsafe {
      let mut s = get_str(left).to_string();
      let n = LLVMConstIntGetZExtValue(right) - 1;
      for _ in 0..n {
        s.push_str(&get_to_string_of(left));
      }

      const_string(self_, &s)
    }
  }
}

pub struct Compiler {
  pub ctx: LLVMContextRef,
  pub builder: LLVMBuilderRef,
  pub module: LLVMModuleRef,

  cstring_cache: HashMap<String, CString>,
  curr_func: Option<LLVMValueRef>,
}

impl Compiler {
  fn cstring(&mut self, s: &str) -> *const i8 {
    let s = s.to_string();
    if let Some(cstring) = self.cstring_cache.get(&s) {
      return cstring.as_ptr();
    }

    let cstring = CString::new(s.as_bytes()).unwrap();
    let ptr = cstring.as_ptr();
    self.cstring_cache.insert(s, cstring);
    ptr
  }

  pub unsafe fn new(file_name: &str) -> Compiler {
    let mut cstring_cache = HashMap::new();
    let cstring = CString::new(file_name).unwrap();
    let ptr = cstring.as_ptr();
    cstring_cache.insert(file_name.to_string(), cstring);

    let ctx = LLVMContextCreate();
    let builder = LLVMCreateBuilderInContext(ctx);
    let module = LLVMModuleCreateWithNameInContext(ptr, ctx);

    Compiler { ctx, builder, module, cstring_cache, curr_func: None }
  }

  fn get_func(&mut self, name: &str) -> Option<LLVMValueRef> {
    unsafe {
      let name = self.cstring(name);
      let func = LLVMGetNamedFunction(self.module, name);
      if func.is_null() {
        None
      } else {
        Some(func)
      }
    }
  }

  fn curr_func(&self) -> LLVMValueRef { self.curr_func.unwrap() }

  fn alloca_at_entry(&mut self, name: &str) -> LLVMValueRef {
    unsafe {
      let builder = LLVMCreateBuilderInContext(self.ctx);
      let entry = LLVMGetFirstBasicBlock(self.curr_func());

      let first_instr = LLVMGetFirstInstruction(entry);
      if !first_instr.is_null() {
        LLVMPositionBuilderBefore(builder, first_instr);
      } else {
        LLVMPositionBuilderAtEnd(builder, entry);
      }

      let name = self.cstring(name);
      LLVMBuildAlloca(builder, LLVMInt64TypeInContext(self.ctx), name)
    }
  }

  fn compile_expr(&mut self, expr: &Expr) -> LLVMValueRef {
    unsafe {
      match &expr.kind {
        ExprKind::Integer(n) => {
          let int_type = LLVMInt64TypeInContext(self.ctx);
          LLVMConstInt(int_type, *n as u64, 0)
        },
        ExprKind::String(s) => utils::const_string(self, s),
        ExprKind::Identifier(_name) => unimplemented!("Variables are not implemented yet"),

        ExprKind::PrefixOp { op, right } => {
          let right = self.compile_expr(right);
          self.compile_prefix_op(op, right)
        },
        ExprKind::InfixOp { op, left, right } => {
          let left = self.compile_expr(left);
          let right = self.compile_expr(right);
          self.compile_infix_op(op, left, right)
        },
        ExprKind::VarAssign { .. } => unimplemented!("Variables are not implemented yet"),
      }
    }
  }

  fn compile_prefix_op(&mut self, op: &TokenKind, right: LLVMValueRef) -> LLVMValueRef {
    unsafe {
      let int_type = LLVMInt64TypeInContext(self.ctx);
      match op {
        TokenKind::Minus => LLVMConstNeg(right),
        TokenKind::Bang if utils::is_truthy(right) => LLVMConstInt(int_type, 0, 0),
        TokenKind::Bang => LLVMConstInt(int_type, 1, 0),

        _ => unreachable!("{op} is not a prefix operator"),
      }
    }
  }

  fn compile_infix_op(
    &mut self,
    op: &TokenKind,
    left: LLVMValueRef,
    right: LLVMValueRef,
  ) -> LLVMValueRef {
    unsafe {
      match op {
        TokenKind::Plus if utils::is_const_integer(left) && utils::is_const_integer(right) => {
          LLVMConstAdd(left, right)
        },
        TokenKind::Plus if utils::is_const_string(left) => utils::add_to_string(self, left, right),
        TokenKind::Plus if utils::is_const_string(right) => utils::add_to_string(self, right, left),
        TokenKind::Plus => unreachable!("Resolver didn't check `{op}` thoroughly"),
        TokenKind::Minus => LLVMConstSub(left, right),
        TokenKind::Star if utils::is_const_string(left) => utils::mul_string(self, left, right),
        TokenKind::Star if utils::is_const_string(right) => utils::mul_string(self, right, left),
        TokenKind::Star => LLVMConstMul(left, right),
        TokenKind::Slash => LLVMConstSDiv(left, right),

        _ => unreachable!("{op} is not an infix operator"),
      }
    }
  }

  pub unsafe fn compile(&mut self, _stmts: &[Stmt]) -> Result<(), ()> {
    todo!();
  }
}

#[cfg(test)]
mod tests;
