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

// TODO: support other types
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

  fn string(&self, ptr: *const i8, length: usize) -> Cow<'_, str> {
    if length < 2 {
      return "".into();
    }
    unsafe {
      CStr::from_ptr(ptr).to_string_lossy()
    }
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

  fn is_truthy(&self, value: LLVMValueRef) -> bool {
    unsafe {
      let int_value = LLVMIsAConstantInt(value);
      if !int_value.is_null() {
        let value = LLVMConstIntGetZExtValue(int_value);
        return value == 1;
      }
      let is_str = LLVMIsConstantString(value);
      if is_str == 1 {
        let mut length = 0;
        let ptr = LLVMGetAsString(value, &mut length);
        let string = self.string(ptr, length);
        return !string.is_empty();
      }

      unreachable!()
    }
  }

  fn compile_expr(&mut self, expr: &Expr) -> LLVMValueRef {
    unsafe {
      match &expr.kind {
        ExprKind::Integer(n) => {
          let int_type = LLVMInt64TypeInContext(self.ctx);
          LLVMConstInt(int_type, *n as u64, 0)
        },
        ExprKind::String(s) => {
          let ptr = self.cstring(s);
          let length = s.len() as u32;
          LLVMConstStringInContext(self.ctx, ptr, length, 0)
        },

        ExprKind::PrefixOp { op, right } => {
          let right = self.compile_expr(right);
          self.compile_prefix_op(op, right)
        },
        _ => todo!(),
      }
    }
  }

  fn compile_prefix_op(&mut self, op: &TokenKind, right: LLVMValueRef) -> LLVMValueRef {
    unsafe {
      let int_type = LLVMInt64TypeInContext(self.ctx);
      match op {
        TokenKind::Minus => LLVMBuildNeg(self.builder, right, self.cstring("sub")),
        TokenKind::Bang if self.is_truthy(right) => LLVMConstInt(int_type, 0, 0),
        TokenKind::Bang => LLVMConstInt(int_type, 1, 0),

        _ => unreachable!("{op} is not a prefix operator"),
      }
    }
  }

  pub unsafe fn compile(&mut self, _stmts: &[Stmt]) -> Result<(), ()> {
    todo!();
  }
}

#[cfg(test)]
mod tests;
