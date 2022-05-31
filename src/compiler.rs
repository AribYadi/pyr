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
use crate::runtime::{
  IndentLevel,
  Variables,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueType {
  Integer,
  String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ValueWrapper {
  v: LLVMValueRef,
  ty: ValueType,
}

impl ValueWrapper {
  unsafe fn new_integer(self_: &mut Compiler, v: i64) -> Self {
    let ty = LLVMInt64TypeInContext(self_.ctx);
    let v = if v > 0 { LLVMConstInt(ty, v as u64, 0) } else { LLVMConstInt(ty, v as u64, 1) };

    Self { v, ty: ValueType::Integer }
  }

  unsafe fn new_string(self_: &mut Compiler, v: &str) -> Self {
    let ptr = self_.cstring(v);
    let length = v.len() as u32;
    let v = LLVMConstStringInContext(self_.ctx, ptr, length, 0);

    Self { v, ty: ValueType::String }
  }

  fn is_integer(&self) -> bool { self.ty == ValueType::Integer }

  fn is_string(&self) -> bool { self.ty == ValueType::String }

  unsafe fn get_as_integer(&self) -> i64 {
    assert!(self.is_integer(), "Value is not an integer");
    LLVMConstIntGetZExtValue(self.v) as i64
  }

  unsafe fn get_as_string(&self) -> String {
    assert!(self.is_string(), "Value is not a string");
    let mut length = 0;
    let ptr = LLVMGetAsString(self.v, &mut length);
    utils::ptr_to_str(ptr, length).to_string()
  }

  fn is_truthy(&self) -> bool {
    unsafe {
      match self.ty {
        ValueType::Integer => self.get_as_integer() == 1,
        ValueType::String => !self.get_as_string().is_empty(),
      }
    }
  }
}

impl std::fmt::Display for ValueWrapper {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    unsafe {
      match self.ty {
        ValueType::Integer => write!(f, "{}", self.get_as_integer()),
        ValueType::String => write!(f, "{}", self.get_as_string()),
      }
    }
  }
}

mod utils {
  use super::*;

  pub fn ptr_to_str<'a>(ptr: *const i8, length: usize) -> Cow<'a, str> {
    if length < 2 {
      return "".into();
    }
    unsafe { CStr::from_ptr(ptr).to_string_lossy() }
  }
}

pub struct Compiler {
  pub ctx: LLVMContextRef,
  pub builder: LLVMBuilderRef,
  pub module: LLVMModuleRef,

  cstring_cache: HashMap<String, CString>,
  variables: Variables<ValueWrapper>,
  indent_level: IndentLevel,
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

    Compiler { ctx, builder, module, cstring_cache, variables: Variables::new(), indent_level: 0 }
  }

  // fn get_func(&mut self, name: &str) -> Option<LLVMValueRef> {
  //   unsafe {
  //     let name = self.cstring(name);
  //     let func = LLVMGetNamedFunction(self.module, name);
  //     if func.is_null() {
  //       None
  //     } else {
  //       Some(func)
  //     }
  //   }
  // }

  // fn curr_func(&self) -> LLVMValueRef { self.curr_func.unwrap() }

  // fn alloca_at_entry(&mut self, name: &str) -> LLVMValueRef {
  //   unsafe {
  //     let builder = LLVMCreateBuilderInContext(self.ctx);
  //     let entry = LLVMGetFirstBasicBlock(self.curr_func());

  //     let first_instr = LLVMGetFirstInstruction(entry);
  //     if !first_instr.is_null() {
  //       LLVMPositionBuilderBefore(builder, first_instr);
  //     } else {
  //       LLVMPositionBuilderAtEnd(builder, entry);
  //     }

  //     let name = self.cstring(name);
  //     LLVMBuildAlloca(builder, LLVMInt64TypeInContext(self.ctx), name)
  //   }
  // }

  fn compile_expr(&mut self, expr: &Expr) -> ValueWrapper {
    unsafe {
      match &expr.kind {
        ExprKind::Integer(n) => ValueWrapper::new_integer(self, *n),
        ExprKind::String(s) => ValueWrapper::new_string(self, s),
        ExprKind::Identifier(name) => self
          .variables
          .get(name)
          .map(|(_, v)| v.clone())
          .unwrap_or_else(|| unreachable!("Resolver didn't report undefined variable {name}")),

        ExprKind::PrefixOp { op, right } => {
          let right = self.compile_expr(right);
          self.compile_prefix_op(op, right)
        },
        ExprKind::InfixOp { op, left, right } => {
          let left = self.compile_expr(left);
          let right = self.compile_expr(right);
          self.compile_infix_op(op, left, right)
        },
        ExprKind::VarAssign { name, expr } => {
          let expr = self.compile_expr(expr);
          self.variables.insert(name.to_string(), (self.indent_level, expr.clone()));
          expr
        },
      }
    }
  }

  fn compile_prefix_op(&mut self, op: &TokenKind, right: ValueWrapper) -> ValueWrapper {
    match op {
      TokenKind::Minus => right.neg(self),
      TokenKind::Bang => right.not(self),

      _ => unreachable!("{op} is not a prefix operator"),
    }
  }

  fn compile_infix_op(
    &mut self,
    op: &TokenKind,
    left: ValueWrapper,
    right: ValueWrapper,
  ) -> ValueWrapper {
    match op {
      TokenKind::Plus => left.add(self, right),
      TokenKind::Minus => left.sub(self, right),
      TokenKind::Star => left.mul(self, right),
      TokenKind::Slash => left.div(self, right),

      _ => unreachable!("{op} is not an infix operator"),
    }
  }

  pub unsafe fn compile(&mut self, _stmts: &[Stmt]) -> Result<(), ()> {
    todo!();
  }
}

mod impl_arithmetics;
#[cfg(test)]
mod tests;
