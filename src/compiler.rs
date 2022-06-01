mod impl_arithmetics;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{
  CStr,
  CString,
};
use std::{
  process,
  ptr,
};

use llvm::analysis::{
  LLVMVerifierFailureAction,
  LLVMVerifyFunction,
  LLVMVerifyModule,
};
use llvm::target::*;
use llvm::target_machine::{
  LLVMCodeGenFileType,
  LLVMCodeGenOptLevel,
  LLVMCodeModel,
  LLVMCreateTargetDataLayout,
  LLVMCreateTargetMachine,
  LLVMGetDefaultTargetTriple,
  LLVMGetTargetFromTriple,
  LLVMRelocMode,
  LLVMTargetMachineEmitToFile,
};
use llvm::transforms::instcombine::LLVMAddInstructionCombiningPass;
use llvm::transforms::scalar::{
  LLVMAddBasicAliasAnalysisPass,
  LLVMAddCFGSimplificationPass,
  LLVMAddGVNPass,
  LLVMAddReassociatePass,
};
use llvm::transforms::util::LLVMAddPromoteMemoryToRegisterPass;
use llvm::LLVMCallConv;
use llvm_sys as llvm;

use llvm::core::*;
use llvm::prelude::*;

use crate::info;
use crate::parser::syntax::{
  Expr,
  ExprKind,
  Stmt,
  StmtKind,
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
  pub module: LLVMModuleRef,
  pub builder: LLVMBuilderRef,
  pub fpm: LLVMPassManagerRef,

  cstring_cache: HashMap<String, CString>,
  main_func: LLVMValueRef,
  curr_func: LLVMValueRef,

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

  fn declare_libc_functions(&mut self) {
    unsafe {
      let printf_type = LLVMFunctionType(
        LLVMInt32TypeInContext(self.ctx),
        [LLVMPointerType(LLVMInt8TypeInContext(self.ctx), 0)].as_mut_ptr(),
        1,
        1,
      );
      let printf_func = LLVMAddFunction(self.module, self.cstring("printf"), printf_type);
      LLVMSetFunctionCallConv(printf_func, LLVMCallConv::LLVMCCallConv as u32);
    }
  }

  pub unsafe fn new(file_name: &str) -> Compiler {
    let mut cstring_cache = HashMap::new();
    let cstring = CString::new(file_name).unwrap();
    let ptr = cstring.as_ptr();
    cstring_cache.insert(file_name.to_string(), cstring);

    let ctx = LLVMContextCreate();
    let module = LLVMModuleCreateWithNameInContext(ptr, ctx);
    let builder = LLVMCreateBuilderInContext(ctx);

    let fpm = LLVMCreateFunctionPassManagerForModule(module);

    LLVMAddInstructionCombiningPass(fpm);
    LLVMAddReassociatePass(fpm);
    LLVMAddGVNPass(fpm);
    LLVMAddCFGSimplificationPass(fpm);
    LLVMAddBasicAliasAnalysisPass(fpm);
    LLVMAddPromoteMemoryToRegisterPass(fpm);

    LLVMInitializeFunctionPassManager(fpm);

    let func_ty = LLVMFunctionType(LLVMInt32TypeInContext(ctx), std::ptr::null_mut(), 0, 0);
    let cstring = CString::new("main").unwrap();
    let ptr = cstring.as_ptr();
    cstring_cache.insert("main".to_string(), cstring);
    let func = LLVMAddFunction(module, ptr, func_ty);

    let cstring = CString::new("entry").unwrap();
    let ptr = cstring.as_ptr();
    cstring_cache.insert("entry".to_string(), cstring);
    let bb = LLVMAppendBasicBlockInContext(ctx, func, ptr);
    LLVMPositionBuilderAtEnd(builder, bb);

    let mut self_ = Compiler {
      ctx,
      module,
      builder,
      fpm,
      cstring_cache,
      variables: Variables::new(),
      indent_level: 0,
      main_func: func,
      curr_func: func,
    };

    self_.declare_libc_functions();

    self_
  }

  fn get_func(&mut self, name: &str) -> Option<(LLVMValueRef, LLVMTypeRef)> {
    unsafe {
      let name = self.cstring(name);
      let func = LLVMGetNamedFunction(self.module, name);
      if func.is_null() {
        None
      } else {
        let ty = LLVMTypeOf(func);
        let ty = LLVMGetElementType(ty);
        Some((func, ty))
      }
    }
  }

  fn alloca_string_at_entry(&mut self, value: LLVMValueRef) -> LLVMValueRef {
    unsafe {
      let builder = LLVMCreateBuilderInContext(self.ctx);
      let entry = LLVMGetFirstBasicBlock(self.curr_func);

      let first_instr = LLVMGetFirstInstruction(entry);
      if !first_instr.is_null() {
        LLVMPositionBuilderBefore(builder, first_instr);
      } else {
        LLVMPositionBuilderAtEnd(builder, entry);
      }

      let name = self.cstring("str");
      let value_ptr = LLVMBuildAlloca(builder, LLVMTypeOf(value), name);
      LLVMBuildStore(self.builder, value, value_ptr);
      value_ptr
    }
  }

  fn compile_stmt(&mut self, stmt: &Stmt) {
    unsafe {
      let zero = LLVMConstInt(LLVMInt64TypeInContext(self.ctx), 0, 0);

      match &stmt.kind {
        StmtKind::Expression { expr } => {
          self.compile_expr(expr);
        },
        StmtKind::Print { expr } => {
          let value = self.compile_expr(expr);
          let value = ValueWrapper::new_string(self, &value.to_string()).v;
          let value_ptr = self.alloca_string_at_entry(value);
          let value = LLVMBuildGEP2(
            self.builder,
            LLVMTypeOf(value),
            value_ptr,
            [zero, zero].as_mut_ptr(),
            2,
            self.cstring(""),
          );

          let (printf_func, printf_ty) = self.get_func("printf").unwrap();
          LLVMBuildCall2(
            self.builder,
            printf_ty,
            printf_func,
            [value].as_mut_ptr(),
            1,
            self.cstring(""),
          );
        },
        StmtKind::If { condition, body, else_stmt } => {
          let condition = self.compile_expr(condition).not(self).not(self);
          let condition = LLVMBuildICmp(
            self.builder,
            llvm::LLVMIntPredicate::LLVMIntNE,
            condition.v,
            zero,
            self.cstring(""),
          );

          let mut then_bb =
            LLVMAppendBasicBlockInContext(self.ctx, self.curr_func, self.cstring("then"));
          let mut else_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("else"));
          let continue_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("continue"));

          LLVMBuildCondBr(self.builder, condition, then_bb, else_bb);
          LLVMPositionBuilderAtEnd(self.builder, then_bb);

          for stmt in body {
            self.compile_stmt(stmt);
          }

          LLVMBuildBr(self.builder, continue_bb);
          then_bb = LLVMGetInsertBlock(self.builder);

          LLVMAppendExistingBasicBlock(self.curr_func, else_bb);
          LLVMPositionBuilderAtEnd(self.builder, else_bb);

          for stmt in else_stmt {
            self.compile_stmt(stmt);
          }

          LLVMBuildBr(self.builder, continue_bb);
          else_bb = LLVMGetInsertBlock(self.builder);

          LLVMAppendExistingBasicBlock(self.curr_func, continue_bb);
          LLVMPositionBuilderAtEnd(self.builder, continue_bb);
        },
        StmtKind::While { condition, body } => {},
      }
    }
  }

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

  unsafe fn finish(&mut self, stmts: &[Stmt]) {
    for stmt in stmts {
      self.compile_stmt(stmt);
    }

    LLVMBuildRet(self.builder, LLVMConstInt(LLVMInt32TypeInContext(self.ctx), 0, 0));
    LLVMVerifyFunction(self.main_func, LLVMVerifierFailureAction::LLVMAbortProcessAction);
    LLVMRunFunctionPassManager(self.fpm, self.main_func);

    LLVMVerifyModule(
      self.module,
      LLVMVerifierFailureAction::LLVMAbortProcessAction,
      ptr::null_mut(),
    );
  }

  pub unsafe fn compile_to_obj(mut self, output_file: &str, stmts: &[Stmt]) {
    self.finish(stmts);

    let target_triple = LLVMGetDefaultTargetTriple();

    LLVM_InitializeAllTargetInfos();
    LLVM_InitializeAllTargets();
    LLVM_InitializeAllTargetMCs();
    LLVM_InitializeAllAsmParsers();
    LLVM_InitializeAllAsmPrinters();

    let mut error = ptr::null_mut();

    let mut target = ptr::null_mut();
    if LLVMGetTargetFromTriple(target_triple as *const _, &mut target, &mut error) != 0 {
      info!(
        ERR,
        "Failed to get target from triple:\n {error}",
        error = CStr::from_ptr(error).to_string_lossy()
      );
      process::exit(1);
    }

    let cpu = self.cstring("generic");
    let feats = self.cstring("");

    let opt = LLVMCodeGenOptLevel::LLVMCodeGenLevelNone;
    let rm = LLVMRelocMode::LLVMRelocDefault;
    let cm = LLVMCodeModel::LLVMCodeModelDefault;
    let target_machine =
      LLVMCreateTargetMachine(target, target_triple as *const _, cpu, feats, opt, rm, cm);

    LLVMSetModuleDataLayout(self.module, LLVMCreateTargetDataLayout(target_machine));
    LLVMSetTarget(self.module, target_triple as *const _);

    let pass = LLVMCreatePassManager();

    let file_type = LLVMCodeGenFileType::LLVMObjectFile;
    let filename = self.cstring(output_file) as *mut _;

    if LLVMTargetMachineEmitToFile(target_machine, self.module, filename, file_type, &mut error)
      != 0
    {
      info!(
        ERR,
        "Failed to emit to file:\n {error}",
        error = CStr::from_ptr(error).to_string_lossy()
      );
      process::exit(1);
    }

    LLVMRunPassManager(pass, self.module);
  }
}

impl Drop for Compiler {
  fn drop(&mut self) {
    unsafe {
      LLVMDisposeBuilder(self.builder);
      LLVMDisposeModule(self.module);
      LLVMContextDispose(self.ctx);
    }
  }
}

#[cfg(test)]
mod tests;
