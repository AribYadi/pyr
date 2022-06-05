mod impl_arithmetics;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{
  CStr,
  CString,
};
use std::os::raw::c_char;
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
use llvm::{
  LLVMCallConv,
  LLVMIntPredicate,
};
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
  is_pointer: bool,
  is_runtime: bool,
  can_be_loaded: bool,
  ptr_len: Option<LLVMValueRef>,
}

impl ValueWrapper {
  unsafe fn new_integer(self_: &mut Compiler, v: i64) -> Self {
    let ty = LLVMInt64TypeInContext(self_.ctx);
    let v = if v > 0 { LLVMConstInt(ty, v as u64, 0) } else { LLVMConstInt(ty, v as u64, 1) };

    Self {
      v,
      ty: ValueType::Integer,
      is_pointer: false,
      can_be_loaded: true,
      is_runtime: false,
      ptr_len: None,
    }
  }

  unsafe fn new_string(self_: &mut Compiler, v: &str) -> Self {
    let ptr = self_.cstring(v);
    let length = v.len() as u32;
    let v = LLVMConstStringInContext(self_.ctx, ptr, length, 0);

    Self {
      v,
      ty: ValueType::String,
      is_pointer: false,
      can_be_loaded: true,
      is_runtime: false,
      ptr_len: None,
    }
  }

  unsafe fn new_variable(self_: &mut Compiler, inner: ValueWrapper) -> Self {
    let v = self_.alloca_at_entry(inner.v);
    Self { v, ty: inner.ty, is_pointer: true, can_be_loaded: true, is_runtime: true, ptr_len: None }
  }

  fn is_integer(&self) -> bool { self.ty == ValueType::Integer }

  fn is_string(&self) -> bool { self.ty == ValueType::String }

  // Since pointers are also runtime values, we can say that a pointer is a
  // runtime value
  fn is_runtime(&self) -> bool { self.is_runtime || self.is_pointer }

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

  unsafe fn load(&self, compiler: &mut Compiler) -> Self {
    if self.is_pointer && self.can_be_loaded {
      let ty = LLVMTypeOf(self.v);
      let ty = LLVMGetElementType(ty);
      let v = LLVMBuildLoad2(compiler.builder, ty, self.v, compiler.cstring("load"));
      Self {
        v,
        ty: self.ty,
        is_pointer: false,
        can_be_loaded: true,
        is_runtime: true,
        ptr_len: None,
      }
    } else if self.is_runtime && self.can_be_loaded {
      Self {
        v: self.v,
        ty: self.ty,
        is_pointer: false,
        can_be_loaded: true,
        is_runtime: true,
        ptr_len: None,
      }
    } else {
      self.clone()
    }
  }

  fn is_truthy(&self) -> bool {
    unsafe {
      match self.ty {
        ValueType::Integer => self.get_as_integer() == 1,
        ValueType::String => !self.get_as_string().is_empty(),
      }
    }
  }

  unsafe fn new_is_truthy(&self, compiler: &mut Compiler) -> Self {
    if self.is_runtime() {
      let self_ = self.load(compiler);
      match self_.ty {
        ValueType::Integer => {
          let int_ty = LLVMInt64TypeInContext(compiler.ctx);

          let v = LLVMBuildICmp(
            compiler.builder,
            LLVMIntPredicate::LLVMIntEQ,
            self_.v,
            LLVMConstInt(int_ty, 1, 0),
            compiler.cstring(""),
          );
          let v = LLVMBuildIntCast2(compiler.builder, v, int_ty, 0, compiler.cstring(""));
          return ValueWrapper {
            v,
            ty: ValueType::Integer,
            is_pointer: false,
            can_be_loaded: true,
            is_runtime: true,
            ptr_len: None,
          };
        },
        ValueType::String => todo!(),
        #[allow(unreachable_patterns)]
        _ => unreachable!(),
      }
    }
    let v = self.is_truthy() as i64;
    ValueWrapper::new_integer(compiler, v)
  }
}

impl std::fmt::Display for ValueWrapper {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    unsafe {
      if self.is_runtime() {
        unreachable!("Runtime value cannot be formatted.");
      }
      match self.ty {
        ValueType::Integer => write!(f, "{}", self.get_as_integer()),
        ValueType::String => write!(f, "{}", self.get_as_string()),
      }
    }
  }
}

mod utils {
  use super::*;

  pub fn ptr_to_str<'a>(ptr: *const c_char, length: usize) -> Cow<'a, str> {
    if length < 2 {
      return "".into();
    }
    unsafe { CStr::from_ptr(ptr as *const c_char).to_string_lossy() }
  }

  pub(super) fn runtime_string_of(
    self_: &mut Compiler,
    value: ValueWrapper,
  ) -> (LLVMValueRef, LLVMValueRef) {
    unsafe {
      let (int_len_func, int_len_ty) = self_.get_func("%%int_len%%").unwrap();
      let (int_as_str_func, int_as_str_ty) = self_.get_func("%%int_as_str%%").unwrap();
      let int64_type = LLVMInt64TypeInContext(self_.ctx);
      let zero = LLVMConstInt(int64_type, 0, 0);

      match &value.ty {
        ValueType::String => {
          let ty = LLVMTypeOf(value.v);
          if value.is_pointer {
            let len = LLVMBuildSub(
              self_.builder,
              value.ptr_len.unwrap(),
              LLVMConstInt(int64_type, 1, 0),
              self_.cstring(""),
            );
            return (value.v, len);
          }
          let v = self_.alloca_at_entry(value.v);
          let v = LLVMBuildGEP2(
            self_.builder,
            LLVMGetElementType(LLVMTypeOf(v)),
            v,
            [zero, zero].as_mut_ptr(),
            2,
            self_.cstring(""),
          );
          (v, LLVMConstInt(int64_type, (LLVMGetArrayLength(ty) - 1) as u64, 0))
        },
        ValueType::Integer => {
          let orig_len = LLVMBuildCall2(
            self_.builder,
            int_len_ty,
            int_len_func,
            [value.v].as_mut_ptr(),
            1,
            self_.cstring(""),
          );
          let len = LLVMBuildAdd(
            self_.builder,
            orig_len,
            LLVMConstInt(int64_type, 1, 0),
            self_.cstring(""),
          );
          let buf = self_.alloca_str(len);
          LLVMBuildCall2(
            self_.builder,
            int_as_str_ty,
            int_as_str_func,
            [buf, value.v, orig_len].as_mut_ptr(),
            3,
            self_.cstring(""),
          );

          (buf, orig_len)
        },
      }
    }
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
  str_arr_count: usize,

  variables: Variables<ValueWrapper>,
  indent_level: IndentLevel,
}

impl Compiler {
  fn cstring(&mut self, s: &str) -> *const c_char {
    let s = s.to_string();
    if let Some(cstring) = self.cstring_cache.get(&s) {
      return cstring.as_ptr() as *const c_char;
    }

    let cstring = CString::new(s.as_bytes()).unwrap();
    let ptr = cstring.as_ptr() as *const c_char;
    self.cstring_cache.insert(s, cstring);
    ptr
  }

  fn declare_libc_functions(&mut self) {
    unsafe {
      let char_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.ctx), 0);

      let printf_type =
        LLVMFunctionType(LLVMInt32TypeInContext(self.ctx), [char_ptr_ty].as_mut_ptr(), 1, 1);
      let printf_func = LLVMAddFunction(self.module, self.cstring("printf"), printf_type);
      LLVMSetFunctionCallConv(printf_func, LLVMCallConv::LLVMCCallConv as u32);

      let strcpy_type =
        LLVMFunctionType(char_ptr_ty, [char_ptr_ty, char_ptr_ty].as_mut_ptr(), 2, 0);
      let strcpy_func = LLVMAddFunction(self.module, self.cstring("strcpy"), strcpy_type);
      LLVMSetFunctionCallConv(strcpy_func, LLVMCallConv::LLVMCCallConv as u32);

      let strcat_type =
        LLVMFunctionType(char_ptr_ty, [char_ptr_ty, char_ptr_ty].as_mut_ptr(), 2, 0);
      let strcat_func = LLVMAddFunction(self.module, self.cstring("strcat"), strcat_type);
      LLVMSetFunctionCallConv(strcat_func, LLVMCallConv::LLVMCCallConv as u32);
    }
  }

  fn define_helper_functions(&mut self) {
    // %%int_len%% => Gives the length of an integer as a string.
    unsafe {
      let i64_type = LLVMInt64TypeInContext(self.ctx);

      let int_len_type = LLVMFunctionType(i64_type, [i64_type].as_mut_ptr(), 1, 0);
      let int_len_func = LLVMAddFunction(self.module, self.cstring("%%int_len%%"), int_len_type);
      let builder = LLVMCreateBuilderInContext(self.ctx);

      let start_bb = LLVMAppendBasicBlockInContext(self.ctx, int_len_func, self.cstring("start"));
      let loop_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("loop"));
      let ret_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("ret"));

      LLVMPositionBuilderAtEnd(builder, start_bb);
      let orig_int_par = LLVMGetParam(int_len_func, 0);
      let condition = LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntEQ,
        orig_int_par,
        LLVMConstInt(i64_type, 0, 0),
        self.cstring(""),
      );
      LLVMBuildCondBr(builder, condition, ret_bb, loop_bb);

      LLVMAppendExistingBasicBlock(int_len_func, loop_bb);
      LLVMPositionBuilderAtEnd(builder, loop_bb);
      let len = LLVMBuildPhi(builder, i64_type, self.cstring("len"));
      let int_par = LLVMBuildPhi(builder, i64_type, self.cstring("int_par"));

      let new_len = LLVMBuildAdd(builder, len, LLVMConstInt(i64_type, 1, 0), self.cstring(""));
      LLVMAddIncoming(
        len,
        [LLVMConstInt(i64_type, 0, 0), new_len].as_mut_ptr(),
        [start_bb, loop_bb].as_mut_ptr(),
        2,
      );
      let new_int_par =
        LLVMBuildSDiv(builder, int_par, LLVMConstInt(i64_type, 10, 0), self.cstring(""));
      LLVMAddIncoming(
        int_par,
        [orig_int_par, new_int_par].as_mut_ptr(),
        [start_bb, loop_bb].as_mut_ptr(),
        2,
      );

      let condition =
        LLVMBuildAdd(builder, int_par, LLVMConstInt(i64_type, 9, 0), self.cstring(""));
      let condition = LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntULT,
        condition,
        LLVMConstInt(i64_type, 19, 0),
        self.cstring(""),
      );
      LLVMBuildCondBr(builder, condition, ret_bb, loop_bb);

      LLVMAppendExistingBasicBlock(int_len_func, ret_bb);
      LLVMPositionBuilderAtEnd(builder, ret_bb);
      let len = LLVMBuildPhi(builder, i64_type, self.cstring("len"));
      LLVMAddIncoming(
        len,
        [LLVMConstInt(i64_type, 1, 0), new_len].as_mut_ptr(),
        [start_bb, loop_bb].as_mut_ptr(),
        2,
      );
      LLVMBuildRet(builder, len);
    }

    // %%int_as_str%% => Gives the string representation of an integer.
    unsafe {
      let i64_type = LLVMInt64TypeInContext(self.ctx);
      let i8_type = LLVMInt8TypeInContext(self.ctx);

      let int_as_str_type = LLVMFunctionType(
        LLVMVoidTypeInContext(self.ctx),
        [LLVMPointerType(i8_type, 0), i64_type, i64_type].as_mut_ptr(),
        3,
        0,
      );
      let int_as_str_func =
        LLVMAddFunction(self.module, self.cstring("%%int_as_str%%"), int_as_str_type);
      let builder = LLVMCreateBuilderInContext(self.ctx);

      let start_bb =
        LLVMAppendBasicBlockInContext(self.ctx, int_as_str_func, self.cstring("start"));
      let loop_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("loop"));
      let ret_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("ret"));

      LLVMPositionBuilderAtEnd(builder, start_bb);
      let buf = LLVMGetParam(int_as_str_func, 0);
      let orig_int_par = LLVMGetParam(int_as_str_func, 1);
      let len = LLVMGetParam(int_as_str_func, 2);

      LLVMBuildBr(builder, loop_bb);

      LLVMAppendExistingBasicBlock(int_as_str_func, loop_bb);
      LLVMPositionBuilderAtEnd(builder, loop_bb);
      let i = LLVMBuildPhi(builder, i64_type, self.cstring("i"));
      let int_par = LLVMBuildPhi(builder, i64_type, self.cstring("int_par"));

      let ch = LLVMBuildSRem(builder, int_par, LLVMConstInt(i64_type, 10, 0), self.cstring(""));
      let ch = LLVMBuildTrunc(builder, ch, i8_type, self.cstring(""));
      let ch = LLVMBuildAdd(builder, ch, LLVMConstInt(i8_type, 48, 0), self.cstring(""));
      let idx =
        LLVMBuildXor(builder, i, LLVMConstNeg(LLVMConstInt(i64_type, 1, 0)), self.cstring(""));
      let idx = LLVMBuildAdd(builder, idx, len, self.cstring(""));
      let ch_ptr = LLVMBuildGEP2(builder, i8_type, buf, [idx].as_mut_ptr(), 1, self.cstring(""));
      LLVMBuildStore(builder, ch, ch_ptr);

      let new_i = LLVMBuildAdd(builder, i, LLVMConstInt(i64_type, 1, 0), self.cstring("new_i"));
      LLVMAddIncoming(
        i,
        [LLVMConstInt(i64_type, 0, 0), new_i].as_mut_ptr(),
        [start_bb, loop_bb].as_mut_ptr(),
        2,
      );
      let new_int_par =
        LLVMBuildSDiv(builder, int_par, LLVMConstInt(i64_type, 10, 0), self.cstring("new_int_par"));
      LLVMAddIncoming(
        int_par,
        [orig_int_par, new_int_par].as_mut_ptr(),
        [start_bb, loop_bb].as_mut_ptr(),
        2,
      );

      let condition =
        LLVMBuildAdd(builder, int_par, LLVMConstInt(i64_type, 9, 0), self.cstring(""));
      let condition = LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntULT,
        condition,
        LLVMConstInt(i64_type, 19, 0),
        self.cstring(""),
      );
      LLVMBuildCondBr(builder, condition, ret_bb, loop_bb);

      LLVMAppendExistingBasicBlock(int_as_str_func, ret_bb);
      LLVMPositionBuilderAtEnd(builder, ret_bb);

      let i = LLVMBuildAnd(builder, new_i, LLVMConstInt(i64_type, 4294967295, 0), self.cstring(""));
      let ch_ptr = LLVMBuildGEP2(builder, i8_type, buf, [i].as_mut_ptr(), 1, self.cstring(""));
      LLVMBuildStore(builder, LLVMConstInt(i8_type, 0, 0), ch_ptr);
      LLVMBuildRetVoid(builder);
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
      str_arr_count: 0,
    };

    LLVMBuildGlobalString(builder, self_.cstring("%d"), self_.cstring("int_format"));
    LLVMBuildGlobalString(builder, self_.cstring("%s"), self_.cstring("str_format"));
    self_.declare_libc_functions();
    self_.define_helper_functions();

    self_
  }

  fn start_block(&mut self) { self.indent_level += 1; }

  fn end_block(&mut self) {
    self.variables.retain(|_, (level, _)| *level != self.indent_level);
    self.indent_level -= 1;
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

  fn alloca_str(&mut self, size: LLVMValueRef) -> LLVMValueRef {
    unsafe {
      LLVMAddGlobal(
        self.module,
        LLVMTypeOf(size),
        self.cstring(&format!("str_arr_{}_len", self.str_arr_count)),
      );
      LLVMBuildArrayAlloca(
        self.builder,
        LLVMInt8TypeInContext(self.ctx),
        size,
        self.cstring("str_arr"),
      )
    }
  }

  fn alloca_at_entry(&mut self, value: LLVMValueRef) -> LLVMValueRef {
    unsafe {
      let builder = LLVMCreateBuilderInContext(self.ctx);
      let entry = LLVMGetFirstBasicBlock(self.curr_func);

      let first_instr = LLVMGetFirstInstruction(entry);
      if !first_instr.is_null() {
        LLVMPositionBuilderBefore(builder, first_instr);
      } else {
        LLVMPositionBuilderAtEnd(builder, entry);
      }

      let name = self.cstring("");
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
          println!();
          let value = if !value.is_runtime() {
            let value = ValueWrapper::new_string(self, &value.to_string()).v;
            let value_ptr = self.alloca_at_entry(value);
            LLVMBuildGEP2(
              self.builder,
              LLVMGetElementType(LLVMTypeOf(value_ptr)),
              value_ptr,
              [zero].as_mut_ptr(),
              1,
              self.cstring(""),
            )
          } else {
            utils::runtime_string_of(self, value).0
          };

          let format = LLVMGetNamedGlobal(self.module, self.cstring("str_format"));
          let format = LLVMBuildGEP2(
            self.builder,
            LLVMGetElementType(LLVMTypeOf(format)),
            format,
            [zero, zero].as_mut_ptr(),
            2,
            self.cstring(""),
          );
          let (printf_func, printf_ty) = self.get_func("printf").unwrap();
          LLVMBuildCall2(
            self.builder,
            printf_ty,
            printf_func,
            [format, value].as_mut_ptr(),
            2,
            self.cstring(""),
          );
        },
        StmtKind::If { condition, body, else_stmt } => {
          let condition = self.compile_expr(condition).new_is_truthy(self);
          let condition = LLVMBuildICmp(
            self.builder,
            llvm::LLVMIntPredicate::LLVMIntNE,
            condition.v,
            zero,
            self.cstring(""),
          );

          let then_bb =
            LLVMAppendBasicBlockInContext(self.ctx, self.curr_func, self.cstring("then"));
          let else_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("else"));
          let continue_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("continue"));

          LLVMBuildCondBr(self.builder, condition, then_bb, else_bb);
          LLVMPositionBuilderAtEnd(self.builder, then_bb);

          self.start_block();
          for stmt in body {
            self.compile_stmt(stmt);
          }
          self.end_block();

          LLVMBuildBr(self.builder, continue_bb);
          // then_bb = LLVMGetInsertBlock(self.builder);

          LLVMAppendExistingBasicBlock(self.curr_func, else_bb);
          LLVMPositionBuilderAtEnd(self.builder, else_bb);

          self.start_block();
          for stmt in else_stmt {
            self.compile_stmt(stmt);
          }
          self.end_block();

          LLVMBuildBr(self.builder, continue_bb);
          // else_bb = LLVMGetInsertBlock(self.builder);

          LLVMAppendExistingBasicBlock(self.curr_func, continue_bb);
          LLVMPositionBuilderAtEnd(self.builder, continue_bb);
        },
        StmtKind::While { condition, body } => {
          let loop_cond = self.compile_expr(condition).new_is_truthy(self);
          let loop_cond = LLVMBuildICmp(
            self.builder,
            llvm::LLVMIntPredicate::LLVMIntNE,
            loop_cond.v,
            zero,
            self.cstring(""),
          );

          let loop_bb =
            LLVMAppendBasicBlockInContext(self.ctx, self.curr_func, self.cstring("loop"));
          let continue_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("continue"));

          LLVMBuildCondBr(self.builder, loop_cond, loop_bb, continue_bb);
          LLVMPositionBuilderAtEnd(self.builder, loop_bb);

          self.start_block();
          for stmt in body {
            self.compile_stmt(stmt);
          }

          let loop_cond = self.compile_expr(condition).new_is_truthy(self);
          let loop_cond = LLVMBuildICmp(
            self.builder,
            llvm::LLVMIntPredicate::LLVMIntNE,
            loop_cond.v,
            zero,
            self.cstring(""),
          );
          self.end_block();

          LLVMBuildCondBr(self.builder, loop_cond, loop_bb, continue_bb);

          LLVMAppendExistingBasicBlock(self.curr_func, continue_bb);
          LLVMPositionBuilderAtEnd(self.builder, continue_bb);
        },
      }
    }
  }

  fn compile_expr(&mut self, expr: &Expr) -> ValueWrapper {
    unsafe {
      match &expr.kind {
        ExprKind::Integer(n) => ValueWrapper::new_integer(self, *n),
        ExprKind::String(s) => ValueWrapper::new_string(self, s),
        ExprKind::Identifier(name) => match self.variables.get(name) {
          Some((_, val)) => val.clone(),
          None => unreachable!("Resolver didn't resolve variable correctly"),
        },

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
          let new_val = ValueWrapper::new_variable(self, expr.clone());
          match self.variables.get_mut(&name.clone()) {
            Some((_, val)) => {
              if val.ty == expr.ty {
                LLVMBuildStore(self.builder, expr.v, val.v);
              } else {
                *val = new_val;
              }
            },
            None => {
              self.variables.insert(name.clone(), (self.indent_level, new_val));
            },
          }
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
      TokenKind::Less => left.lt(self, right),
      TokenKind::LessEqual => left.le(self, right),
      TokenKind::Greater => left.gt(self, right),
      TokenKind::GreaterEqual => left.ge(self, right),
      TokenKind::EqualEqual => left.eq(self, right),
      TokenKind::BangEqual => left.ne(self, right),

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

    if LLVMTargetMachineEmitToFile(target_machine, self.module, filename, file_type, &mut error) !=
      0
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
