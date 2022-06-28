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
use llvm::transforms::scalar::{
  LLVMAddBasicAliasAnalysisPass,
  LLVMAddCFGSimplificationPass,
  LLVMAddGVNPass,
  LLVMAddInstructionCombiningPass,
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

use crate::parser::syntax::{
  Expr,
  ExprKind,
  Stmt,
  StmtKind,
  TokenKind,
};
use crate::runtime::{
  func_name,
  IndentLevel,
  ValueType,
  Variables,
};
use crate::{
  ignore_return,
  info,
};

#[derive(Debug, Clone, PartialEq)]
struct ValueWrapper {
  v: LLVMValueRef,
  ty: ValueType,
  is_pointer: bool,
  is_runtime: bool,
  can_be_loaded: bool,
}

unsafe impl Send for ValueWrapper {}

impl ValueWrapper {
  unsafe fn new_integer(self_: &mut Compiler, v: i64) -> Self {
    let ty = LLVMInt64TypeInContext(self_.ctx);
    let v = if v > 0 { LLVMConstInt(ty, v as u64, 0) } else { LLVMConstInt(ty, v as u64, 1) };

    Self { v, ty: ValueType::Integer, is_pointer: false, can_be_loaded: true, is_runtime: false }
  }

  unsafe fn new_string(self_: &mut Compiler, v: &str) -> Self {
    let ptr = self_.cstring(v);
    let length = v.len() as u32;
    let v = LLVMConstStringInContext(self_.ctx, ptr, length, 0);

    Self { v, ty: ValueType::String, is_pointer: false, can_be_loaded: true, is_runtime: false }
  }

  unsafe fn new_variable(self_: &mut Compiler, inner: ValueWrapper) -> Self {
    let v = if self_.indent_level == 0 {
      let ty = self_.compile_type(inner.ty.clone());
      let ptr = self_.alloca_global(ty);
      let v = if inner.ty == ValueType::String {
        utils::gep_string_ptr(self_, inner.clone())
      } else {
        inner.v
      };
      LLVMBuildStore(self_.builder, v, ptr);
      ptr
    } else {
      self_.malloc(inner.v)
    };
    Self { v, ty: inner.ty, is_pointer: true, can_be_loaded: true, is_runtime: true }
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
      Self { v, ty: self.ty.clone(), is_pointer: false, can_be_loaded: true, is_runtime: true }
    } else if self.is_runtime && self.can_be_loaded {
      Self {
        v: self.v,
        ty: self.ty.clone(),
        is_pointer: false,
        can_be_loaded: true,
        is_runtime: true,
      }
    } else {
      self.clone()
    }
  }

  fn is_truthy(&self) -> bool {
    unsafe {
      if self.is_runtime() {
        return false;
      }
      match &self.ty {
        ValueType::Void => unreachable!("Void should not be used as a value"),
        ValueType::Integer => self.get_as_integer() == 1,
        ValueType::String => !self.get_as_string().is_empty(),
        ValueType::Array(_, len) => len.kind == ExprKind::Integer(0),
      }
    }
  }

  unsafe fn new_is_truthy(&self, compiler: &mut Compiler) -> Self {
    if self.is_runtime() {
      let i64_ty = LLVMInt64TypeInContext(compiler.ctx);
      match &self.ty {
        ValueType::Void => unreachable!("Void should not be used as a value"),
        ValueType::Integer => {
          let self_ = self.load(compiler);
          let v = LLVMBuildICmp(
            compiler.builder,
            LLVMIntPredicate::LLVMIntEQ,
            self_.v,
            LLVMConstInt(i64_ty, 1, 0),
            compiler.cstring(""),
          );
          let v = LLVMBuildIntCast2(compiler.builder, v, i64_ty, 0, compiler.cstring(""));
          return ValueWrapper {
            v,
            ty: ValueType::Integer,
            is_pointer: false,
            can_be_loaded: true,
            is_runtime: true,
          };
        },
        ValueType::String => {
          let (strlen_func, strlen_ty) = compiler.get_func("strlen").unwrap();
          let self_ = utils::gep_string_ptr(compiler, self.clone());

          let str_len = LLVMBuildCall2(
            compiler.builder,
            strlen_ty,
            strlen_func,
            [self_].as_mut_ptr(),
            1,
            compiler.cstring(""),
          );
          let v = LLVMBuildICmp(
            compiler.builder,
            LLVMIntPredicate::LLVMIntNE,
            str_len,
            LLVMConstInt(i64_ty, 0, 0),
            compiler.cstring(""),
          );
          let v = LLVMBuildIntCast2(compiler.builder, v, i64_ty, 0, compiler.cstring(""));
          return ValueWrapper {
            v,
            ty: ValueType::Integer,
            is_pointer: false,
            can_be_loaded: true,
            is_runtime: true,
          };
        },
        ValueType::Array(_, len) => {
          return ValueWrapper::new_integer(compiler, (len.kind == ExprKind::Integer(0)) as i64)
        },
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
        ValueType::Void => unreachable!("Void should not be used as a value"),
        ValueType::Integer => write!(f, "{}", self.get_as_integer()),
        ValueType::String => write!(f, "{}", self.get_as_string()),
        ValueType::Array(_, _) => unreachable!("Array are and should always be runtime values"),
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
    compiler: &mut Compiler,
    value: ValueWrapper,
  ) -> (LLVMValueRef, LLVMValueRef) {
    unsafe {
      let (int_len_func, int_len_ty) = compiler.get_func("%%int_len%%").unwrap();
      let (int_as_str_func, int_as_str_ty) = compiler.get_func("%%int_as_str%%").unwrap();
      let (strlen_func, strlen_ty) = compiler.get_func("strlen").unwrap();
      let i64_type = LLVMInt64TypeInContext(compiler.ctx);

      match &value.ty {
        ValueType::Void => unreachable!("Void should not be used as a value"),
        ValueType::String => {
          let v = utils::gep_string_ptr(compiler, value);
          let ptr_len = LLVMBuildCall2(
            compiler.builder,
            strlen_ty,
            strlen_func,
            [v].as_mut_ptr(),
            1,
            compiler.cstring(""),
          );
          let len = LLVMBuildSub(
            compiler.builder,
            ptr_len,
            LLVMConstInt(i64_type, 1, 0),
            compiler.cstring(""),
          );
          (v, len)
        },
        ValueType::Integer => {
          let orig_len = LLVMBuildCall2(
            compiler.builder,
            int_len_ty,
            int_len_func,
            [value.v].as_mut_ptr(),
            1,
            compiler.cstring(""),
          );
          let len = LLVMBuildAdd(
            compiler.builder,
            orig_len,
            LLVMConstInt(i64_type, 1, 0),
            compiler.cstring(""),
          );
          let buf = compiler.malloc_str(len);
          LLVMBuildCall2(
            compiler.builder,
            int_as_str_ty,
            int_as_str_func,
            [buf, value.v, orig_len].as_mut_ptr(),
            3,
            compiler.cstring(""),
          );

          (buf, orig_len)
        },
        // TODO: add support for arrays
        ValueType::Array(_, _) => {
          unreachable!("Resolver didn't resolve array to string conversion")
        },
      }
    }
  }

  pub(super) fn gep_string_ptr(compiler: &mut Compiler, value: ValueWrapper) -> LLVMValueRef {
    unsafe { gep_array_ptr(compiler, LLVMInt8TypeInContext(compiler.ctx), value) }
  }

  pub fn gep_string_ptr_raw(compiler: &mut Compiler, value: LLVMValueRef) -> LLVMValueRef {
    unsafe { gep_array_ptr_raw(compiler, LLVMInt8TypeInContext(compiler.ctx), value) }
  }

  pub(super) fn gep_array_ptr(
    compiler: &mut Compiler,
    ty: LLVMTypeRef,
    value: ValueWrapper,
  ) -> LLVMValueRef {
    unsafe {
      let value = value.load(compiler);
      if LLVMTypeOf(value.v) == LLVMPointerType(ty, 0) {
        return value.v;
      }
      let value = if !value.is_pointer { compiler.malloc(value.v) } else { value.v };
      utils::gep_array_ptr_raw(compiler, ty, value)
    }
  }

  pub fn gep_array_ptr_raw(
    compiler: &mut Compiler,
    ty: LLVMTypeRef,
    value: LLVMValueRef,
  ) -> LLVMValueRef {
    unsafe {
      if LLVMTypeOf(value) == LLVMPointerType(ty, 0) {
        return value;
      }
      let zero = LLVMConstInt(LLVMInt64TypeInContext(compiler.ctx), 0, 0);
      LLVMBuildGEP2(
        compiler.builder,
        LLVMGetElementType(LLVMTypeOf(value)),
        value,
        [zero, zero].as_mut_ptr(),
        2,
        compiler.cstring(""),
      )
    }
  }
}

type VarWithName = (String, ValueWrapper);

#[derive(Clone)]
struct FuncDesc {
  arg_len: usize,
  // We store the return type for type checking
  ret_ty: Option<ValueType>,
  local_vars: Vec<VarWithName>,
}

impl FuncDesc {
  fn new_non_closure(arg_len: usize, ret_ty: Option<ValueType>) -> Self {
    FuncDesc { arg_len, ret_ty, local_vars: Vec::new() }
  }
}

pub struct Compiler {
  pub ctx: LLVMContextRef,
  pub module: LLVMModuleRef,
  pub builder: LLVMBuilderRef,
  pub fpm: LLVMPassManagerRef,

  cstring_cache: HashMap<String, CString>,
  // main_func: LLVMValueRef,
  curr_func: LLVMValueRef,

  // We store all functions for optimizing later
  funcs: Vec<LLVMValueRef>,

  variables: Variables<ValueWrapper>,
  function_descs: Variables<FuncDesc>,
  indent_level: IndentLevel,

  // Since functions can return nothing, we need to track whether are we ignoring the return value.
  ignore_return: bool,
  break_label: Option<LLVMBasicBlockRef>,
}

impl Compiler {
  pub unsafe fn define_std(&mut self) {
    // `print` function
    {
      let str_format = LLVMGetNamedGlobal(self.module, self.cstring("%%str_format%%"));
      let str_format = utils::gep_string_ptr_raw(self, str_format);
      let int_format = LLVMGetNamedGlobal(self.module, self.cstring("%%int_format%%"));
      let int_format = utils::gep_string_ptr_raw(self, int_format);

      let void_type = LLVMVoidTypeInContext(self.ctx);
      let i64_type = LLVMInt64TypeInContext(self.ctx);

      let (printf_func, printf_ty) = self.get_func("printf").unwrap();

      {
        // Define a function inside llvm
        let print_string_type = LLVMFunctionType(
          void_type,
          [LLVMPointerType(LLVMInt8TypeInContext(self.ctx), 0)].as_mut_ptr(),
          1,
          0,
        );
        let print_string_func =
          LLVMAddFunction(self.module, self.cstring("print.string"), print_string_type);

        let entry_bb =
          LLVMAppendBasicBlockInContext(self.ctx, print_string_func, self.cstring("entry"));
        let builder = LLVMCreateBuilderInContext(self.ctx);

        LLVMPositionBuilderAtEnd(builder, entry_bb);

        let expr = LLVMGetParam(print_string_func, 0);
        LLVMBuildCall2(
          builder,
          printf_ty,
          printf_func,
          [str_format, expr].as_mut_ptr(),
          2,
          self.cstring(""),
        );

        LLVMBuildRetVoid(builder);

        // Declare a function inside our own compiler
        self.funcs.push(print_string_func);
        self.function_descs.declare("print.string", 0, FuncDesc::new_non_closure(1, None));
      }

      {
        // Define a function inside llvm
        let print_int_type = LLVMFunctionType(void_type, [i64_type].as_mut_ptr(), 1, 0);
        let print_int_func =
          LLVMAddFunction(self.module, self.cstring("print.int"), print_int_type);

        let entry_bb =
          LLVMAppendBasicBlockInContext(self.ctx, print_int_func, self.cstring("entry"));
        let builder = LLVMCreateBuilderInContext(self.ctx);

        LLVMPositionBuilderAtEnd(builder, entry_bb);

        let expr = LLVMGetParam(print_int_func, 0);
        LLVMBuildCall2(
          builder,
          printf_ty,
          printf_func,
          [int_format, expr].as_mut_ptr(),
          2,
          self.cstring(""),
        );

        LLVMBuildRetVoid(builder);

        // Declare a function inside our own compiler
        self.funcs.push(print_int_func);
        self.function_descs.declare("print.int", 1, FuncDesc::new_non_closure(1, None));
      }
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

    // LLVMAddAggressiveDCEPass(fpm);
    // // LLVMAddArgumentPromotionPass(fpm);
    // LLVMAddBasicAliasAnalysisPass(fpm);
    // LLVMAddConstantMergePass(fpm);
    // LLVMAddCFGSimplificationPass(fpm);
    // LLVMAddDCEPass(fpm);
    // LLVMAddDeadStoreEliminationPass(fpm);
    // // LLVMAddFunctionAttrsPass(fpm);
    // LLVMAddGVNPass(fpm);
    // LLVMAddIndVarSimplifyPass(fpm);
    // LLVMAddInstructionCombiningPass(fpm);
    // LLVMAddAggressiveInstCombinerPass(fpm);
    // LLVMAddJumpThreadingPass(fpm);
    // LLVMAddLoopUnrollPass(fpm);
    // LLVMAddMemCpyOptPass(fpm);
    // LLVMAddPromoteMemoryToRegisterPass(fpm);
    // LLVMAddReassociatePass(fpm);

    // First, combine combinable instrs
    LLVMAddInstructionCombiningPass(fpm);
    // Second, reassociate commutative exprs to better constant folding
    LLVMAddReassociatePass(fpm);
    // Third, perform global value numbering
    LLVMAddGVNPass(fpm);
    // Fourth, perform basic block optimizations
    LLVMAddCFGSimplificationPass(fpm);
    // Fifth, perform basic alias analysis
    LLVMAddBasicAliasAnalysisPass(fpm);
    // Sixth, promotes args by reference to by value
    LLVMAddPromoteMemoryToRegisterPass(fpm);
    // Seventh, combines combinable instrs again
    LLVMAddInstructionCombiningPass(fpm);
    // Eighth, perform global value numbering
    LLVMAddReassociatePass(fpm);

    LLVMInitializeFunctionPassManager(fpm);

    let func_ty = LLVMFunctionType(LLVMInt32TypeInContext(ctx), std::ptr::null_mut(), 0, 0);
    let cstring = CString::new("main").unwrap();
    let ptr = cstring.as_ptr();
    cstring_cache.insert("main".to_string(), cstring);
    let func = LLVMAddFunction(module, ptr, func_ty);

    let mut self_ = Compiler {
      ctx,
      module,
      builder,
      fpm,
      cstring_cache,
      funcs: Vec::new(),
      variables: Variables::new(),
      function_descs: Variables::new(),
      indent_level: 0,
      // main_func: func,
      curr_func: func,
      ignore_return: false,
      break_label: None,
    };

    self_.funcs.push(func);
    self_.append_entry_bb();

    LLVMBuildGlobalString(builder, self_.cstring("%d"), self_.cstring("%%int_format%%"));
    LLVMBuildGlobalString(builder, self_.cstring("%s"), self_.cstring("%%str_format%%"));
    self_.declare_libc_functions();
    self_.define_helper_functions();
    self_.declare_llvm_functions();

    self_
  }

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

      let strlen_type =
        LLVMFunctionType(LLVMInt64TypeInContext(self.ctx), [char_ptr_ty].as_mut_ptr(), 1, 0);
      let strlen_func = LLVMAddFunction(self.module, self.cstring("strlen"), strlen_type);
      LLVMSetFunctionCallConv(strlen_func, LLVMCallConv::LLVMCCallConv as u32);

      let strcmp_type = LLVMFunctionType(
        LLVMInt32TypeInContext(self.ctx),
        [char_ptr_ty, char_ptr_ty].as_mut_ptr(),
        2,
        0,
      );
      let strcmp_func = LLVMAddFunction(self.module, self.cstring("strcmp"), strcmp_type);
      LLVMSetFunctionCallConv(strcmp_func, LLVMCallConv::LLVMCCallConv as u32);

      let memcmp_type = LLVMFunctionType(
        LLVMInt32TypeInContext(self.ctx),
        [char_ptr_ty, char_ptr_ty, LLVMInt64TypeInContext(self.ctx)].as_mut_ptr(),
        3,
        0,
      );
      let memcmp_func = LLVMAddFunction(self.module, self.cstring("memcmp"), memcmp_type);
      LLVMSetFunctionCallConv(memcmp_func, LLVMCallConv::LLVMCCallConv as u32);
    }
  }

  fn define_helper_functions(&mut self) {
    // %%int_len%% => Gives the length of an integer as a string.
    unsafe {
      let i64_type = LLVMInt64TypeInContext(self.ctx);

      let int_len_type = LLVMFunctionType(i64_type, [i64_type].as_mut_ptr(), 1, 0);
      let int_len_func = LLVMAddFunction(self.module, self.cstring("%%int_len%%"), int_len_type);
      self.funcs.push(int_len_func);
      let builder = LLVMCreateBuilderInContext(self.ctx);

      let entry_bb = LLVMAppendBasicBlockInContext(self.ctx, int_len_func, self.cstring("entry"));
      let abs_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("abs"));
      let loop_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("loop"));
      let ret_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("ret"));

      LLVMPositionBuilderAtEnd(builder, entry_bb);
      let orig_val = LLVMGetParam(int_len_func, 0);
      let condition = LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntSLE,
        orig_val,
        LLVMConstInt(i64_type, 0, 0),
        self.cstring(""),
      );
      LLVMBuildCondBr(builder, condition, abs_bb, loop_bb);

      LLVMAppendExistingBasicBlock(int_len_func, abs_bb);
      LLVMPositionBuilderAtEnd(builder, abs_bb);

      let abs_val = LLVMBuildNeg(builder, orig_val, self.cstring(""));
      let condition = LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntEQ,
        abs_val,
        LLVMConstInt(i64_type, 0, 0),
        self.cstring(""),
      );
      LLVMBuildCondBr(builder, condition, ret_bb, loop_bb);

      LLVMAppendExistingBasicBlock(int_len_func, loop_bb);
      LLVMPositionBuilderAtEnd(builder, loop_bb);
      let len = LLVMBuildPhi(builder, i64_type, self.cstring("len"));
      let val = LLVMBuildPhi(builder, i64_type, self.cstring("val"));

      let new_len = LLVMBuildAdd(builder, len, LLVMConstInt(i64_type, 1, 0), self.cstring(""));
      LLVMAddIncoming(
        len,
        [LLVMConstInt(i64_type, 0, 0), LLVMConstInt(i64_type, 1, 0), new_len].as_mut_ptr(),
        [entry_bb, abs_bb, loop_bb].as_mut_ptr(),
        3,
      );
      let new_val = LLVMBuildSDiv(builder, val, LLVMConstInt(i64_type, 10, 0), self.cstring(""));
      LLVMAddIncoming(
        val,
        [orig_val, abs_val, new_val].as_mut_ptr(),
        [entry_bb, abs_bb, loop_bb].as_mut_ptr(),
        3,
      );

      let condition = LLVMBuildAdd(builder, val, LLVMConstInt(i64_type, 9, 0), self.cstring(""));
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
        [abs_bb, loop_bb].as_mut_ptr(),
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
      self.funcs.push(int_as_str_func);
      let builder = LLVMCreateBuilderInContext(self.ctx);

      let entry_bb =
        LLVMAppendBasicBlockInContext(self.ctx, int_as_str_func, self.cstring("entry"));
      let abs_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("abs"));
      let loop_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("loop"));
      let ret_bb = LLVMCreateBasicBlockInContext(self.ctx, self.cstring("ret"));

      LLVMPositionBuilderAtEnd(builder, entry_bb);
      let orig_buf = LLVMGetParam(int_as_str_func, 0);
      let orig_val = LLVMGetParam(int_as_str_func, 1);
      let orig_len = LLVMGetParam(int_as_str_func, 2);

      let condition = LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntSLT,
        orig_val,
        LLVMConstInt(i64_type, 0, 0),
        self.cstring(""),
      );

      LLVMBuildCondBr(builder, condition, abs_bb, loop_bb);

      LLVMAppendExistingBasicBlock(int_as_str_func, abs_bb);
      LLVMPositionBuilderAtEnd(builder, abs_bb);

      let ch_ptr = LLVMBuildGEP2(
        builder,
        i8_type,
        orig_buf,
        [LLVMConstInt(i64_type, 0, 0)].as_mut_ptr(),
        1,
        self.cstring(""),
      );
      LLVMBuildStore(builder, LLVMConstInt(i8_type, 45, 0), ch_ptr);
      let abs_buf = LLVMBuildGEP2(
        builder,
        i8_type,
        orig_buf,
        [LLVMConstInt(i64_type, 1, 0)].as_mut_ptr(),
        1,
        self.cstring(""),
      );
      let abs_val = LLVMBuildNeg(builder, orig_val, self.cstring(""));
      let abs_len = LLVMBuildSub(builder, orig_len, LLVMConstInt(i64_type, 1, 0), self.cstring(""));

      LLVMBuildBr(builder, loop_bb);

      LLVMAppendExistingBasicBlock(int_as_str_func, loop_bb);
      LLVMPositionBuilderAtEnd(builder, loop_bb);
      let i = LLVMBuildPhi(builder, i64_type, self.cstring("i"));
      let val = LLVMBuildPhi(builder, i64_type, self.cstring("val"));
      let len = LLVMBuildPhi(builder, i64_type, self.cstring("len"));
      LLVMAddIncoming(
        len,
        [orig_len, abs_len, len].as_mut_ptr(),
        [entry_bb, abs_bb, loop_bb].as_mut_ptr(),
        3,
      );
      let buf = LLVMBuildPhi(builder, LLVMPointerType(i8_type, 0), self.cstring("buf"));
      LLVMAddIncoming(
        buf,
        [orig_buf, abs_buf, buf].as_mut_ptr(),
        [entry_bb, abs_bb, loop_bb].as_mut_ptr(),
        3,
      );

      let ch = LLVMBuildSRem(builder, val, LLVMConstInt(i64_type, 10, 0), self.cstring(""));
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
        [LLVMConstInt(i64_type, 0, 0), LLVMConstInt(i64_type, 0, 0), new_i].as_mut_ptr(),
        [entry_bb, abs_bb, loop_bb].as_mut_ptr(),
        3,
      );
      let new_val =
        LLVMBuildSDiv(builder, val, LLVMConstInt(i64_type, 10, 0), self.cstring("new_int_par"));
      LLVMAddIncoming(
        val,
        [orig_val, abs_val, new_val].as_mut_ptr(),
        [entry_bb, abs_bb, loop_bb].as_mut_ptr(),
        3,
      );

      let condition = LLVMBuildAdd(builder, val, LLVMConstInt(i64_type, 9, 0), self.cstring(""));
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

  fn declare_llvm_functions(&mut self) {
    unsafe {
      let i64_type = LLVMInt64TypeInContext(self.ctx);

      let powi_i64_i64_type = LLVMFunctionType(i64_type, [i64_type, i64_type].as_mut_ptr(), 2, 0);
      let powi_i64_i64_func =
        LLVMAddFunction(self.module, self.cstring("llvm.powi.i64.i64"), powi_i64_i64_type);
      LLVMSetFunctionCallConv(powi_i64_i64_func, LLVMCallConv::LLVMCCallConv as u32);

      let memcpy_p0i8_p0i8_i64_type = LLVMFunctionType(
        LLVMVoidTypeInContext(self.ctx),
        [
          LLVMPointerType(LLVMInt8TypeInContext(self.ctx), 0),
          LLVMPointerType(LLVMInt8TypeInContext(self.ctx), 0),
          i64_type,
          LLVMInt1TypeInContext(self.ctx),
        ]
        .as_mut_ptr(),
        4,
        0,
      );
      let memcpy_p0i8_p0i8_i64_func = LLVMAddFunction(
        self.module,
        self.cstring("llvm.memcpy.p0i8.p0i8.i64"),
        memcpy_p0i8_p0i8_i64_type,
      );
      LLVMSetFunctionCallConv(memcpy_p0i8_p0i8_i64_func, LLVMCallConv::LLVMCCallConv as u32);
    }
  }

  fn start_block(&mut self) { self.indent_level += 1; }

  fn end_block(&mut self) {
    self.variables.remove_all_with_indent(self.indent_level);
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

  fn malloc_str(&mut self, size: LLVMValueRef) -> LLVMValueRef {
    self.malloc_str_at(self.builder, size)
  }

  fn malloc_str_at(&mut self, builder: LLVMBuilderRef, size: LLVMValueRef) -> LLVMValueRef {
    unsafe {
      LLVMBuildArrayMalloc(builder, LLVMInt8TypeInContext(self.ctx), size, self.cstring("str_arr"))
    }
  }

  fn malloc(&mut self, value: LLVMValueRef) -> LLVMValueRef {
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
      let value_ptr = LLVMBuildMalloc(self.builder, LLVMTypeOf(value), name);
      LLVMBuildStore(self.builder, value, value_ptr);
      value_ptr
    }
  }

  fn alloca_global(&mut self, ty: LLVMTypeRef) -> LLVMValueRef {
    unsafe {
      let name = self.cstring("");
      let global = LLVMAddGlobal(self.module, ty, name);
      LLVMSetInitializer(global, LLVMGetUndef(ty));
      global
    }
  }

  fn compile_type(&mut self, ty: ValueType) -> LLVMTypeRef {
    unsafe {
      match ty {
        ValueType::Void => LLVMVoidTypeInContext(self.ctx),
        ValueType::Integer => LLVMInt64TypeInContext(self.ctx),
        ValueType::String => LLVMPointerType(LLVMInt8TypeInContext(self.ctx), 0),
        ValueType::Array(ty, _) => LLVMPointerType(self.compile_type(*ty), 0),
      }
    }
  }

  fn append_entry_bb(&mut self) {
    unsafe {
      let entry_bb = LLVMAppendBasicBlockInContext(self.ctx, self.curr_func, self.cstring("entry"));
      LLVMAppendBasicBlockInContext(self.ctx, self.curr_func, self.cstring("unused"));
      LLVMPositionBuilderAtEnd(self.builder, entry_bb);
    }
  }

  fn get_unused_bb(&mut self) -> LLVMBasicBlockRef {
    unsafe {
      let bb = LLVMGetNextBasicBlock(LLVMGetFirstBasicBlock(self.curr_func));
      let name = CStr::from_ptr(LLVMGetBasicBlockName(bb)).to_str().unwrap();
      assert_eq!(name, "unused", "Current function's first block is not `unused`");
      bb
    }
  }

  // Gets all local variables of a function used in another function
  // TODO: make this better
  fn func_get_vars(&mut self, declared: Vec<String>, stmts: &[Stmt]) -> Vec<VarWithName> {
    fn expr_as_var(self_: &mut Compiler, expr: &Expr) -> Vec<VarWithName> {
      let names = match &expr.kind {
        ExprKind::Array(_, exprs, _) => {
          exprs.iter().flat_map(|expr| expr_as_var(self_, expr)).map(|(name, _)| name).collect()
        },
        ExprKind::Identifier(name) => vec![name.to_string()],
        ExprKind::PrefixOp { right, .. } => {
          expr_as_var(self_, right).into_iter().map(|(name, _)| name).collect()
        },
        ExprKind::InfixOp { left, right, .. } => {
          let mut out = expr_as_var(self_, left);
          out.extend(expr_as_var(self_, right));
          out.into_iter().map(|(name, _)| name).collect()
        },
        ExprKind::ShortCircuitOp { left, right, .. } => {
          let mut out = expr_as_var(self_, left);
          out.extend(expr_as_var(self_, right));
          out.into_iter().map(|(name, _)| name).collect()
        },
        ExprKind::FuncCall { params, .. } => {
          let mut out = vec![];
          for param in params {
            out.extend(expr_as_var(self_, param));
          }
          out.into_iter().map(|(name, _)| name).collect()
        },

        _ => vec![],
      };

      names
        .into_iter()
        .map(|name| (name.clone(), self_.variables.get_variable(&name)))
        .filter_map(|(name, var)| {
          if let Some((_, indent_level, _)) = var {
            if indent_level < self_.indent_level && indent_level != 0 {
              return Some((name, var.unwrap().2));
            }
          }

          None
        })
        .collect()
    }

    fn stmt_as_var(self_: &mut Compiler, declared: &[String], stmt: &Stmt) -> Vec<VarWithName> {
      let names = match &stmt.kind {
        StmtKind::Expression { expr } => expr_as_var(self_, expr),
        StmtKind::If { condition, body, else_stmt } => {
          let mut out = expr_as_var(self_, condition);
          out.extend(stmts_as_var(self_, vec![], body));
          out.extend(stmts_as_var(self_, vec![], else_stmt));
          out
        },
        StmtKind::While { condition, body } => {
          let mut out = expr_as_var(self_, condition);
          out.extend(stmts_as_var(self_, vec![], body));
          out
        },
        StmtKind::FuncDef { args, body, .. } => stmts_as_var(
          self_,
          args.iter().map(|(name, _)| name.to_string()).collect::<Vec<_>>(),
          body,
        ),

        #[allow(unreachable_patterns)]
        _ => vec![],
      };

      names
        .into_iter()
        .map(|(name, _)| (name.clone(), self_.variables.get_variable(&name)))
        .filter_map(|(name, var)| {
          if let Some((_, indent_level, _)) = var {
            if indent_level < self_.indent_level && indent_level != 0 && !declared.contains(&name) {
              return Some((name, var.unwrap().2));
            }
          }

          None
        })
        .collect()
    }

    fn stmts_as_var(
      self_: &mut Compiler,
      declared: Vec<String>,
      stmts: &[Stmt],
    ) -> Vec<VarWithName> {
      let mut out = vec![];
      for stmt in stmts {
        out.extend(stmt_as_var(self_, &declared, stmt));
      }
      out
    }

    stmts_as_var(self, declared, stmts)
  }

  fn compile_stmt(&mut self, stmt: &Stmt) {
    unsafe {
      let zero = LLVMConstInt(LLVMInt64TypeInContext(self.ctx), 0, 0);

      match &stmt.kind {
        StmtKind::Expression { expr } => {
          ignore_return!(self, expr, self.compile_expr(expr));
        },
        StmtKind::If { condition, body, else_stmt } => {
          let condition =
            ignore_return!(NEVER_WITH_RET; self, condition, self.compile_expr(condition))
              .new_is_truthy(self);
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
          let loop_cond =
            ignore_return!(NEVER_WITH_RET; self, condition, self.compile_expr(condition))
              .new_is_truthy(self);
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

          let prev_break_label = self.break_label;
          self.break_label = Some(continue_bb);

          LLVMBuildCondBr(self.builder, loop_cond, loop_bb, continue_bb);
          LLVMPositionBuilderAtEnd(self.builder, loop_bb);

          self.start_block();
          for stmt in body {
            self.compile_stmt(stmt);
          }

          let loop_cond =
            ignore_return!(NEVER_WITH_RET; self, condition, self.compile_expr(condition))
              .new_is_truthy(self);
          let loop_cond = LLVMBuildICmp(
            self.builder,
            llvm::LLVMIntPredicate::LLVMIntNE,
            loop_cond.v,
            zero,
            self.cstring(""),
          );
          self.end_block();

          self.break_label = prev_break_label;

          LLVMBuildCondBr(self.builder, loop_cond, loop_bb, continue_bb);

          LLVMAppendExistingBasicBlock(self.curr_func, continue_bb);
          LLVMPositionBuilderAtEnd(self.builder, continue_bb);
        },
        StmtKind::FuncDef { name, args, body, ret_ty } => {
          let arg_types = args.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>();
          let func_name = func_name(name, &arg_types);

          let args_len = args.len();
          self.indent_level += 1;
          let local_vars =
            self.func_get_vars(args.iter().map(|(name, _)| name.clone()).collect(), body);
          self.indent_level -= 1;
          let mut arg_types =
            arg_types.iter().map(|ty| self.compile_type(ty.clone())).collect::<Vec<_>>();
          arg_types.extend(local_vars.iter().map(|(_, val)| {
            if val.is_pointer {
              LLVMPointerType(self.compile_type(val.ty.clone()), 0)
            } else {
              self.compile_type(val.ty.clone())
            }
          }));

          let func_type = LLVMFunctionType(
            if let Some(ret_type) = ret_ty.clone() {
              self.compile_type(ret_type)
            } else {
              LLVMVoidTypeInContext(self.ctx)
            },
            arg_types.as_mut_ptr(),
            arg_types.len() as u32,
            0,
          );
          let func = LLVMAddFunction(self.module, self.cstring(&func_name), func_type);
          self.funcs.push(func);
          self.function_descs.declare(
            &func_name,
            self.indent_level,
            FuncDesc { arg_len: args_len, ret_ty: ret_ty.clone(), local_vars: local_vars.clone() },
          );

          let prev_builder = self.builder;
          let prev_func = self.curr_func;

          self.builder = LLVMCreateBuilderInContext(self.ctx);
          self.curr_func = func;

          self.append_entry_bb();

          self.start_block();

          for (i, (arg, ty)) in args.iter().enumerate() {
            let arg_ptr = LLVMGetParam(func, i as u32);
            let arg_val = ValueWrapper {
              v: arg_ptr,
              ty: ty.clone(),
              can_be_loaded: false,
              is_pointer: false,
              is_runtime: true,
            };
            self.variables.declare(arg, self.indent_level, arg_val);
          }
          for (i, (name, var)) in local_vars.iter().enumerate() {
            let var_ptr = LLVMGetParam(func, args_len as u32 + i as u32);
            let var_val = ValueWrapper {
              v: var_ptr,
              ty: var.ty.clone(),
              can_be_loaded: var.can_be_loaded,
              is_pointer: true,
              is_runtime: true,
            };
            self.variables.declare(name, self.indent_level, var_val);
          }

          for stmt in body {
            self.compile_stmt(stmt);
          }

          if ret_ty.is_none() {
            LLVMBuildRetVoid(self.builder);
          } else {
            LLVMBuildUnreachable(self.builder);
          }

          LLVMDeleteBasicBlock(self.get_unused_bb());

          self.end_block();

          self.builder = prev_builder;
          self.curr_func = prev_func;
        },
        StmtKind::Ret { expr } => {
          let unused_bb = self.get_unused_bb();

          if let Some(expr) = expr {
            let expr = ignore_return!(NEVER_WITH_RET; self, expr, self.compile_expr(expr));
            if expr.ty == ValueType::String {
              let expr = expr.load(self);
              let v = utils::gep_string_ptr(self, expr);
              let ret_val = self.malloc(v);
              let ret_val = LLVMBuildLoad2(self.builder, LLVMTypeOf(v), ret_val, self.cstring(""));

              LLVMBuildRet(self.builder, ret_val);
            } else {
              LLVMBuildRet(self.builder, expr.load(self).v);
            }
          } else {
            LLVMBuildRetVoid(self.builder);
          }

          LLVMPositionBuilderAtEnd(self.builder, unused_bb);
        },
        StmtKind::Break => {
          if let Some(break_label) = self.break_label {
            LLVMBuildBr(self.builder, break_label);
            LLVMPositionBuilderAtEnd(self.builder, self.get_unused_bb());
          } else {
            unreachable!("Resolver didn't catch break stmt outside of loop");
          }
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
          Some(val) => val.load(self),
          None => unreachable!("Resolver didn't resolve variable correctly"),
        },
        ExprKind::Array(ty, elems, len) => {
          let i64_type = LLVMInt64TypeInContext(self.ctx);

          let len_expr = len.clone().unwrap_or_else(|| {
            rc!(Expr::new(ExprKind::Integer(elems.len() as i64), expr.span.clone()))
          });
          let len_val = self.compile_expr(len_expr.as_ref());

          let runtime_ty = self.compile_type(ty.clone());
          let arr = LLVMBuildArrayMalloc(self.builder, runtime_ty, len_val.v, self.cstring(""));

          for (i, expr) in elems.iter().enumerate() {
            let val = self.compile_expr(expr);
            let gep = LLVMBuildGEP2(
              self.builder,
              LLVMGetElementType(LLVMTypeOf(arr)),
              arr,
              [LLVMConstInt(i64_type, i as u64, 0)].as_mut_ptr(),
              1,
              self.cstring(""),
            );
            let v = if val.ty == ValueType::String {
              utils::gep_string_ptr(self, val.clone())
            } else {
              val.v
            };
            LLVMBuildStore(self.builder, v, gep);
          }

          let elems_len = LLVMConstInt(i64_type, elems.len() as u64, 0);

          if len_val.is_runtime() {
            let loop_cond = LLVMBuildICmp(
              self.builder,
              LLVMIntPredicate::LLVMIntSLT,
              elems_len,
              len_val.v,
              self.cstring(""),
            );

            let prev_bb = LLVMGetInsertBlock(self.builder);
            let loop_bb = LLVMAppendBasicBlockInContext(
              self.ctx,
              self.curr_func,
              self.cstring("array_init_loop"),
            );
            let continue_bb =
              LLVMCreateBasicBlockInContext(self.ctx, self.cstring("array_init_continue"));

            LLVMBuildCondBr(self.builder, loop_cond, loop_bb, continue_bb);
            LLVMPositionBuilderAtEnd(self.builder, loop_bb);

            let i = LLVMBuildPhi(self.builder, i64_type, self.cstring(""));

            let gep = LLVMBuildGEP2(
              self.builder,
              LLVMGetElementType(LLVMTypeOf(arr)),
              arr,
              [i].as_mut_ptr(),
              1,
              self.cstring(""),
            );

            let val_index = LLVMBuildSRem(self.builder, i, elems_len, self.cstring(""));
            let val = LLVMBuildGEP2(
              self.builder,
              LLVMGetElementType(LLVMTypeOf(arr)),
              arr,
              [val_index].as_mut_ptr(),
              1,
              self.cstring(""),
            );
            let val = LLVMBuildLoad2(
              self.builder,
              LLVMGetElementType(LLVMTypeOf(val)),
              val,
              self.cstring(""),
            );
            let v =
              if ty == &ValueType::String { utils::gep_string_ptr_raw(self, val) } else { val };

            LLVMBuildStore(self.builder, v, gep);

            let new_i =
              LLVMBuildAdd(self.builder, i, LLVMConstInt(i64_type, 1, 0), self.cstring(""));
            LLVMAddIncoming(i, [elems_len, new_i].as_mut_ptr(), [prev_bb, loop_bb].as_mut_ptr(), 2);

            let loop_cond = LLVMBuildICmp(
              self.builder,
              LLVMIntPredicate::LLVMIntSLT,
              i,
              len_val.v,
              self.cstring(""),
            );

            LLVMBuildCondBr(self.builder, loop_cond, loop_bb, continue_bb);

            LLVMAppendExistingBasicBlock(self.curr_func, continue_bb);
            LLVMPositionBuilderAtEnd(self.builder, continue_bb);
          } else {
            let len = len_val.get_as_integer();

            for i in 0..len as usize {
              let i = LLVMConstInt(i64_type, i as u64, 0);

              let gep = LLVMBuildGEP2(
                self.builder,
                LLVMGetElementType(LLVMTypeOf(arr)),
                arr,
                [i].as_mut_ptr(),
                1,
                self.cstring(""),
              );

              let val_index = LLVMBuildSRem(self.builder, i, elems_len, self.cstring(""));
              let val = LLVMBuildGEP2(
                self.builder,
                LLVMGetElementType(LLVMTypeOf(arr)),
                arr,
                [val_index].as_mut_ptr(),
                1,
                self.cstring(""),
              );
              let val = LLVMBuildLoad2(
                self.builder,
                LLVMGetElementType(LLVMTypeOf(val)),
                val,
                self.cstring(""),
              );

              LLVMBuildStore(self.builder, val, gep);
            }
          }

          ValueWrapper {
            v: arr,
            ty: ValueType::Array(bx!(ty.clone()), len_expr),
            can_be_loaded: false,
            is_pointer: true,
            is_runtime: true,
          }
        },

        ExprKind::PrefixOp { op, right } => {
          let right = self.compile_expr(right);
          self.compile_prefix_op(op, right)
        },
        ExprKind::InfixOp { op, left, right } => {
          if *op == TokenKind::Equal {
            return self.compile_assignment(left, right);
          }
          let left = self.compile_expr(left);
          let right = self.compile_expr(right);
          self.compile_infix_op(op, left, right)
        },
        ExprKind::ShortCircuitOp { op, left, right } => {
          self.compile_short_circuit_op(op, left, right)
        },
        ExprKind::FuncCall { name, params } => {
          let params =
            params.iter().map(|expr| self.compile_expr(expr).load(self)).collect::<Vec<_>>();
          let param_types = params.iter().map(|val| val.ty.clone()).collect::<Vec<_>>();

          let func_name = func_name(name, &param_types);
          let FuncDesc { arg_len, ret_ty, local_vars } = match self.function_descs.get(&func_name) {
            Some(func_desc) => func_desc,
            None => unreachable!("Resolver didn't resolve function correctly"),
          };

          if params.len() != arg_len {
            unreachable!("Function call has wrong number of arguments");
          }

          let (func, func_ty) = self.get_func(&func_name).unwrap();
          let mut params = params
            .into_iter()
            .map(|expr| {
              if expr.ty == ValueType::String {
                return utils::gep_string_ptr(self, expr);
              }
              expr.load(self).v
            })
            .collect::<Vec<_>>();
          params.extend(local_vars.iter().map(|(_, var)| {
            if var.ty == ValueType::String {
              return utils::gep_string_ptr(self, var.clone());
            }
            var.v
          }));

          let v = LLVMBuildCall2(
            self.builder,
            func_ty,
            func,
            params.as_mut_ptr(),
            params.len() as u32,
            self.cstring(""),
          );

          match LLVMTypeOf(v) != LLVMVoidTypeInContext(self.ctx) {
            true => ValueWrapper {
              v,
              ty: ret_ty.unwrap(),
              can_be_loaded: false,
              is_pointer: false,
              is_runtime: true,
            },
            false if self.ignore_return => ValueWrapper::new_integer(self, 1656),
            false => unreachable!("Function call has no return type"),
          }
        },
        ExprKind::Index { array, index } => {
          let array = self.compile_expr(array);
          let index = self.compile_expr(index);
          let (out, is_array) = self.compile_index(array, index);
          let mut out = out.load(self);

          if !is_array && out.ty == ValueType::String {
            out.v = {
              let array_ptr = LLVMBuildLoad2(
                self.builder,
                LLVMGetElementType(LLVMTypeOf(out.v)),
                out.v,
                self.cstring(""),
              );

              let out = self.malloc_str(LLVMConstInt(LLVMInt32TypeInContext(self.ctx), 2, 0));
              LLVMBuildStore(self.builder, array_ptr, out);
              let null_ptr = LLVMBuildGEP2(
                self.builder,
                LLVMGetElementType(LLVMTypeOf(out)),
                out,
                [LLVMConstInt(LLVMInt32TypeInContext(self.ctx), 1, 0)].as_mut_ptr(),
                1,
                self.cstring(""),
              );
              LLVMBuildStore(
                self.builder,
                LLVMConstInt(LLVMInt8TypeInContext(self.ctx), 0, 0),
                null_ptr,
              );
              out
            };
          }

          out
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
      TokenKind::Caret => left.pow(self, right),
      TokenKind::Percent => left.mod_(self, right),
      TokenKind::LeftShift => left.shl(self, right),
      TokenKind::RightShift => left.shr(self, right),
      TokenKind::Ampersand => left.band(self, right),
      TokenKind::Pipe => left.bor(self, right),

      _ => unreachable!("{op} is not an infix operator"),
    }
  }

  fn compile_short_circuit_op(
    &mut self,
    op: &TokenKind,
    left: &Expr,
    right: &Expr,
  ) -> ValueWrapper {
    match op {
      TokenKind::And => ValueWrapper::and(self, left, right),
      TokenKind::Or => ValueWrapper::or(self, left, right),

      _ => unreachable!("{op} is not a short circuit operator"),
    }
  }

  //
  // indexed     is_array
  fn compile_index(&mut self, array: ValueWrapper, index: ValueWrapper) -> (ValueWrapper, bool) {
    unsafe {
      let array_ty = array.ty.clone();
      let index_ty = index.ty.clone();

      let index = index.load(self);

      match (&array_ty, &index_ty) {
        (ValueType::Array(ty, _), ValueType::Integer) => {
          let array_ptr = LLVMBuildGEP2(
            self.builder,
            LLVMGetElementType(LLVMTypeOf(array.v)),
            array.v,
            [index.v].as_mut_ptr(),
            1,
            self.cstring(""),
          );

          (
            ValueWrapper {
              v: array_ptr,
              ty: *ty.clone(),
              can_be_loaded: true,
              is_pointer: true,
              is_runtime: true,
            },
            true,
          )
        },
        // TODO: allow for slicing of strings
        (ValueType::String, ValueType::Integer) => {
          let array_ptr = utils::gep_string_ptr(self, array);
          let array_ptr = LLVMBuildGEP2(
            self.builder,
            LLVMGetElementType(LLVMTypeOf(array_ptr)),
            array_ptr,
            [index.v].as_mut_ptr(),
            1,
            self.cstring(""),
          );

          (
            ValueWrapper {
              v: array_ptr,
              ty: array_ty.clone(),
              can_be_loaded: false,
              is_pointer: true,
              is_runtime: true,
            },
            false,
          )
        },

        _ => unreachable!("Resolver didn't resolve indexing correctly"),
      }
    }
  }

  fn compile_assignment(&mut self, left: &Expr, right: &Expr) -> ValueWrapper {
    unsafe {
      let right = self.compile_expr(right).load(self).load(self);
      let right_v = if right.ty == ValueType::String {
        utils::gep_string_ptr(self, right.clone())
      } else {
        right.v
      };

      match &left.kind {
        ExprKind::Identifier(name) => {
          match self.variables.get_mut(&name.clone()) {
            // Since strings have a variable size, we just overwrite the value
            Some(val) if val.ty == right.ty && val.ty != ValueType::String => {
              LLVMBuildStore(self.builder, right_v, val.v);
            },
            _ => {
              let new_val = ValueWrapper::new_variable(self, right.clone());
              self.variables.assign_or_declare(name, self.indent_level, new_val);
            },
          }
        },
        ExprKind::Index { array, index } => {
          let array = self.compile_expr(array);
          let array_ty = array.ty.clone();

          let index = self.compile_expr(index);
          let (lhs, _) = self.compile_index(array, index);

          match array_ty {
            ValueType::Array(_, _) => {
              LLVMBuildStore(self.builder, right_v, lhs.v);
            },
            ValueType::String => {
              let (memcpy_func, memcpy_ty) = self.get_func("llvm.memcpy.p0i8.p0i8.i64").unwrap();
              let (strlen_func, strlen_ty) = self.get_func("strlen").unwrap();

              let len = LLVMBuildCall2(
                self.builder,
                strlen_ty,
                strlen_func,
                [right_v].as_mut_ptr(),
                1,
                self.cstring(""),
              );

              LLVMBuildCall2(
                self.builder,
                memcpy_ty,
                memcpy_func,
                [lhs.v, right_v, len, LLVMConstInt(LLVMInt1TypeInContext(self.ctx), 0, 0)]
                  .as_mut_ptr(),
                4,
                self.cstring(""),
              );
            },

            _ => unreachable!("Resolver didn't resolve indexing correctly"),
          }
        },

        _ => unreachable!("Resolver didn't resolve assignment correctly"),
      }

      right
    }
  }

  unsafe fn finish(&mut self, stmts: &[Stmt]) {
    for stmt in stmts {
      self.compile_stmt(stmt);
    }

    LLVMDeleteBasicBlock(self.get_unused_bb());

    LLVMBuildRet(self.builder, LLVMConstInt(LLVMInt32TypeInContext(self.ctx), 0, 0));
    for func in &self.funcs {
      LLVMVerifyFunction(*func, LLVMVerifierFailureAction::LLVMAbortProcessAction);
      LLVMRunFunctionPassManager(self.fpm, *func);
    }

    #[cfg(debug_assertions)]
    {
      LLVMDumpModule(self.module);
      LLVMPrintModuleToFile(self.module, self.cstring("tmp.ll"), ptr::null_mut());
    }

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
