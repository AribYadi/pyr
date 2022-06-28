use llvm::LLVMIntPredicate;

use super::*;

macro_rules! impl_arithmetics_for_runtime {
  (
    $compiler:ident, $self:ident, $other:ident;
    $($pattern:pat $(if $condition:expr)? => $pattern_out:expr;)*
  ) => {
    if $self.is_runtime() || $other.is_runtime() {
      let self_ = $self.load($compiler);
      let other_ = $other.load($compiler);
      #[allow(clippy::redundant_closure_call)]
      #[allow(unreachable_patterns)]
      match (self_.ty.clone(), other_.ty.clone()) {
        $($pattern $(if $condition)? => {
          let (v, ty, is_pointer, can_be_loaded) = ($pattern_out)(self_, other_);
          return Self { v, ty, is_pointer, can_be_loaded, is_runtime: true }
        },)*
        _ => (),
      }
    }
  };
}

impl ValueWrapper {
  pub fn add(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| (LLVMBuildAdd(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer, false, true);
        (ValueType::String, ValueType::Array(_, _)) | (ValueType::Array(_, _), ValueType::String) => |_, _| unreachable!("Resolver didn't type check infix operator `+`");
        (ValueType::String, _) | (_, ValueType::String) => |left: Self, right: Self| {
          let (left, left_len) = utils::runtime_string_of(compiler, left);
          let (right, right_len) = utils::runtime_string_of(compiler, right);

          let len = LLVMBuildAdd(compiler.builder, left_len, right_len, compiler.cstring(""));
          let len = LLVMBuildAdd(compiler.builder, len, LLVMConstInt(LLVMInt64TypeInContext(compiler.ctx), 1, 0), compiler.cstring(""));
          let buf = compiler.malloc_str(len);

          let (strcpy_func, strcpy_ty) = compiler.get_func("strcpy").unwrap();
          let (strcat_func, strcat_ty) = compiler.get_func("strcat").unwrap();
          LLVMBuildCall2(
            compiler.builder,
            strcpy_ty,
            strcpy_func,
            [buf, left].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          LLVMBuildCall2(
            compiler.builder,
            strcat_ty,
            strcat_func,
            [buf, right].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          let len = LLVMBuildAdd(compiler.builder, len, LLVMConstInt(LLVMInt64TypeInContext(compiler.ctx), 1, 0), compiler.cstring(""));
          let null_term = LLVMBuildGEP2(
            compiler.builder,
            LLVMInt8TypeInContext(compiler.ctx),
            buf,
            [len].as_mut_ptr(),
            1,
            compiler.cstring(""),
          );
          LLVMBuildStore(compiler.builder, LLVMConstInt(LLVMInt8TypeInContext(compiler.ctx), 0, 0), null_term);

          (buf, ValueType::String, true, false)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() + other.get_as_integer())
        },
        (ValueType::String, _) | (_, ValueType::String) => {
          Self::new_string(compiler, &[self.to_string(), other.to_string()].concat())
        },
        #[allow(unreachable_patterns)]
        _ => unreachable!("Resolver didn't type check infix operator `+`"),
      }
    }
  }

  pub fn sub(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| (LLVMBuildSub(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer, false, true);
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() - other.get_as_integer())
        },
        _ => unreachable!("Resolver didn't type check infix operator `-`"),
      }
    }
  }

  pub fn mul(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| (LLVMBuildMul(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer, false, true);
        (ValueType::String, ValueType::Integer) | (ValueType::Integer, ValueType::String) => |left: Self, right: Self| {
          let i64_type = LLVMInt64TypeInContext(compiler.ctx);

          let (left, left_len) = utils::runtime_string_of(compiler, left);

          let len = LLVMBuildMul(compiler.builder, left_len, right.v, compiler.cstring(""));
          let buf = compiler.malloc_str(len);

          let (strcpy_func, strcpy_ty) = compiler.get_func("strcpy").unwrap();
          let (strcat_func, strcat_ty) = compiler.get_func("strcat").unwrap();

          let start_bb = LLVMAppendBasicBlockInContext(compiler.ctx, compiler.curr_func, compiler.cstring("mul_start"));
          let loop_bb = LLVMCreateBasicBlockInContext(compiler.ctx, compiler.cstring("mul_loop"));
          let done_bb = LLVMCreateBasicBlockInContext(compiler.ctx, compiler.cstring("mul_done"));

          let condition = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntNE, right.v, LLVMConstInt(i64_type, 0, 0), compiler.cstring(""));
          LLVMBuildCondBr(compiler.builder, condition, start_bb, done_bb);

          LLVMPositionBuilderAtEnd(compiler.builder, start_bb);
          LLVMBuildCall2(
            compiler.builder,
            strcpy_ty,
            strcpy_func,
            [buf, left].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          let condition = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntNE, right.v, LLVMConstInt(i64_type, 1, 0), compiler.cstring(""));
          LLVMBuildCondBr(compiler.builder, condition, loop_bb, done_bb);

          LLVMAppendExistingBasicBlock(compiler.curr_func, loop_bb);
          LLVMPositionBuilderAtEnd(compiler.builder, loop_bb);
          let i = LLVMBuildPhi(compiler.builder, i64_type, compiler.cstring("i"));
          LLVMBuildCall2(
            compiler.builder,
            strcat_ty,
            strcat_func,
            [buf, left].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          let new_i = LLVMBuildAdd(compiler.builder, i, LLVMConstInt(i64_type, 1, 0), compiler.cstring("new_i"));
          LLVMAddIncoming(
            i,
            [LLVMConstInt(i64_type, 1, 0), new_i].as_mut_ptr(),
            [start_bb, loop_bb].as_mut_ptr(),
            2,
          );
          let condition = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntULT, new_i, right.v, compiler.cstring(""));
          LLVMBuildCondBr(compiler.builder, condition, loop_bb, done_bb);

          LLVMAppendExistingBasicBlock(compiler.curr_func, done_bb);
          LLVMPositionBuilderAtEnd(compiler.builder, done_bb);

          let len = LLVMBuildAdd(compiler.builder, len, LLVMConstInt(LLVMInt64TypeInContext(compiler.ctx), 1, 0), compiler.cstring(""));
          let null_term = LLVMBuildGEP2(
            compiler.builder,
            LLVMInt8TypeInContext(compiler.ctx),
            buf,
            [len].as_mut_ptr(),
            1,
            compiler.cstring(""),
          );
          LLVMBuildStore(compiler.builder, LLVMConstInt(LLVMInt8TypeInContext(compiler.ctx), 0, 0), null_term);

          (buf, ValueType::String, true, false)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() * other.get_as_integer())
        },
        (ValueType::String, ValueType::Integer) | (ValueType::Integer, ValueType::String) => {
          let mut string = String::new();
          if self.ty == ValueType::String {
            for _ in 0..other.get_as_integer() {
              string.push_str(&self.get_as_string());
            }
          } else {
            for _ in 0..self.get_as_integer() {
              string.push_str(&other.get_as_string());
            }
          }
          Self::new_string(compiler, &string)
        },
        _ => unreachable!("Resolver didn't type check infix operator `*`"),
      }
    }
  }

  pub fn div(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| (LLVMBuildSDiv(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer, false, true);
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() / other.get_as_integer())
        },
        _ => unreachable!("Resolver didn't type check infix operator `/`"),
      }
    }
  }

  pub fn neg(self, compiler: &mut Compiler) -> Self {
    unsafe {
      if self.is_runtime() {
        let self_ = self.load(compiler);
        let v = LLVMBuildNeg(compiler.builder, self_.v, compiler.cstring(""));
        return Self {
          v,
          ty: ValueType::Integer,
          is_pointer: false,
          can_be_loaded: true,
          is_runtime: true,
        };
      };

      match self.ty {
        ValueType::Integer => Self::new_integer(compiler, -self.get_as_integer()),
        _ => unreachable!("Resolver didn't type check prefix operator `-`"),
      }
    }
  }

  pub fn not(self, compiler: &mut Compiler) -> Self {
    unsafe {
      if self.is_runtime() {
        let self_ = self.new_is_truthy(compiler);
        let v = LLVMBuildIntCast2(
          compiler.builder,
          self_.v,
          LLVMInt1TypeInContext(compiler.ctx),
          0,
          compiler.cstring(""),
        );
        let v = LLVMBuildNot(compiler.builder, v, compiler.cstring(""));
        let v = LLVMBuildIntCast2(
          compiler.builder,
          v,
          LLVMInt64TypeInContext(compiler.ctx),
          0,
          compiler.cstring(""),
        );
        return Self {
          v,
          ty: ValueType::Integer,
          is_pointer: false,
          can_be_loaded: true,
          is_runtime: true,
        };
      };

      ValueWrapper::new_integer(compiler, !self.is_truthy() as i64)
    }
  }

  pub fn lt(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntSLT, self_.v, other_.v, compiler.cstring(""));
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, (self.get_as_integer() < other.get_as_integer()) as i64)
        },
        _ => unreachable!("Resolver didn't type check infix operator `<`"),
      }
    }
  }

  pub fn le(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntSLE, self_.v, other_.v, compiler.cstring(""));
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, (self.get_as_integer() <= other.get_as_integer()) as i64)
        },
        _ => unreachable!("Resolver didn't type check infix operator `<=`"),
      }
    }
  }

  pub fn gt(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntSGT, self_.v, other_.v, compiler.cstring(""));
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, (self.get_as_integer() > other.get_as_integer()) as i64)
        },
        _ => unreachable!("Resolver didn't type check infix operator `>`"),
      }
    }
  }

  pub fn ge(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntSGE, self_.v, other_.v, compiler.cstring(""));
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, (self.get_as_integer() >= other.get_as_integer()) as i64)
        },
        _ => unreachable!("Resolver didn't type check infix operator `>=`"),
      }
    }
  }

  pub fn eq(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntEQ, self_.v, other_.v, compiler.cstring(""));
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true)
        };
        (ValueType::String, ValueType::String) => |_, _| {
          let self_ = utils::gep_string_ptr(compiler, self);
          let other_ = utils::gep_string_ptr(compiler, other);

          let (strcmp_func, strcmp_ty) = compiler.get_func("strcmp").unwrap();
          let match_ = LLVMBuildCall2(
            compiler.builder,
            strcmp_ty,
            strcmp_func,
            [self_, other_].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntEQ, match_, LLVMConstInt(LLVMInt32TypeInContext(compiler.ctx), 0, 0), compiler.cstring(""));
          let v = LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring(""));
          (v, ValueType::Integer, false, true)
        };
        (ValueType::Array(ty1, len1), ValueType::Array(ty2, len2)) if len1 == len2 && ty1 == ty2 => |_, _| {
          let ty1 = compiler.compile_type(*ty1.clone());
          let self_ = utils::gep_array_ptr(compiler, ty1, self);
          let ty2 = compiler.compile_type(*ty2.clone());
          let other_ = utils::gep_array_ptr(compiler, ty2, other);

          let (memcmp_func, memcmp_ty) = compiler.get_func("memcmp").unwrap();
          let match_ = LLVMBuildCall2(
            compiler.builder,
            memcmp_ty,
            memcmp_func,
            [self_, other_].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntEQ, match_, LLVMConstInt(LLVMInt32TypeInContext(compiler.ctx), 0, 0), compiler.cstring(""));
          let v = LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring(""));
          (v, ValueType::Integer, false, true)
        };
        _ => |_, _| (LLVMConstInt(LLVMInt64TypeInContext(compiler.ctx), 0, 0), ValueType::Integer, false, true);
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, (self.get_as_integer() == other.get_as_integer()) as i64)
        },
        (ValueType::String, ValueType::String) => {
          Self::new_integer(compiler, (self.get_as_string() == other.get_as_string()) as i64)
        },
        (ValueType::Array(_, _), _) | (_, ValueType::Array(_, _)) => {
          unreachable!("Array are always runtime values")
        },
        _ => Self::new_integer(compiler, 0),
      }
    }
  }

  pub fn ne(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntNE, self_.v, other_.v, compiler.cstring(""));
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true)
        };
        (ValueType::String, ValueType::String) => |_, _| {
          let self_ = utils::gep_string_ptr(compiler, self);
          let other_ = utils::gep_string_ptr(compiler, other);

          let (strcmp_func, strcmp_ty) = compiler.get_func("strcmp").unwrap();
          let match_ = LLVMBuildCall2(
            compiler.builder,
            strcmp_ty,
            strcmp_func,
            [self_, other_].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntNE, match_, LLVMConstInt(LLVMInt32TypeInContext(compiler.ctx), 0, 0), compiler.cstring(""));
          let v = LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring(""));
          (v, ValueType::Integer, false, true)
        };
        (ValueType::Array(ty1, len1), ValueType::Array(ty2, len2)) if len1 == len2 && ty1 == ty2 => |_, _| {
          let ty1 = compiler.compile_type(*ty1.clone());
          let self_ = utils::gep_array_ptr(compiler, ty1, self);
          let ty2 = compiler.compile_type(*ty2.clone());
          let other_ = utils::gep_array_ptr(compiler, ty2, other);

          let (memcmp_func, memcmp_ty) = compiler.get_func("memcmp").unwrap();
          let match_ = LLVMBuildCall2(
            compiler.builder,
            memcmp_ty,
            memcmp_func,
            [self_, other_].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntNE, match_, LLVMConstInt(LLVMInt32TypeInContext(compiler.ctx), 0, 0), compiler.cstring(""));
          let v = LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring(""));
          (v, ValueType::Integer, false, true)
        };
        _ => |_, _| (LLVMConstInt(LLVMInt64TypeInContext(compiler.ctx), 1, 0), ValueType::Integer, false, true);
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, (self.get_as_integer() != other.get_as_integer()) as i64)
        },
        (ValueType::String, ValueType::String) => {
          Self::new_integer(compiler, (self.get_as_string() != other.get_as_string()) as i64)
        },
        (ValueType::Array(_, _), _) | (_, ValueType::Array(_, _)) => {
          unreachable!("Array are always runtime values")
        },
        _ => Self::new_integer(compiler, 1),
      }
    }
  }

  pub fn and(compiler: &mut Compiler, self_expr: &Expr, other_expr: &Expr) -> Self {
    unsafe {
      let i64_type = LLVMInt64TypeInContext(compiler.ctx);
      let mut left = compiler.compile_expr(self_expr).load(compiler);
      if left.ty == ValueType::String {
        // Turn string(array) into string(pointers)
        left = Self {
          v: utils::runtime_string_of(compiler, left.clone()).0,
          ty: ValueType::String,
          is_pointer: true,
          is_runtime: true,
          can_be_loaded: false,
        };
      }

      let start_bb = LLVMGetInsertBlock(compiler.builder);
      let right_bb = LLVMCreateBasicBlockInContext(compiler.ctx, compiler.cstring("and_right"));
      let continue_bb =
        LLVMCreateBasicBlockInContext(compiler.ctx, compiler.cstring("and_continue"));

      let condition = left.new_is_truthy(compiler).v;
      let condition = LLVMBuildICmp(
        compiler.builder,
        LLVMIntPredicate::LLVMIntEQ,
        condition,
        LLVMConstInt(i64_type, 1, 0),
        compiler.cstring(""),
      );
      LLVMBuildCondBr(compiler.builder, condition, right_bb, continue_bb);

      LLVMAppendExistingBasicBlock(compiler.curr_func, right_bb);
      LLVMPositionBuilderAtEnd(compiler.builder, right_bb);

      let mut right = compiler.compile_expr(other_expr).load(compiler);
      if right.ty == ValueType::String {
        // Turn string(array) into string(pointers)
        right = Self {
          v: utils::runtime_string_of(compiler, right.clone()).0,
          ty: ValueType::String,
          is_pointer: true,
          is_runtime: true,
          can_be_loaded: false,
        };
      }
      if left.ty != right.ty {
        right = right.new_is_truthy(compiler);
      }

      LLVMBuildBr(compiler.builder, continue_bb);

      LLVMAppendExistingBasicBlock(compiler.curr_func, continue_bb);
      LLVMPositionBuilderAtEnd(compiler.builder, continue_bb);

      let out_ty = match (&left.ty, &right.ty) {
        (ValueType::Integer, ValueType::Integer) => ValueType::Integer,
        (ValueType::String, ValueType::String) => ValueType::String,
        (ValueType::Array(ty1, len1), ValueType::Array(ty2, len2))
          if len1 == len2 && ty1 == ty2 =>
        {
          ValueType::Array(ty1.clone(), len1.clone())
        },
        _ => ValueType::Integer,
      };
      let v_ty = compiler.compile_type(out_ty.clone());
      let v_value = if left.ty == right.ty {
        let v_value = LLVMBuildPhi(compiler.builder, v_ty, compiler.cstring(""));
        LLVMAddIncoming(
          v_value,
          [left.v, right.v].as_mut_ptr(),
          [start_bb, right_bb].as_mut_ptr(),
          2,
        );
        v_value
      } else {
        let v_value = LLVMBuildPhi(compiler.builder, v_ty, compiler.cstring(""));
        LLVMAddIncoming(
          v_value,
          [LLVMConstInt(i64_type, 0, 0), right.v].as_mut_ptr(),
          [start_bb, right_bb].as_mut_ptr(),
          2,
        );
        v_value
      };

      let v = if matches!(out_ty, ValueType::String | ValueType::Array(_, _)) {
        v_value
      } else {
        compiler.malloc(v_value)
      };

      Self {
        v,
        can_be_loaded: matches!(out_ty, ValueType::Integer),
        ty: out_ty,
        is_pointer: true,
        is_runtime: true,
      }
    }
  }

  pub fn or(compiler: &mut Compiler, self_expr: &Expr, other_expr: &Expr) -> Self {
    unsafe {
      let i64_type = LLVMInt64TypeInContext(compiler.ctx);
      let mut left = compiler.compile_expr(self_expr).load(compiler);
      if left.ty == ValueType::String {
        // Turn string(array) into string(pointers)
        left = Self {
          v: utils::runtime_string_of(compiler, left.clone()).0,
          ty: ValueType::String,
          is_pointer: true,
          is_runtime: true,
          can_be_loaded: false,
        };
      }

      let start_bb = LLVMGetInsertBlock(compiler.builder);
      let right_bb = LLVMCreateBasicBlockInContext(compiler.ctx, compiler.cstring("or_right"));
      let continue_bb =
        LLVMCreateBasicBlockInContext(compiler.ctx, compiler.cstring("or_continue"));

      let condition = left.new_is_truthy(compiler).v;
      let condition = LLVMBuildICmp(
        compiler.builder,
        LLVMIntPredicate::LLVMIntEQ,
        condition,
        LLVMConstInt(i64_type, 0, 0),
        compiler.cstring(""),
      );
      LLVMBuildCondBr(compiler.builder, condition, right_bb, continue_bb);

      LLVMAppendExistingBasicBlock(compiler.curr_func, right_bb);
      LLVMPositionBuilderAtEnd(compiler.builder, right_bb);

      let mut right = compiler.compile_expr(other_expr).load(compiler);
      if right.ty == ValueType::String {
        // Turn string(array) into string(pointers)
        right = Self {
          v: utils::runtime_string_of(compiler, right.clone()).0,
          ty: ValueType::String,
          is_pointer: true,
          is_runtime: true,
          can_be_loaded: false,
        };
      }
      if left.ty != right.ty {
        left = left.new_is_truthy(compiler);
        right = right.new_is_truthy(compiler);
      }

      LLVMBuildBr(compiler.builder, continue_bb);

      LLVMAppendExistingBasicBlock(compiler.curr_func, continue_bb);
      LLVMPositionBuilderAtEnd(compiler.builder, continue_bb);

      let out_ty = match (&left.ty, &right.ty) {
        (ValueType::Integer, ValueType::Integer) => ValueType::Integer,
        (ValueType::String, ValueType::String) => ValueType::String,
        (ValueType::Array(ty1, len1), ValueType::Array(ty2, len2))
          if len1 == len2 && ty1 == ty2 =>
        {
          ValueType::Array(ty1.clone(), len1.clone())
        },
        _ => ValueType::Integer,
      };
      let v_ty = compiler.compile_type(out_ty.clone());
      let v_value = if left.ty == right.ty {
        let v_value = LLVMBuildPhi(compiler.builder, v_ty, compiler.cstring(""));
        LLVMAddIncoming(
          v_value,
          [left.v, right.v].as_mut_ptr(),
          [start_bb, right_bb].as_mut_ptr(),
          2,
        );
        v_value
      } else {
        let v_value = LLVMBuildPhi(compiler.builder, v_ty, compiler.cstring(""));
        LLVMAddIncoming(
          v_value,
          [LLVMConstInt(i64_type, 0, 0), right.v].as_mut_ptr(),
          [start_bb, right_bb].as_mut_ptr(),
          2,
        );
        v_value
      };

      let v = if matches!(out_ty, ValueType::String | ValueType::Array(_, _)) {
        v_value
      } else {
        compiler.malloc(v_value)
      };

      Self {
        v,
        can_be_loaded: matches!(out_ty, ValueType::Integer),
        ty: out_ty,
        is_pointer: true,
        is_runtime: true,
      }
    }
  }

  pub fn pow(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let (powi_func, powi_ty) = compiler.get_func("llvm.powi.i64.i64").unwrap();
          let v = LLVMBuildCall2(
            compiler.builder,
            powi_ty,
            powi_func,
            [self_.v, other_.v].as_mut_ptr(),
            2,
            compiler.cstring(""),
          );
          (v, ValueType::Integer, false, false)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer().pow(other.get_as_integer() as u32))
        },

        _ => unreachable!("Resolver didn't type check infix operator `^`"),
      }
    }
  }

  pub fn mod_(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildSRem(compiler.builder, self_.v, other_.v, compiler.cstring(""));
          (v, ValueType::Integer, false, false)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() % other.get_as_integer())
        },

        _ => unreachable!("Resolver didn't type check infix operator `%`"),
      }
    }
  }

  pub fn shl(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildShl(compiler.builder, self_.v, other_.v, compiler.cstring(""));
          (v, ValueType::Integer, false, false)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() << other.get_as_integer())
        },

        _ => unreachable!("Resolver didn't type check infix operator `<<`"),
      }
    }
  }

  pub fn shr(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildAShr(compiler.builder, self_.v, other_.v, compiler.cstring(""));
          (v, ValueType::Integer, false, false)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() >> other.get_as_integer())
        },

        _ => unreachable!("Resolver didn't type check infix operator `>>`"),
      }
    }
  }

  pub fn band(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildAnd(compiler.builder, self_.v, other_.v, compiler.cstring(""));
          (v, ValueType::Integer, false, false)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() & other.get_as_integer())
        },

        _ => unreachable!("Resolver didn't type check infix operator `&`"),
      }
    }
  }

  pub fn bor(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildOr(compiler.builder, self_.v, other_.v, compiler.cstring(""));
          (v, ValueType::Integer, false, false)
        };
      };

      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() | other.get_as_integer())
        },

        _ => unreachable!("Resolver didn't type check infix operator `|`"),
      }
    }
  }
}
