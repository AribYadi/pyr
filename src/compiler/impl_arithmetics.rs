use llvm::LLVMIntPredicate;

use super::*;

macro_rules! impl_arithmetics_for_runtime {
  (
    $compiler:ident, $self:ident, $other:ident;
    $($pattern:pat => $pattern_out:expr;)*
  ) => {
    if $self.is_runtime() || $other.is_runtime() {
      let self_ = $self.load($compiler);
      let other_ = $other.load($compiler);
      #[allow(clippy::redundant_closure_call)]
      #[allow(unreachable_patterns)]
      match (&self_.ty, &other_.ty) {
        $($pattern => {
          let (v, ty, is_pointer, can_be_loaded, ptr_len) = ($pattern_out)(self_, other_);
          return Self { v, ty, is_pointer, can_be_loaded, is_runtime: true, ptr_len }
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
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| (LLVMBuildAdd(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer, false, true, None);
        (ValueType::String, _) | (_, ValueType::String) => |left: Self, right: Self| {
          let (left, left_len) = utils::runtime_string_of(compiler, left);
          let (right, right_len) = utils::runtime_string_of(compiler, right);

          let len = LLVMBuildAdd(compiler.builder, left_len, right_len, compiler.cstring(""));
          let len = LLVMBuildAdd(compiler.builder, len, LLVMConstInt(LLVMInt64TypeInContext(compiler.ctx), 1, 0), compiler.cstring(""));
          let buf = compiler.alloca_str(len);

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
          let null_term = LLVMBuildGEP2(
            compiler.builder,
            LLVMInt8TypeInContext(compiler.ctx),
            buf,
            [len].as_mut_ptr(),
            1,
            compiler.cstring(""),
          );
          LLVMBuildStore(compiler.builder, LLVMConstInt(LLVMInt8TypeInContext(compiler.ctx), 0, 0), null_term);

          (buf, ValueType::String, true, false, Some(len))
        };
      }
      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, self.get_as_integer() + other.get_as_integer())
        },
        (ValueType::String, _) | (_, ValueType::String) => {
          Self::new_string(compiler, &format!("{self}{other}"))
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
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| (LLVMBuildSub(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer, false, true, None);
      }
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
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| (LLVMBuildMul(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer, false, true, None);
        (ValueType::String, ValueType::Integer) | (ValueType::Integer, ValueType::String) => |left: Self, right: Self| {
          let i64_type = LLVMInt64TypeInContext(compiler.ctx);

          let (left, left_len) = utils::runtime_string_of(compiler, left);

          let len = LLVMBuildMul(compiler.builder, left_len, right.v, compiler.cstring(""));
          let buf = compiler.alloca_str(len);

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

          let null_term = LLVMBuildGEP2(
            compiler.builder,
            LLVMInt8TypeInContext(compiler.ctx),
            buf,
            [len].as_mut_ptr(),
            1,
            compiler.cstring(""),
          );
          LLVMBuildStore(compiler.builder, LLVMConstInt(LLVMInt8TypeInContext(compiler.ctx), 0, 0), null_term);

          (buf, ValueType::String, true, false, Some(len))
        };
      }
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
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| (LLVMBuildSDiv(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer, false, true, None);
      }
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
          ptr_len: None,
        };
      }
      match self.ty {
        ValueType::Integer => Self::new_integer(compiler, -self.get_as_integer()),
        _ => unreachable!("Resolver didn't type check prefix operator `-`"),
      }
    }
  }

  pub fn not(self, compiler: &mut Compiler) -> Self {
    unsafe {
      if self.is_runtime() {
        let self_ = self.load(compiler);
        match self_.ty {
          ValueType::Integer => {
            let v = LLVMBuildICmp(
              compiler.builder,
              LLVMIntPredicate::LLVMIntNE,
              self_.v,
              LLVMConstInt(LLVMInt64TypeInContext(compiler.ctx), 1, 0),
              compiler.cstring(""),
            );
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
      ValueWrapper::new_integer(compiler, !self.is_truthy() as i64)
    }
  }

  pub fn lt(self, compiler: &mut Compiler, other: Self) -> Self {
    unsafe {
      impl_arithmetics_for_runtime! {
        compiler, self, other;
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| {
          let v = LLVMBuildICmp(compiler.builder, LLVMIntPredicate::LLVMIntSLT, self_.v, other_.v, compiler.cstring(""));
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true, None)
        };
      }
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true, None)
        };
      }
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true, None)
        };
      }
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true, None)
        };
      }
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true, None)
        };
        (ValueType::String, ValueType::String) => |_, _| todo!();
      }
      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, (self.get_as_integer() == other.get_as_integer()) as i64)
        },
        (ValueType::String, ValueType::String) => {
          Self::new_integer(compiler, (self.get_as_string() == other.get_as_string()) as i64)
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer, false, true, None)
        };
        (ValueType::String, ValueType::String) => |_, _| todo!();
      }
      match (&self.ty, &other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          Self::new_integer(compiler, (self.get_as_integer() != other.get_as_integer()) as i64)
        },
        (ValueType::String, ValueType::String) => {
          Self::new_integer(compiler, (self.get_as_string() != other.get_as_string()) as i64)
        },
        _ => Self::new_integer(compiler, 1),
      }
    }
  }
}
