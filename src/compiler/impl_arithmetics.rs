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
          let (v, ty) = ($pattern_out)(self_, other_);
          return Self { v, ty, is_pointer: false, is_runtime: true }
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
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| { (LLVMBuildAdd(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer) };
        (ValueType::String, _) | (_, ValueType::String) => |_, _| todo!();
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
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| { (LLVMBuildSub(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer) };
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
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| { (LLVMBuildMul(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer) };
        (ValueType::String, ValueType::Integer) | (ValueType::Integer, ValueType::String) => |_, _| todo!();
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
        (ValueType::Integer, ValueType::Integer) => |self_: Self, other_: Self| { (LLVMBuildSDiv(compiler.builder, self_.v, other_.v, compiler.cstring("")), ValueType::Integer) };
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
        return Self { v, ty: ValueType::Integer, is_pointer: false, is_runtime: true };
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
            return ValueWrapper { v, ty: ValueType::Integer, is_pointer: false, is_runtime: true };
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer)
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer)
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer)
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer)
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer)
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
          (LLVMBuildIntCast2(compiler.builder, v, LLVMInt64TypeInContext(compiler.ctx), 0, compiler.cstring("")), ValueType::Integer)
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
