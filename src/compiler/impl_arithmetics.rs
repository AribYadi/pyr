use super::*;

impl ValueWrapper {
  pub fn add(self, compiler: &mut Compiler, other: ValueWrapper) -> ValueWrapper {
    unsafe {
      match (self.ty, other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          ValueWrapper::new_integer(compiler, self.get_as_integer() + other.get_as_integer())
        },
        (ValueType::String, _) | (_, ValueType::String) => {
          ValueWrapper::new_string(compiler, &format!("{self}{other}"))
        },
        #[allow(unreachable_patterns)]
        _ => unreachable!("Resolver didn't type check infix operator `+`"),
      }
    }
  }

  pub fn sub(self, compiler: &mut Compiler, other: ValueWrapper) -> ValueWrapper {
    unsafe {
      match (self.ty, other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          ValueWrapper::new_integer(compiler, self.get_as_integer() - other.get_as_integer())
        },
        _ => unreachable!("Resolver didn't type check infix operator `-`"),
      }
    }
  }

  pub fn mul(self, compiler: &mut Compiler, other: ValueWrapper) -> ValueWrapper {
    unsafe {
      match (self.ty, other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          ValueWrapper::new_integer(compiler, self.get_as_integer() * other.get_as_integer())
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
          ValueWrapper::new_string(compiler, &string)
        },
        _ => unreachable!("Resolver didn't type check infix operator `*`"),
      }
    }
  }

  pub fn div(self, compiler: &mut Compiler, other: ValueWrapper) -> ValueWrapper {
    unsafe {
      match (self.ty, other.ty) {
        (ValueType::Integer, ValueType::Integer) => {
          ValueWrapper::new_integer(compiler, self.get_as_integer() / other.get_as_integer())
        },
        _ => unreachable!("Resolver didn't type check infix operator `/`"),
      }
    }
  }

  pub fn neg(self, compiler: &mut Compiler) -> ValueWrapper {
    unsafe {
      match self.ty {
        ValueType::Integer => ValueWrapper::new_integer(compiler, -self.get_as_integer()),
        _ => unreachable!("Resolver didn't type check prefix operator `-`"),
      }
    }
  }

  pub fn not(self, compiler: &mut Compiler) -> ValueWrapper {
    unsafe { ValueWrapper::new_integer(compiler, !self.is_truthy() as i64) }
  }
}
