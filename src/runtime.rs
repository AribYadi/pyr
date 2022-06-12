use crate::interpreter::Interpreter;
use crate::parser::syntax::Stmt;

mod impl_arithmetics;

pub type IndentLevel = usize;

#[derive(Clone)]
pub struct Variables<T: Clone> {
  variables: Vec<(String, IndentLevel, T)>,
}

impl<T: Clone> Variables<T> {
  pub fn new() -> Self { Self { variables: Vec::new() } }

  pub fn declare(&mut self, name: &str, level: IndentLevel, value: T) -> T {
    self.variables.push((name.to_string(), level, value.clone()));
    value
  }

  pub fn get(&self, name: &str) -> Option<T> {
    self.variables.iter().find(|(n, _, _)| n == name).map(|(_, _, v)| v).cloned()
  }

  pub fn get_mut(&mut self, name: &str) -> Option<&mut T> {
    self.variables.iter_mut().find(|(n, _, _)| n == name).map(|(_, _, v)| v)
  }

  pub fn assign_or_declare(&mut self, name: &str, level: IndentLevel, value: T) -> T {
    if let Some((_, _, v)) = self.variables.iter_mut().find(|(n, _, _)| n == name) {
      *v = value.clone();
      value
    } else {
      self.declare(name, level, value)
    }
  }

  pub fn remove_all_with_indent(&mut self, level: IndentLevel) {
    self.variables.retain(|(_, l, _)| *l != level);
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  String(String),
  Integer(i64),
}

impl std::fmt::Display for Literal {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Literal::String(s) => write!(f, "{s}"),
      Literal::Integer(n) => write!(f, "{n}"),
    }
  }
}

impl Literal {
  pub fn is_truthy(&self) -> bool {
    match self {
      Literal::Integer(n) => *n == 1,
      Literal::String(s) => !s.is_empty(),
    }
  }

  pub fn is_same_variant(&self, other: &Self) -> bool {
    matches!(
      (self, other),
      (Literal::String(_), Literal::String(_)) | (Literal::Integer(_), Literal::Integer(_))
    )
  }
}

pub type Args = Vec<Literal>;
pub type NativeFunction = fn(&mut Interpreter, Args) -> ReturnValue<Literal>;
pub type ReturnValue<T> = Option<T>;

#[derive(Clone)]
pub enum Function {
  Native(NativeFunction, usize),
  UserDefined(Vec<String>, Vec<Stmt>),
}

impl std::fmt::Debug for Function {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Function::Native(_, _) => write!(f, "Native(..)"),
      Function::UserDefined(args, body) => write!(f, "UserDefined({:?}, {:?})", args, body),
    }
  }
}

impl PartialEq for Function {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Function::Native(_, _), Function::Native(_, _)) => false,
      (Function::UserDefined(a1, b1), Function::UserDefined(a2, b2)) => a1 == a2 && b1 == b2,
      _ => false,
    }
  }
}

impl From<(NativeFunction, usize)> for Function {
  fn from((f, arg_count): (NativeFunction, usize)) -> Self { Function::Native(f, arg_count) }
}
