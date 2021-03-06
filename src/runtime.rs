use std::ffi::c_void;
use std::rc::Rc;

use crate::info;
use crate::interpreter::Interpreter;
use crate::parser::syntax::{
  Expr,
  ExprKind,
  Stmt,
};

mod impl_arithmetics;

#[macro_export]
macro_rules! ignore_return {
  (NEVER; $self:ident, $expr:expr, $($callback:tt)*) => {{
    ignore_return!(false; $self, $expr, $($callback)*);
  }};
  (NEVER_WITH_RET; $self:ident, $expr:expr, $($callback:tt)*) => {{
    let prev_ignore_return = $self.ignore_return;
    $self.ignore_return = false;
    let ret = $($callback)*;
    $self.ignore_return = prev_ignore_return;
    ret
  }};
  ($self:ident, $expr:expr, $($callback:tt)*) => {{
    ignore_return!(matches!($expr.kind, ExprKind::Integer(_) | ExprKind::String(_) | ExprKind::Identifier(_) | ExprKind::FuncCall { .. }); $self, $expr, $($callback)*);
  }};
  ($cond:expr; $self:ident, $expr:expr, $($callback:tt)*) => {{
    let prev_ignore_return = $self.ignore_return;
    $self.ignore_return = $cond;
    $($callback)*;
    $self.ignore_return = prev_ignore_return;
  }};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum State {
  TopLevel,
  Function,
  Loop,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateStack {
  pub stack: Vec<State>,
  pub last_rewind: Option<State>,
}

impl StateStack {
  pub fn new() -> Self { Self { stack: vec![State::TopLevel], last_rewind: None } }

  pub fn push(&mut self, state: State) { self.stack.push(state); }

  pub fn pop(&mut self, state: State) {
    if self.last_rewind != Some(state) {
      self.stack.pop();
    } else {
      self.last_rewind = None;
    }
  }

  pub fn contains(&self, state: State) -> bool { self.stack.contains(&state) }

  pub fn pop_until(&mut self, state: State) {
    let pos = self.stack.iter().rev().position(|&s| s == state).unwrap();
    self.stack.truncate(self.stack.len() - pos);
    self.last_rewind = self.stack.pop();
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
  Void,
  Integer,
  String,
  Array(Box<ValueType>, Rc<Expr>),
}

impl std::fmt::Display for ValueType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ValueType::Void => write!(f, "void"),
      ValueType::Integer => write!(f, "int"),
      ValueType::String => write!(f, "string"),
      ValueType::Array(t, len) => write!(f, "{t}[{len}]"),
    }
  }
}

impl From<ReturnValue<ValueType>> for ValueType {
  fn from(ret: ReturnValue<ValueType>) -> Self {
    match ret {
      Some(v) => v,
      None => ValueType::Void,
    }
  }
}

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

  pub fn get(&mut self, name: &str) -> Option<T> {
    self.variables.iter().rev().find_map(|(n, _, v)| if n == name { Some(v.clone()) } else { None })
  }

  pub fn get_mut(&mut self, name: &str) -> Option<&mut T> {
    self.variables.iter_mut().rev().find_map(|(n, _, v)| if n == name { Some(v) } else { None })
  }

  pub fn get_variable(&mut self, name: &str) -> Option<(String, IndentLevel, T)> {
    self.variables.iter_mut().rev().find_map(
      |var| {
        if var.0 == name {
          Some(var.clone())
        } else {
          None
        }
      },
    )
  }

  pub fn assign_or_declare(&mut self, name: &str, level: IndentLevel, value: T) -> T {
    if let Some((_, _, v)) = self.variables.iter_mut().rev().find(|(n, _, _)| n == name) {
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
  Array(Vec<Literal>),
}

impl std::fmt::Display for Literal {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Literal::String(s) => write!(f, "{s}"),
      Literal::Integer(n) => write!(f, "{n}"),

      // Commented out because in the compiler, we currently don't support arrays
      // Literal::Array(elems) => write!(
      //   f,
      //   "{elems}",
      //   elems = elems.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
      // ),
      Literal::Array(_) => info!(INTR_ERR, "Resolver didn't resolve array to string conversion"),
    }
  }
}

impl Literal {
  pub fn is_truthy(&self) -> bool {
    match self {
      Literal::Integer(n) => *n == 1,
      Literal::String(s) => !s.is_empty(),
      Literal::Array(elems) => !elems.is_empty(),
    }
  }

  pub fn is_same_variant(&self, other: &Self) -> bool {
    matches!(
      (self, other),
      (Literal::String(_), Literal::String(_)) |
        (Literal::Integer(_), Literal::Integer(_)) |
        (Literal::Array(_), Literal::Array(_))
    )
  }

  pub fn get_type(&self) -> ValueType {
    match self {
      Literal::String(_) => ValueType::String,
      Literal::Integer(_) => ValueType::Integer,
      Literal::Array(elems) => ValueType::Array(
        bx!(elems[0].get_type()),
        rc!(Expr::new(ExprKind::Integer(elems.len() as i64), 999..999)),
      ),
    }
  }
}

pub type Params = Vec<Literal>;
pub type NativeFunction = fn(&mut Interpreter, Params) -> ReturnValue<Literal>;
pub type ExternalFunction =
  crate::libloading::Symbol<unsafe extern "C" fn(*const c_void, ...) -> *const c_void>;
pub type ReturnValue<T> = Option<T>;

pub fn func_name(base_name: &str, args: &[ValueType]) -> String {
  let args_as_string: Vec<_> = args.iter().map(ToString::to_string).collect();
  [base_name.to_string(), args_as_string.join(".")].join(".")
}

#[derive(Clone)]
pub enum Function {
  Native(NativeFunction, usize),
  UserDefined(Vec<String>, Vec<Stmt>, ReturnValue<ValueType>),
  External(ExternalFunction, usize, ReturnValue<ValueType>),
}

impl std::fmt::Debug for Function {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Function::Native(_, _) => write!(f, "Native(..)"),
      Function::UserDefined(args, body, ret_ty) => {
        write!(f, "UserDefined({args:?}, {body:?}, {ret_ty:?})")
      },
      Function::External(_, _, _) => write!(f, "External(..)"),
    }
  }
}

impl PartialEq for Function {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Function::UserDefined(a1, b1, rt1), Function::UserDefined(a2, b2, rt2)) => {
        a1 == a2 && b1 == b2 && rt1 == rt2
      },
      _ => false,
    }
  }
}

impl From<(NativeFunction, usize)> for Function {
  fn from((f, arg_count): (NativeFunction, usize)) -> Self { Function::Native(f, arg_count) }
}
