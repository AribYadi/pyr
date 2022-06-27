macro_rules! bx {
  ($val:expr) => {
    Box::new($val)
  };
}

macro_rules! rc {
  ($val:expr) => {
    std::rc::Rc::new($val)
  };
}
