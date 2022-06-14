## Functions

Functions in Pyr are defined with the `func` keyword.
Function arguments are explicitly [typed](./literals.md), and are separated by commas.

```python
func foo(a: int, b: int) -> int:
  print a + b + "\n"
```

To call a function, simply use the function name and parentheses with the arguments.

```python
foo(1, 2) # prints 3
```

Functions and variables have different scopes.
So you can have both a variable named `foo` and a function named `foo`.

```python
foo = "bar"
func foo():
  print foo + "\n"

foo() # prints "bar"
foo = "baz"
foo() # prints "baz"
```

Functions can also return a value.

```python
func sum_of_squares(a: int, b: int) -> int:
  return a ^ 2 + b ^ 2

print sum_of_squares(2, 3) # prints 9
```

In the future, functions will be able to be overloaded.
