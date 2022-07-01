## Functions

- [Functions](#functions)
  - [`print`](#print)
  - [`sqrt`](#sqrt)
  - [`to_int`](#to_int)

### `print`

`print` prints the `v` to the console without appendding a newline.

Signature:

```python
def print(v: int | str):
  ...
```

Example:

```python
print("Hello, World!")
print(123)
```

### `sqrt`

`sqrt` returns the square root of the `n`

Signature:

```python
def sqrt(n: int) -> int:
  ...
```

Example:

```python
sqrt(64) == 8
sqrt(16) == 4
```

### `to_int`

`to_int` converts `s` into an integer then returns it

Signature:

```python
def to_int(s: str) -> int:
  ...
```

Example:

```Python
to_int("123") == 123
```
