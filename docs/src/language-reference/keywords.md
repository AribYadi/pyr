## Keywords

Keywords in Pyr are as follows:

- [Keywords](#keywords)
  - [`if`](#if)
  - [`else`](#else)
  - [`while`](#while)
  - [`and`](#and)
  - [`or`](#or)
  - [`func`](#func)

### `if`

`if` evaluates its condition and if it's truthy, it executes its body.

```python
if 34 < 56:
  print("34 is less than 56\n") # Executes this line
```

### `else`

`else` executes its body if the previous `if`'s condition is falsy.

```python
if 100 < 50:
  print("100 is less than 50\n") # Skips this line
else:
  print("100 is greater than 50\n") # Executes this line
```

`else` can also be continued with another `if`:

```python
if 100 < 50:
  print("100 is less than 50\n") # Skips this line
else if 100 > 50:
  print("100 is greater than 50\n") # Executes this line
else:
  print("100 is equal to 50\n") # Skips this line
```

### `while`

`while` will execute its body until the condition is falsy.

```python
i = 0
while i < 10: # Executes its body 10 times
  print(i + "\n")
  i += 1
```

### `and`

`and` with the same type of operands will return the left value if either is falsy and will return the right value if both are truthy. \
`and` with different types of operands will return `false` if either is falsy and will `true` if both are truthy. \
`and` will short circuit if left hand side is falsy. \

```python
10 > 2 and 10 < 20 # result is true
"hello" and "world" # result is "world"
123 and "" # result is false
```

### `or`

`or` with the same type of operands will return the left value if it is truthy otherwise it will return the right hand side. \
`or` with different types of operands will return `true` if either is truthy and will return `false` if both are falsy. \
`or` will short circuit if left hand side is truthy. \

```python
10 > 2 or 10 < 20 # result is true
"hello" or "world" # result is "hello"
123 or "" # result is true
```

### `func`

`func` defines a function.
For more information, see [Functions](./functions.md).

```python
func square(x: int):
  print(x * x)
```
