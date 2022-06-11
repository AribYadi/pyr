## Arithmetic

- [Arithmetic](#arithmetic)
  - [Addition](#addition)
  - [Subtraction](#subtraction)
  - [Multiplication](#multiplication)
  - [Division](#division)
  - [Negation](#negation)
  - [Exponent](#exponent)
  - [Modulo / Remainder](#modulo--remainder)

### Addition

`+` with integers will add the numbers together.

```python
1 + 2 # result is 3
```

`+` with strings is concatenation.

```python
"Hello, " + "World!" # result is "Hello, World!"
```

### Subtraction

`-` will subtract the first number with the second.

```python
1 - 2 # result is -1
```

### Multiplication

`*` with integers will multiply the numbers together.

```python
1 * 2 # result is 2
```

`*` between strings and integers is string repetition.

```python
"Hello" * 3 # result is "HelloHelloHello"
```

### Division

`/` with integers will divide the first number by the second.

```python
30 / 5 # result is 6
```

### Negation

`-` will either make its right hand side negative or positive. \
`!` will return `true` if its right hand side is falsy, and `false` if it is truthy.

```python
-1 # result is -1
!true # result is 0
```

For more information about truthiness and falsiness, see [Truthiness](./truthiness.md).

### Exponent

`^` will multiply its left hand side by itself the number of times on the right hand side.

```python
2 ^ 3 # result is 8
```

### Modulo / Remainder

`%` will return the remainder of its left hand side subtracted by its right hand side multiple times until it is less than its original value.

```python
30 % 5 # result is 0
4 % 3 # result is 1
12 % 5 # result is 2
```
