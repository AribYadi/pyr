## Literals

Pyr currently supports the following literals:

- [Literals](#literals)
  - [Integer](#integer)
  - [Boolean](#boolean)
  - [String](#string)

### Integer

Integers in Pyr are 64 bit signed integers.

```python
123
```

Type: `int`

### Boolean

Booleans in Pyr are secretly integers with `true` as 1 and `false` as 0.

```python
true # returns 1
false # returns 0
```

Type: `int`

### String

Strings in Pyr are delimited by double quotes. \
For escaping things like double quotes, you can use a backslash. \
Multiline strings are also supported. \

```python
"Hello, World!"
"Newlines\n"
```

Type: `string`
