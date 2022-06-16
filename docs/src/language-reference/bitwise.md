## Bitwise

- [Bitwise](#bitwise)
  - [`<<`](#)
  - [`>>`](#-1)
  - [`&`](#-2)
  - [`|`](#-3)

### `<<`

`<<` shifts the bits of the left operand to the left by the number of bits specified by the right operand.

```python
a = 2 # 00000010
b = 3
a << b # 00010000 or 16
```

### `>>`

`>>` shifts the bits of the left operand to the right by the number of bits specified by the right operand.

```python
a = 2 # 00000010
b = 1
a >> b # 00000001 or 1
```

### `&`

`&` performs a [bitwise AND](https://en.wikipedia.org/wiki/Bitwise_operation#AND) operation on the two operands.

```python
a = 2 # 00000100
b = 3 # 00000011
a & b # 00000000 or 0
```

### `|`

`|` performs a [bitwise OR](https://en.wikipedia.org/wiki/Bitwise_operation#OR) operation on the two operands.

```python
a = 2 # 00000100
b = 3 # 00000011
a | b # 00000111 or 7
```
