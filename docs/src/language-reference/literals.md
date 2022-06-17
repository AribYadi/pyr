## Literals

Pyr currently supports the following literals:

- [Literals](#literals)
  - [Integer](#integer)
  - [Boolean](#boolean)
  - [String](#string)
  - [Arrays](#arrays)

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

You can also index into strings.

```python
"Hello, World!"[0] # returns "H"
```

Type: `string`

### Arrays

Arrays in Pyr starts with the types of the elements, followed by the elements delimited with square brackets and separated by commas.

```python
int[] # returns an empty array of integers
int[123, 456] # returns an array of integers with two elements
string["Hello, World!"] # returns an array of strings with one element
```

When initializing an array, you can specify an element then clone the element by the given length.

```python
int[0; 30] # returns an array of 30 zeros
int[1, 2, 3; 10] # returns int[1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
```

You can index into an array with the index.

```python
int[1, 2, 3; 10][1] # returns 2
```

Type: `[type; len]` where `type` is the type of the elements and `len` is the length of the array. \
Note: Currently, arrays cannot be converted to strings.
