# Pyr

[![build status](https://img.shields.io/github/workflow/status/AribYadi/pyr/Continuous%20integration/master)](https://github.com/AribYadi/pyr/actions?query=branch%3Amaster)

Pyr is a programming language similar to python (or in other words, python but not python).

## Table of Contents

- [Pyr](#pyr)
  - [Table of Contents](#table-of-contents)
  - [Requirements](#requirements)
  - [Building](#building)
  - [Usage](#usage)
  - [Examples](#examples)
  - [Testing](#testing)
  - [Editor Support](#editor-support)
  - [License](#license)

## Requirements

- [LLVM](https://llvm.org/releases) == 14.0.x

## Building

```bash
cargo build --release
```

## Usage

To interpret the source code, run the following command:

```bash
$ pyr run ./examples/hello.pyr
Hello, World!
```

To compile the source code, run the following command:

```bash
$ pyr build ./examples/hello.pyr
$ clang -o hello hello.o
$ ./hello
Hello, World!
```

## Examples

Hello, World:

```python
print "Hello, World"
```

If statements:

```python
if true:
  print 123
else:
  print 456
```

## Testing

To test, you can either do

```bash
cargo test
```

or

```bash
python test.py -d tests
python test.py -d examples
```

## Editor Support

- VSCode: [vscode-pyr](https://github.com/AribYadi/vscode-pyr.git)

## License

[Apache-2.0](LICENSE)
