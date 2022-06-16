# Pyr

![build status](https://img.shields.io/github/workflow/status/AribYadi/pyr/Continuous%20integration/master)

Pyr is a programming language similar to python (or in other words, python but not python).
For more information, see [the documentation](https://aribyadi.github.io/pyr/).

## Table of Contents

- [Pyr](#pyr)
  - [Table of Contents](#table-of-contents)
  - [Requirements](#requirements)
  - [Building](#building)
  - [Usage](#usage)
  - [Examples](#examples)
  - [Testing](#testing)
  - [Documentation](#documentation)
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
$ pyr compile ./examples/hello.pyr --link
$ ./examples/hello
Hello, World!
```

## Examples

Hello, World:

```python
print("Hello, World")
```

If statements:

```python
if true:
  print(123)
else:
  print(456)
```

For more examples, see the [examples directory](./examples).

## Testing

To run unit test the source code, run the following command:

```bash
cargo test
```

To run end-to-end tests, run the following command:

```bash
./run_tests.py
```

## Documentation

For the online documentation, <https://aribyadi.github.io/pyr/>. \
Or you can build it locally and run it with the following command:

```bash
cd docs
mdbook serve --open
```

Note: To build it locally you need to install [mdbook](https://rust-lang.github.io/mdBook/guide/installation.html).

## Editor Support

- VSCode: [vscode-pyr](https://github.com/AribYadi/vscode-pyr.git)

## License

[Apache-2.0](LICENSE)
