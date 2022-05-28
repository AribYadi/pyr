# Pyr

[![build status](https://img.shields.io/github/workflow/status/AribYadi/pyr/Continuous%20integration/master)](https://github.com/AribYadi/pyr/actions?query=branch%3Amaster)

Pyr is a programming language similar to python (or in other words, python but not python).

## Building

```bash
cargo build --release
```

## Usage

```bash
$ pyr ./examples/hello.pyr
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

## License

[Apache-2.0](LICENSE)
