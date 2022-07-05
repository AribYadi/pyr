# Command Line Interface

`pyr [OPTIONS] <SUBCOMMAND> [SUBCOMMAND_OPTIONS] <source_file>`

- [Command Line Interface](#command-line-interface)
  - [Options](#options)
    - [`--help` | `-h`](#--help---h)
    - [`--link` | `-l`](#--link---l)
  - [Subcommand](#subcommand)
    - [Run](#run)
    - [Compile](#compile)
      - [Options](#options-1)
        - [`--out` | `-o`](#--out---o)
        - [`--exe` | `-e`](#--exe---e)

## Options

### `--help` | `-h`

Prints the help message.

### `--link` | `-l`

Links a dynamic/shared library.

## Subcommand

### Run

Run/Interprets the program line by line.

### Compile

Compiles the program into an object file.

#### Options

##### `--out` | `-o`

Specifies the output file name.

##### `--exe` | `-e`

Links the object file into an executable.
