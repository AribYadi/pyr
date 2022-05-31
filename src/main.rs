use std::path::PathBuf;
use std::process;

use compiler::Compiler;
use interpreter::Interpreter;
use line_col::LineColLookup;
use parser::Parser;

mod compiler;
mod error;
mod interpreter;
mod parser;
mod resolver;
mod runtime;

#[macro_export]
macro_rules! info {
  (ERR, $($msg:tt)*) => {{
    let msg = format!($($msg)*);
    eprintln!("\x1b[1;31m[ERR]\x1b[0m : {msg}");
  }};
  (WARN, $($msg:tt)*) => {{
    let msg = format!($($msg)*);
    println!("\x1b[33m[WARN]\x1b[0m: {msg}");
  }};
  (INFO, $($msg:tt)*) => {{
    let msg = format!($($msg)*);
    println!("\x1b[2;96m[INFO]\x1b[0m: {msg}");
  }};
}

struct Args {
  subcommand: ArgsSubcommand,
  source_path: String,
}

enum ArgsSubcommand {
  Compile { out: Option<String> },
  Run,
}

fn main() {
  let args = get_args();
  let source_path = PathBuf::from(&args.source_path);
  let input_file = &source_path.file_name().unwrap().to_string_lossy();

  let input = match std::fs::read_to_string(&source_path) {
    Ok(source) => source,
    Err(error) => match error.kind() {
      std::io::ErrorKind::NotFound => {
        info!(ERR, "File not found: {}", args.source_path);
        process::exit(1);
      },
      _ => {
        info!(ERR, "Error reading file: {error}");
        process::exit(1);
      },
    },
  };
  let lookup = LineColLookup::new(&input);

  let mut parser = Parser::new(&input);
  let stmts = match parser.parse() {
    Ok(stmts) => stmts,
    Err(errors) => {
      for error in errors {
        let (line, col) = lookup.get(error.span.start);

        info!(ERR, "A parsing error has been found!");
        info!(ERR, " -> {input_file}:{line}:{col}",);
        info!(ERR, " -> {msg}", msg = error.kind)
      }

      process::exit(1);
    },
  };
  let mut resolver = resolver::Resolver::new();
  match resolver.resolve(&stmts) {
    Ok(_) => (),
    Err(errors) => {
      for error in errors {
        let (line, col) = lookup.get(error.span.start);

        info!(ERR, "A runtime error has been found!");
        info!(ERR, " -> {input_file}:{line}:{col}",);
        info!(ERR, " -> {msg}", msg = error.kind)
      }

      process::exit(1);
    },
  };
  match args.subcommand {
    ArgsSubcommand::Compile { out } => {
      let output_file = out.unwrap_or_else(|| {
        source_path.with_extension("o").file_name().unwrap().to_string_lossy().to_string()
      });
      unsafe {
        let compiler = Compiler::new(input_file);
        compiler.compile_to_obj(&output_file, &stmts);
      }
    },
    ArgsSubcommand::Run => {
      let mut interpreter = Interpreter::new();
      match interpreter.interpret(&stmts) {
        Ok(_) => (),
        Err(error) => {
          let (line, col) = lookup.get(error.span.start);

          info!(ERR, "A runtime error has been found!");
          info!(ERR, " -> {input_file}:{line}:{col}");
          info!(ERR, " -> {msg}", msg = error.kind);

          process::exit(1);
        },
      };
    },
  }
}

fn get_args() -> Args {
  let mut args = std::env::args().skip(1);
  let mut arg_pos = 0;

  let mut subcommand = None;
  let mut source_path = None;

  while let Some(arg) = args.next() {
    if arg.starts_with('-') {
      let text = arg.strip_prefix("--").unwrap_or_else(|| arg.strip_prefix('-').unwrap());
      match text {
        "help" | "h" => {
          print_help();
          process::exit(0);
        },
        _ => {
          if let Some(ArgsSubcommand::Compile { ref mut out }) = subcommand {
            match text {
              "o" | "out" => {
                if args.len() < 1 {
                  print_help();
                  info!(ERR, "Missing argument for option: `{arg}`.");

                  process::exit(1);
                }
                *out = Some(args.next().unwrap());
                continue;
              },
              _ => (),
            }
          }

          print_help();
          info!(ERR, "Unknown argument: `{arg}`.");

          process::exit(1);
        },
      }
    }

    match arg_pos {
      0 => match arg.as_str() {
        "run" => subcommand = Some(ArgsSubcommand::Run),
        "compile" => subcommand = Some(ArgsSubcommand::Compile { out: None }),
        _ => {
          print_help();
          info!(ERR, "Unknown subcommand: `{arg}`.");

          process::exit(1);
        },
      },
      1 => source_path = Some(arg),
      _ => {
        info!(WARN, "Unused leftover argument: `{arg}`.");
      },
    }
    arg_pos += 1;
  }

  if subcommand.is_none() {
    print_help();
    info!(ERR, "No subcommand specified.");

    process::exit(1);
  }

  if source_path.is_none() {
    print_help();
    info!(ERR, "No source file specified.");

    process::exit(1);
  }

  Args { subcommand: subcommand.unwrap(), source_path: source_path.unwrap() }
}

fn print_help() {
  info!(
    INFO,
    "\x1b[1;32mUsage\x1b[0m: `pyr [OPTIONS] <SUBCOMMAND> [SUBCOMMAND_OPTIONS] <source_file>`"
  );
  info!(INFO, "");
  info!(INFO, "\x1b[1;32mSUBCOMMAND\x1b[0m:");
  info!(INFO, "  `compile` : Compiles the source file to an object file.");
  info!(INFO, "    --out, -o: Specifies the output file name.");
  info!(INFO, "  `run`     : Interprets the source file line by line.");
  info!(INFO, "\x1b[1;32mOptions\x1b[0m:");
  info!(INFO, "  --help, -h: Print this help message.");
}
