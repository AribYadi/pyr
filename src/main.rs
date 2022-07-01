use std::path::PathBuf;
use std::process::{
  self,
  Command,
};

use compiler::Compiler;
use interpreter::Interpreter;
use line_col::LineColLookup;
use parser::Parser;

#[macro_use]
mod utils;
mod compiler;
mod error;
mod interpreter;
mod optimizer;
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

struct Params {
  subcommand: ArgsSubcommand,
  source_path: String,
  so: Vec<String>,
}

enum ArgsSubcommand {
  Compile { out: Option<String>, exe: bool },
  Run,
}

#[rustfmt::skip]
// TODO: allow function to take variadic arguments
// TODO: report the correct position of errors, right now some errors have the wrong position
// TODO: prettify compiler code
// TODO: make indexing out of bounds error at compile time

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
  resolver.define_std();

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

  let options = optimizer::OptimizerOptions { ignore_expr_stmts: true, precalc_constant_ops: true, pre_do_if_stmts: true, ignore_falsy_while: true };
  let optimizer = optimizer::Optimizer::new(options);
  let stmts = optimizer.optimize(&stmts);

  match args.subcommand {
    ArgsSubcommand::Compile { out, exe } => {
      let obj_file = out
        .clone()
        .unwrap_or_else(|| source_path.with_extension("o").to_string_lossy().to_string());
      unsafe {
        let mut compiler = Compiler::new(input_file);
        compiler.define_std();

        compiler.compile_to_obj(&obj_file, &stmts);
      }
      info!(INFO, "Compiled to `{obj_file}`");
      if exe {
        #[cfg(target_os = "windows")]
        let exe_file =
          out.unwrap_or_else(|| source_path.with_extension("exe").to_string_lossy().to_string());
        #[cfg(not(target_os = "windows"))]
        let exe_file =
          out.unwrap_or_else(|| source_path.with_extension("").to_string_lossy().to_string());
        let clang_output =
          match Command::new("clang").arg("-o").arg(&exe_file).arg(&obj_file).args(args.so.iter()).output() {
            Ok(output) => output,
            Err(err) => {
              info!(ERR, "Failed to run `clang -o {exe_file} {obj_file} {so}`: {err}", so = args.so.join(" "));
              process::exit(1);
            },
          };
        if !clang_output.status.success() {
          info!(ERR, "Failed to link {obj_file}!");
          info!(ERR, "exit_code: {status}", status = clang_output.status);
          info!(ERR, "stderr: {stderr}", stderr = String::from_utf8_lossy(&clang_output.stderr));
          process::exit(1);
        }
        info!(INFO, "Successfully compiled and linked `{obj_file}` to `{exe_file}`!");
      }
    },
    ArgsSubcommand::Run => {
      let mut interpreter = Interpreter::new();
      interpreter.define_std();

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

fn get_args() -> Params {
  let mut args = std::env::args().skip(1);
  let mut arg_pos = 0;

  let mut subcommand = None;
  let mut source_path = None;
  let mut so = Vec::new();

  while let Some(arg) = args.next() {
    if arg.starts_with('-') {
      let text = arg.strip_prefix("--").unwrap_or_else(|| arg.strip_prefix('-').unwrap());
      match text {
        "help" | "h" => {
          print_help();
          process::exit(0);
        },
        "link" | "l" => {
          if args.len() < 1 {
            print_help();
            info!(ERR, "Missing argument for option: `{arg}`.");

            process::exit(1);
          }
          so.push(args.next().unwrap());
          continue;
        },
        _ => {
          if let Some(ArgsSubcommand::Compile { ref mut out, ref mut exe }) = subcommand {
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
              "e" | "exe" => {
                *exe = true;
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
        "compile" => subcommand = Some(ArgsSubcommand::Compile { out: None, exe: false }),
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

  Params { subcommand: subcommand.unwrap(), source_path: source_path.unwrap(), so }
}

fn print_help() {
  info!(
    INFO,
    "\x1b[1;32mUsage\x1b[0m: `pyr [OPTIONS] <SUBCOMMAND> [SUBCOMMAND_OPTIONS] <source_file>`"
  );
  info!(INFO, "");
  info!(INFO, "\x1b[1;32mSUBCOMMAND\x1b[0m:");
  info!(INFO, "  `compile` : Compiles the source file to an object file.");
  info!(INFO, "    --out, -o <name>: Specifies the output file name.");
  info!(INFO, "    --exe, -e       : Links the object file into an executable.");
  info!(INFO, "  `run`     : Interprets the source file line by line.");
  info!(INFO, "\x1b[1;32mOptions\x1b[0m:");
  info!(INFO, "  --help, -h       : Print this help message.");
  info!(INFO, "  --link, -l <path>: Links a shared library.")
}
