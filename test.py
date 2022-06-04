#!/usr/bin/env python3

from dataclasses import dataclass
import dataclasses
from enum import Enum
from os import path
import os
import subprocess
import sys
from typing import BinaryIO, List

def print_help():
  print("\x1b[2;96m[INFO]\x1b[0m: \x1b[1;32mUsage\x1b[0m: `test.py [OPTIONS]`")
  print()
  print("\x1b[2;96m[INFO]\x1b[0m: \x1b[1;32mOptions\x1b[0m:")
  print("\x1b[2;96m[INFO]\x1b[0m:   --help,     -h: Print this help message.")
  print("\x1b[2;96m[INFO]\x1b[0m:   --release,  -r: Test in release mode.")
  print("\x1b[2;96m[INFO]\x1b[0m:   --always-build: Always build `pyr` even if already built.")
  print("\x1b[2;96m[INFO]\x1b[0m:   --dir,      -d: Specify test dir.")

def executable_extension():
  if sys.platform.startswith("win"):
    return ".exe"
  return ""

@dataclass
class Args():
  update: bool = False
  release: bool = False
  always_build: bool = False
  dir: str = "tests"

  @classmethod
  def from_args(cls, args: List[str]) -> "Args":
    self = cls()

    arg_pos = 0
    for arg in args:
      if arg.startswith("-"):
        text = [arg[1:]]
        if text[0].startswith("-"):
          text[0] = text[1:]
        
        if text[0] == "h" or text[0] == "help":
          print_help()
          exit(0)
        elif text[0] == "u" or text[0] == "update":
          self.update = True
        elif text[0] == "r" or text[0] == "release":
          self.release = True
        elif text[0] == "always-build":
          self.always_build = True
        elif text[0] == "d" or text[0] == "dir":
          if arg_pos + 1 > len(args):
            print_help()
            print(f"\x1b[1;31m[ERR]\x1b[0m: Missing argument for option `{arg}`.", file = sys.stderr)
            exit(1)
          self.dir = args[arg_pos + 1]
        else:
          print_help()
          print(f"\x1b[1;31m[ERR]\x1b[0m: Unknown argument: `{arg}`.", file = sys.stderr)
          exit(1)
      arg_pos += 1
    return self

args = Args.from_args(sys.argv[1:])

BASE_DIR = path.dirname(__file__)

PYR_DEBUG_BINARY = path.join(BASE_DIR, "target", "debug", "pyr")
PYR_RELEASE_BINARY = path.join(BASE_DIR, "target", "release", "pyr")
TEST_DIR = path.join(BASE_DIR, args.dir)

PYR_EXT = ".pyr"
RECORD_EXT = ".record"

END_TAG = "\n:end:\n"

def exe_exist(path: str) -> bool:
  return os.path.exists(path) or os.path.exists(path + ".exe")

def cargo_build():
  print("\x1b[2;96m[INFO]\x1b[0m: Building pyr..")
  extra_args = []
  if args.release:
    extra_args.append("--release")
  output = subprocess.run(["cargo", "build", *extra_args], capture_output = True)
  if output.returncode != 0:
    print(f"\x1b[1;31m[ERR]\x1b[0m: Cargo build failed.", file = sys.stderr)

    print(f"\x1b[1;31m[ERR]\x1b[0m: Stderr:", file = sys.stderr)
    print(f"{output.stderr}", file = sys.stderr)
    exit(1)
  print("\x1b[2;96m[INFO]\x1b[0m: pyr built.")

@dataclass
class TestCase():
  exitcode: int
  stdout: bytes
  stderr: bytes

  def write(self, path: str) -> bool:
    try:
      if TestCase.read(path) == self:
        return False
    except (FileNotFoundError, AssertionError):
      pass
    with open(path, "wb") as f:
      TestCase.write_int_field(f, "exitcode", self.exitcode)
      TestCase.write_blob_field(f, "stdout", self.stdout)
      TestCase.write_blob_field(f, "stderr", self.stderr)
      return True

  @classmethod
  def read(cls, path: str) -> "TestCase":
    with open(path, "rb") as f:
      exitcode = TestCase.read_int_field(f, "exitcode")
      stdout = TestCase.read_blob_field(f, "stdout")
      stderr = TestCase.read_blob_field(f, "stderr")
    return cls(exitcode, stdout, stderr)

  @staticmethod
  def write_blob_field(f: BinaryIO, name: str, blob: bytes):
    TestCase.write_int_field(f, name, len(blob))
    f.write(blob)
    f.write(END_TAG.encode())

  @staticmethod
  def write_int_field(f: BinaryIO, name: str, value: int):
    f.write(f":{name} {value}:\n".encode())
  
  @staticmethod
  def read_blob_field(f: BinaryIO, name: str) -> bytes:
    line = f.readline()
    field = f":{name} ".encode()
    assert line.startswith(field), f"Broken record file. Missing blob `{name}` tag."
    assert line.endswith(b":\n"), f"Broken record file. Missing closing tag of blob `{name}`."
    size = int(line[len(field):-2])
    blob = f.read(size)
    assert f.read(len(END_TAG)) == END_TAG.encode(), f"Broken record file. Missing end of blob `{name}` tag."
    return blob
  
  @staticmethod
  def read_int_field(f: BinaryIO, name: str) -> int:
    line = f.readline()
    field = f":{name} ".encode()
    assert line.startswith(field), f"Broken record file. Missing int `{name}` tag."
    assert line.endswith(b":\n"), f"Broken record file. Missing closing tag of int `{name}`."
    return int(line[len(field):-2])

@dataclass
class TestResults():
  passed: int = 0
  failed: int = 0
  skipped: int = 0
  updated: int = 0
  to_be_deleted: List[str] = dataclasses.field(default_factory = list)

class Subcommand(Enum):
  Run = 1
  Compile = 2

  def __str__(self) -> str:
    if self == Subcommand.Run:
      return "run"
    elif self == Subcommand.Compile:
      return "compile"

def relative_path(path):
  return str(path).replace(BASE_DIR, ".")

def test_file(input_path, subcommand: Subcommand, results: TestResults):
  assert input_path.endswith(PYR_EXT), f"{relative_path(input_path)} is not a pyr file."

  tc_path = input_path[:-len(PYR_EXT)] + RECORD_EXT
  tc_path = relative_path(tc_path)

  if not os.path.exists(tc_path):
    print(f"\x1b[33m[WARN]\x1b[0m: Couldn't find record file for {input_path}. Skipping.")
    results.skipped += 1
    return

  expected_test_case = TestCase.read(tc_path)

  pyr_output = subprocess.run([PYR_DEBUG_BINARY, subcommand.__str__(), input_path], capture_output = True)
  output = []
  if expected_test_case.exitcode == 0 and subcommand == Subcommand.Compile:
    if pyr_output.returncode != 0:
      print(f"\x1b[1;31m[ERR]\x1b[0m: Compilation failed for {relative_path(input_path)}.", file = sys.stderr)
      print(f"\x1b[1;31m[ERR]\x1b[0m: Stderr:", file = sys.stderr)
      print(f"{pyr_output.stderr}", file = sys.stderr)
      results.failed += 1
      return
    object_file = input_path[:-len(PYR_EXT)] + ".o"
    exe_file = input_path[:-len(PYR_EXT)] + executable_extension()
    clang_output = subprocess.run(["clang", object_file, "-o", exe_file], capture_output = True)
    if clang_output.returncode != 0:
      print(f"\x1b[1;31m[ERR]\x1b[0m: Clang failed to link `{object_file}`.", file = sys.stderr)
      print(f"\x1b[1;31m[ERR]\x1b[0m: Stderr:", file = sys.stderr)
      print(f"{clang_output.stderr.decode()}", file = sys.stderr)
      results.failed += 1
      return
    results.to_be_deleted.append(object_file)
    results.to_be_deleted.append(exe_file)
    output.append(subprocess.run([exe_file], capture_output = True))
  else:
    output.append(pyr_output)

  output = output[0]
  test_case = TestCase(output.returncode, output.stdout.replace(b'\r\n', b'\n'), output.stderr.replace(b'\r\n', b'\n'))

  input_path = relative_path(input_path)
  
  if args.update:
    print(f"\x1b[2;96m[INFO]\x1b[0m: Updating `{tc_path}`..")
    if test_case.exitcode != 0:
      print(f"\x1b[33m[WARN]\x1b[0m: Test `{input_path}` returned an abnormal exit code {test_case.exitcode}.", file = sys.stderr)
    if test_case.write(tc_path):
      print(f"\x1b[2;96m[INFO]\x1b[0m: {tc_path} updated.")
      results.updated += 1
    else:
      print(f"\x1b[2;96m[INFO]\x1b[0m: {tc_path} is up to date. Skipping.")
      results.skipped += 1
  else:
    print(f"\x1b[2;96m[INFO]\x1b[0m: Testing `{input_path}` with subcommand `{subcommand.__str__()}`..")

    if test_case == expected_test_case:
      print(f"\x1b[2;96m[INFO]\x1b[0m: {input_path} passed.")
      results.passed += 1
    else:
      print(f"\x1b[1;31m[ERR]\x1b[0m: {input_path} failed with subcommand `{subcommand.__str__()}`.", file = sys.stderr)
      print(f"\x1b[1;31m[ERR]\x1b[0m: Expected:", file = sys.stderr)
      print(f"  exit code: {expected_test_case.exitcode}", file = sys.stderr)
      print(f"  stdout: {expected_test_case.stdout}", file = sys.stderr)
      print(f"  stderr: {expected_test_case.stderr}", file = sys.stderr)
      print(f"\x1b[1;31m[ERR]\x1b[0m: Got:", file = sys.stderr)
      print(f"  exit code: {test_case.exitcode}", file = sys.stderr)
      print(f"  stdout: {test_case.stdout}", file = sys.stderr)
      print(f"  stderr: {test_case.stderr}", file = sys.stderr)
      results.failed += 1

if __name__ == "__main__":
  if not args.release and (not exe_exist(PYR_DEBUG_BINARY), args.always_build):
    subprocess.run(["cargo", "build"], capture_output = True)
  elif args.release and (not exe_exist(PYR_RELEASE_BINARY), args.always_update):
    subprocess.run(["cargo", "build", "--release"], capture_output = True)

  print()

  os.chdir(BASE_DIR)
  results = TestResults()
  for entry in os.scandir(TEST_DIR):
    if entry.is_file() and entry.path.endswith(PYR_EXT):
      test_file(entry.path, Subcommand.Run, results)
      if not args.update:
        test_file(entry.path, Subcommand.Compile, results)

  if args.update:
    print()
    print(f"\x1b[2;96m[INFO]\x1b[0m: {results.updated} updated.")
    print(f"\x1b[2;96m[INFO]\x1b[0m: {results.skipped} skipped.")
  else:
    print()
    print(f"\x1b[2;96m[INFO]\x1b[0m: {results.passed + results.failed} tested.")
    print(f"\x1b[2;96m[INFO]\x1b[0m: {results.passed} passed.")
    print(f"\x1b[2;96m[INFO]\x1b[0m: {results.failed} failed.")
    print(f"\x1b[2;96m[INFO]\x1b[0m: {results.skipped} skipped.")
    for file in results.to_be_deleted:
      os.remove(file)

  if results.failed > 0:
    exit(1)
