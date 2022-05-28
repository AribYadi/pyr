#!/usr/bin/env python3

import glob
from os import path
import os
import subprocess
import pathlib
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument("-u", "--update", help = "Update the tests outputs", action = "store_true")
parser.add_argument("-d", "--dir", help = "Directory to run tests in", default = "tests")
args = parser.parse_args()

BASE_DIR = path.dirname(__file__)
os.chdir(BASE_DIR)

PYR_BINARY = path.join(BASE_DIR, "target", "debug", "pyr")
TEST_DIR = path.join(BASE_DIR, args.dir)

PYR_EXT = ".pyr"
RECORD_EXT = ".record"

EXIT_RUN_PASS = 0
EXIT_RUN_FAIL = 1

passing = True
n_fail = 0
n_pass = 0 

def fail():
  global passing
  passing = True
  n_fail += 1

def relative_path(path):
  return str(path).replace(BASE_DIR, ".")

def test_file(path):
  output = subprocess.run([PYR_BINARY, path], capture_output = True)
  output_file = pathlib.Path(path).with_suffix(RECORD_EXT)

  def decode(stream):
    stream = stream.decode()
    output = stream.replace(BASE_DIR, "$DD")
    return output

  exitcode = output.returncode
  stdout = decode(output.stdout)
  stderr = decode(output.stderr)
  
  file_path_relative = relative_path(output_file)
  if args.update:
    with open(output_file, "w") as f:
      f.write(f":code {exitcode}:\n")
      f.write(f":stdout {len(stdout)}:\n")
      f.write(stdout)
      f.write("\n")
      f.write(f":stderr {len(stderr)}:\n")
      f.write(stderr)
      f.write("\n")
      f.write(f":end")
      print(f"\x1b[2;96m[INFO]\x1b[0m: {file_path_relative} updated.")
  else:
    with open(output_file, "r") as f:
      expected = f.read();

      assert expected.__contains__(f":code "), "Broken record file. Missing `code` tag."
      assert expected.__contains__(f":stdout "), "Broken record file. Missing `stdout` tag."
      assert expected.__contains__(f":stderr "), "Broken record file. Missing `stderr` tag."
      assert expected.__contains__(f":end"), "Broken record file. Missing `end` tag."

      expected_exitcode = int(expected.split(":code ")[1].split(":", 1)[0])
      expected_stdout_len = int(expected.split(":stdout ")[1].split(":", 1)[0])
      expected_stdout = expected.split(":stdout ")[1].split(":\n", 1)[1].split("\n:stderr ", 1)[0]
      expected_stderr_len = int(expected.split(":stderr ")[1].split(":", 1)[0])
      expected_stderr = expected.split(":stderr ")[1].split(":\n", 1)[1].split("\n:end", 1)[0]

      if exitcode == expected_exitcode and len(stdout) == expected_stdout_len and stdout == expected_stdout and len(stderr) == expected_stderr_len and stderr == expected_stderr:
        global n_pass
        print(f"\x1b[2;96m[INFO]\x1b[0m: {file_path_relative} passed.")
        n_pass += 1
      else:
        global n_fail
        print(f"\x1b[1;31m[ERR]\x1b[0m: {file_path_relative} failed.", file = sys.stderr)
        print(f"\x1b[1;31m[ERR]\x1b[0m: Expected:", file = sys.stderr)
        print(f"  exit code: {expected_exitcode}", file = sys.stderr)
        print(f"  stdout: \n{expected_stdout}", file = sys.stderr)
        print(f"  stderr: \n{expected_stderr}", file = sys.stderr)
        print(f"\x1b[1;31m[ERR]\x1b[0m: Got:", file = sys.stderr)
        print(f"  exit code: {exitcode}", file = sys.stderr)
        print(f"  stdout: \n{stdout}", file = sys.stderr)
        print(f"  stderr: \n{stderr}", file = sys.stderr)
        n_fail += 1

for entry in os.scandir(TEST_DIR):
  if entry.is_file() and entry.path.endswith(PYR_EXT):
    test_file(entry.path)

print()
print(f"\x1b[2;96m[INFO]\x1b[0m: {n_pass + n_fail} tested.")
print(f"\x1b[2;96m[INFO]\x1b[0m: {n_pass} passed.")
print(f"\x1b[2;96m[INFO]\x1b[0m: {n_fail} failed.")

if not passing:
  exit(1)
