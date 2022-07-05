#!/usr/bin/env python3

from re import sub
import subprocess

if __name__ == "__main__":
  output = subprocess.run(["cargo", "ntest"])
  if output.returncode != 0:
    exit(1)
  output = subprocess.run(["python3", "./test.py", "-d", "examples", "-D", "--always-build"])
  if output.returncode != 0:
    exit(1)
  output = subprocess.run(["python3", "./test.py", "-d", "tests", "-D", "--always-build"])
  if output.returncode != 0:
    exit(1)
