#!/usr/bin/env python3

import subprocess

if __name__ == "__main__":
  subprocess.run(["python3", "./test.py", "-d", "examples", "-u", "-D", "--always-build"])
  subprocess.run(["python3", "./test.py", "-d", "tests", "-u", "-D", "--always-build"])
