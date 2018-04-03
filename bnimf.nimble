# Package

version       = "0.1.0"
author        = "Eric Lee"
description   = "Parser Generator for Nim"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 0.17.3"
requires "simple_graph"

# Tasks

task clean, "Clean project directory":
  exec "find . -name nimcache | xargs rm -rf"
  exec "rm -rf ./tests/runner"

task test, "Runs the test suite":
  exec "nim c -r tests/runner"
