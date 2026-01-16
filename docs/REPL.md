# MicroHs REPL Mechanism

This document provides a technical overview of how the REPL (Read-Eval-Print Loop) is implemented in `xeus-haskell`.

## Overview

Unlike kernels based on GHC, `xeus-haskell` leverages **MicroHs**, a minimal Haskell implementation. The REPL provides an incremental execution environment where state (definitions and types) is preserved across multiple Jupyter cells.

## Core Components

1. **`src/Repl.hs`**: The heart of the REPL logic, written in Haskell and compiled with MicroHs. It handles module construction, compilation, and execution.
2. **`src/mhs_repl.cpp`**: A C++ bridge that communicates with the Haskell logic via the Foreign Function Interface (FFI). It also manages standard output redirection to capture execution results.
3. **`src/xinterpreter.cpp`**: The `xeus` interpreter implementation that orchestrates the kernel lifecycle and delegates execution requests to the C++ bridge.

---

## Execution Flow

When a user executes a cell in Jupyter, the following sequence occurs:

1. **Reception**: `xinterpreter::execute_request_impl` receives the code snippet.
2. **Dispatch**: The code is passed to `MicroHsRepl::execute`.
3. **Analysis**: The Haskell backend (`Repl.hs`) analyzes the snippet to determine if it is a **definition** or an **expression**.
4. **Wait for Result**: The C++ bridge redirects `stdout` to a pipe before calling the Haskell execution function.
5. **Return**: The captured output (and any errors) is sent back to the Jupyter frontend.

### 1. Definition vs. Expression Detection

The REPL uses the MicroHs parser to classify the input:

- **Definition**: Functions, data types, type aliases, or pattern bindings (e.g., `f x = x + 1` or `data Color = Red | Blue`).
- **Expression**: Values or IO actions to be evaluated (e.g., `f 10` or `putStrLn "hello"`).

### 2. State Maintenance (The persistent Context)

State is maintained through a persistent `ReplCtx` structure in `Repl.hs`:

```haskell
data ReplCtx = ReplCtx
  { rcFlags :: Flags      -- Compilation flags
  , rcCache :: Cache      -- MicroHs compiler cache
  , rcDefs  :: [StoredDef] -- List of user-defined snippets
  }
```

#### Handling Definitions

When a definition is entered:

1. **Name Extraction**: The kernel identifies which names (identifiers) are being defined.
2. **Shadowing**: If a previous definition exists with the same name, it is stripped from `rcDefs` to simulate shadowing.
3. **Incremental Module**: All stored definitions are concatenated into a temporary source file (the "Inline" module).
4. **Cache Update**: The module is re-compiled, and the internal compiler cache is updated.

#### Handling Expressions

When an expression is entered:

1. **Wrapping**: The expression is wrapped in a temporary `runResult` function inside the "Inline" module.
2. **Execution wrapper**: It uses `_printOrRun` to correctly handle both pure values (printing them) and `IO` actions (executing them).
3. **Evaluation**: The temporary module is compiled and executed in the current runtime state.

---

## Technical Details

### Standard Output Redirection

Since MicroHs writes directly to `stdout`, `xeus-haskell` implements a robust capture mechanism in C++. It creates a POSIX pipe (or Windows pipe) and duplicates the stdout file descriptor during the execution of a cell. This allows the kernel to intercept everything produced by `putStrLn`, `print`, or the `Display` system.

### Optimization: The Warmup Expression

During initialization, the `MicroHsRepl` constructor evaluates a trivial code snippet (`"0"`). This triggers the initial compilation of the `Prelude` and other standard libraries, caching them in the `Cache` to ensure subsequent cell executions are fast.

### Limitations

- **Selective Redefinition**: Because the kernel concatenates all previous definitions for every cell that adds a new definition, very large notebooks with hundreds of complex definitions may see a slight compilation slowdown.
- **MicroHs Boundaries**: Language features are limited to those supported by the MicroHs compiler (Haskell 2010 + some extensions). GHC-specific extensions (like `TypeApplications` or `GADTs`) are generally not available.

## Advanced Usage: The `Display` System

The REPL is integrated with the rich content protocol described in [COMMUNICATION.md](./COMMUNICATION.md). If an expression evaluates to a type that implements the `Display` typeclass, its output is automatically wrapped in the rich content markers and rendered as HTML, LaTeX, or Markdown in the notebook.
