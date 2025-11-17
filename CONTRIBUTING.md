# Contributing to xeus-haskell

Xeus and xeus-haskell are subprojects of Project Jupyter and subject to the
[Jupyter governance](https://github.com/jupyter/governance) and
[Code of conduct](https://github.com/jupyter/governance/blob/master/conduct/code_of_conduct.md).

## General Guidelines

For general documentation about contributing to Jupyter projects, see the
[Project Jupyter Contributor Documentation](https://jupyter.readthedocs.io/en/latest/contributor/content-contributor.html).

## Community

The Xeus team organizes public video meetings. The schedule for future meetings and
minutes of past meetings can be found on our
[team compass](https://jupyter-xeus.github.io/).

## Setting up a development environment

First, fork the project and install [Pixi](https://pixi.sh/) if you have not
already:

```bash
curl -fsSL https://pixi.sh/install.sh | sh
```

Then clone your fork and create the development environment using Pixi:

```bash
git clone https://github.com/<your-github-username>/xeus-haskell.git
cd xeus-haskell
pixi run -e dev prebuild
```

The `dev` environment defined in `pixi.toml` pulls in the toolchain (CMake,
compilers, pytest, etc.) so you do not need to manage those dependencies
manually.

## Building and installing xeus-haskell

```bash
pixi run -e dev build
pixi run -e dev install
```

## Running the tests

To run Python tests, from the build directory, type

```bash
pixi run -e dev ctest
pixi run -e dev pytest
```
