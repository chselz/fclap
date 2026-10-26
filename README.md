# fclap: Command Line Argument Parser Library for Fortran projects

[![License](https://img.shields.io/badge/license-MIT-blue)](LICENSE.md)
[![Release](https://img.shields.io/github/v/release/chselz/fclap)](https://github.com/chselz/fclap/releases/latest)
[![Build](https://github.com/chselz/fclap/actions/workflows/ci.yml/badge.svg)](https://github.com/chselz/fclap/actions/workflows/ci.yml)
[![Documentation Status](https://readthedocs.org/projects/fclap/badge/?version=latest)](https://fclap.readthedocs.io/en/latest/)
[![codecov](https://codecov.io/gh/chselz/fclap/graph/badge.svg)](https://codecov.io/gh/chselz/fclap)
[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit)](https://github.com/chselz/fclap/blob/main/.pre-commit-config.yaml)

fclap is a modular Fortran 2018 command-line argument parser inspired by
Python's `argparse` usage style and partly by Rust's clap regarding features.
It provides typed values, generated help, structured
non-terminating errors, groups, validators, parent parsers, and nested
subcommands.

Command lines use the familiar spellings: `--threads=4`, `-j4`, combined
flags such as `-vvn`, `--` to end options, and `-` as an ordinary value.
Numbers are parsed strictly, integers have 64-bit range, and diagnostics read
like `prog: error: argument -j/--threads: value must not be less than 1 (got '0')`.
`--feature/--no-feature` flags come from `boolean_optional()`, and
`parser%error("...")` reports your own post-parse checks the same way.

The supported application interface is the `fclap` facade module. 

```fortran
program demo
    use fclap, only : ArgumentParser, Namespace, store_true
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: args

    call parser%init(prog="demo", description="Process a file")
    call parser%add_argument("input", help="input file")
    call parser%add_argument("-v", "--verbose", action=store_true(), &
        help="enable verbose output")
    args = parser%parse_args()
end program demo
```

Three complete example programs demonstrating various capabilities of `fclap` are available:

- [`example/basic.f90`](example/basic.f90) demonstrates the terminating CLI
  facade, typed retrieval, a flag, a default, and a validator.
- [`example/subcommands.f90`](example/subcommands.f90) demonstrates owned
  child parsers and required subcommands.
- [`example/library_mode.f90`](example/library_mode.f90) demonstrates
  non-terminating parsing and structured failure inspection.

## Installation

fclap is not packaged for any distribution channel yet; build it from source
or add it as an fpm, CMake, or Meson dependency as described below.

### Building from source
To build this project from the source code in this repository you need to have

- a Fortran compiler supporting Fortran 2018

- One of the supported build systems

  - [meson](https://mesonbuild.com) version 0.55 or newer
  - [CMake](https://cmake.org/) version 3.15 or newer
  - [Fortran package manager (fpm)](https://github.com/fortran-lang/fpm) version 0.2.0 or newer

Get the source by cloning the repository

```
git clone https://github.com/chselz/fclap
cd fclap
```
Tests use [test-drive](https://github.com/fortran-lang/test-drive). CMake and Meson may
resolve that dependency when no installed package is available.


#### Building with meson

Meson provides the same library and test coverage:

```sh
meson setup _build
meson compile -C _build
meson test -C _build --print-errorlogs
meson install -C _build
```

#### Building with CMake

CMake builds the library, Test Drive suite, and CLI policy fixtures:

```sh
cmake -S . -B _build -DCMAKE_BUILD_TYPE=Debug
cmake --build _build
ctest --test-dir _build --output-on-failure
cmake --install _build --prefix /path/to/prefix
```

#### Building with fpm

The Fortran package manager ([fpm](https://github.com/fortran-lang/fpm)) supports the addition of fclap as a dependency.
In the package manifest, `fpm.toml`, you can add fclap dependency via:

```toml
[dependencies]
fclap.git = "https://github.com/chselz/fclap"
```

Then build and test normally.

```sh
fpm build
fpm test
```

Examples can be tested with

```sh
fpm run --example basic -- input.dat --threads 2 --verbose
```

## Documentation

Start with the [tutorial](doc/source/tutorial.rst); every public procedure is
described in the [API reference](doc/source/api/fclap.rst). Maintainers should
read [ARCHITECTURE.md](ARCHITECTURE.md) first.

Build the Sphinx documentation locally with:

```sh
python -m pip install -r doc/requirements.txt
sphinx-build -W -b html doc/source doc/build/html
```

## Contributing

This is a volunteer open source project and contributions are always welcome.
Please, take a moment to read the [contributing guidelines](CONTRIBUTING.md) on how to get involved in fclap.


## License

fclap is free software: you can redistribute it and/or modify it under the terms of the [MIT license](LICENSE.md).

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an _as is_ basis, without warranties or conditions of any kind, either express or implied. See the License for the specific language governing permissions and limitations under the License.
