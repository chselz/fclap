# fclap

fclap is a modular Fortran 2018 command-line argument parser inspired by
Python's `argparse`. It provides typed values, generated help, structured
non-terminating errors, groups, validators, parent parsers, and nested
subcommands.

Command lines use the familiar spellings: `--threads=4`, `-j4`, combined
flags such as `-vvn`, `--` to end options, and `-` as an ordinary value.
Numbers are parsed strictly, integers have 64-bit range, and diagnostics read
like `prog: error: argument -j/--threads: value must not be less than 1 (got '0')`.
`--feature/--no-feature` flags come from `boolean_optional()`, and
`parser%error("...")` reports your own post-parse checks the same way.

The supported application interface is the `fclap` facade module. Import it
with an `only` list; a bare `use fclap` shadows the `count` intrinsic:

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

Three complete programs are compiled and smoke-tested by fpm:

- [`example/basic.f90`](example/basic.f90) demonstrates the terminating CLI
  facade, typed retrieval, a flag, a default, and a validator.
- [`example/subcommands.f90`](example/subcommands.f90) demonstrates owned
  child parsers and required subcommands.
- [`example/library_mode.f90`](example/library_mode.f90) demonstrates
  non-terminating parsing and structured failure inspection.

## Build and test

fpm is the primary developer workflow:

```sh
fpm build
fpm test
fpm run --example basic -- input.dat --threads 2 --verbose
```

CMake builds the library, Test Drive suite, and CLI policy fixtures:

```sh
cmake -S . -B _build -DCMAKE_BUILD_TYPE=Debug
cmake --build _build
ctest --test-dir _build --output-on-failure
cmake --install _build --prefix /path/to/prefix
```

Meson provides the same library and test coverage:

```sh
meson setup _build
meson compile -C _build
meson test -C _build --print-errorlogs
meson install -C _build
```

The project requires a Fortran 2018 compiler. Tests use
[test-drive](https://github.com/fortran-lang/test-drive). CMake and Meson may
resolve that dependency when no installed package is available.

The full test suite and process-policy checks pass with these compilers on
Linux; older versions are untested:

| Compiler | Verified version | Notes |
|---|---|---|
| GNU Fortran | 13.3, 14 | also under ASan/UBSan and `-fcheck=all` |
| Intel ifx | 2026.1.1 | also under `-check all` |
| LLVM flang | 20.1.8 | conda-forge build; configure CMake with `-DTESTDRIVE_WITH_QP=OFF -DTESTDRIVE_WITH_XDP=OFF` because that runtime lacks quad-precision math (only test-drive needs it) |

After changing a public derived type, an incremental gfortran build can retain
incompatible module files. If compilation reports a derived-type component
mismatch between fclap modules, refresh the generated fpm outputs and retry:

```sh
fpm clean --skip
fpm test
```

## Documentation

The [tutorial](doc/source/tutorial.rst),
[public API reference](doc/source/api/fclap.rst), and
[migration guide](doc/source/migration.rst) are maintained alongside the
tested API contract. The detailed behavioral contract is in
[`doc/design/api-contract.md`](doc/design/api-contract.md).

Build the Sphinx documentation locally with:

```sh
python -m pip install -r doc/requirements.txt
sphinx-build -W -b html doc/source doc/build/html
```

## Contributing

Contributions are welcome. See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

fclap is distributed under the [MIT License](LICENSE.md).
