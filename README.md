# fclap
fclap, a Fortran command line argument parser.


This project is an effort to create a simple to use, modern library implementation of a command line argument parser for fortran projects.
This project aims to replicate the functionalities of the python `Argparse` module.

**Important Note: This project is still under development so use with caution as it may break**

## Building from source

To build *fclap* from the source code in this repository you need to have
a Fortran compiler supporting Fortran 2008 and one of the supported build systems:

- [meson](https://mesonbuild.com) version 0.57.2 or newer, with
  a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.10 or newer
- [cmake](https://cmake.org) version 3.14 or newer, with
  a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.10 or newer
- [fpm](https://github.com/fortran-lang/fpm) version 0.3.0 or newer

fpm was used as the primary build system during development.
CMake and meson support are availible but the functionality may be limited
Currently, *fclap* support GCC 8 and newer or Intel 18 and newer.

Detailed installation instruction are available in the project documentation under the [installation category](https://fclap.readthedocs.io/en/latest/installation.html).


#### Building with meson

Setup a build with

```sh
meson setup _build
```

You can select the Fortran compiler by the `FC` environment variable.
To compile and run the projects testsuite use

```sh
meson test -C _build --print-errorlogs
```

To run the more extensive testing for the available parametrizations use

```sh
meson test -C _build --print-errorlogs --benchmark
```

If the testsuites pass you can install with

```sh
meson configure _build --prefix=/path/to/install
meson install -C _build
```

This might require administrator access depending on the chosen install prefix.
For more details see the [meson installation instructions](https://fclap.readthedocs.io/en/latest/installation.html#meson-based-build).

## Documentation

The user documentation is available at [readthedocs](https://fclap.readthedocs.io).
Additionally, the [doxygen](https://doxygen.nl) generated API documentation is available [here](https://fclap.github.io/fclap).

To build the user documentation locally we use sphinx, install the dependencies you can use the *mamba* package manager

```bash
mamba create -n fclap_docs python -y
mamba activate flacp_docs
```

Make sure to be in the docs directory and continue with:

```bash
pip install -r requirements.txt
make html
```

The build files for the documentation can be found in the docs/build folder.


## Contributing

This is a volunteer open source projects and contributions are always welcome.
Please, take a moment to read the [contributing guidelines](CONTRIBUTING.md) on how to get involved in fclap.

## License

The MIT License (MIT)

Copyright (c) 2026 Christian Selzer

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.