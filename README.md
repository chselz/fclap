i want to build something like this in fortran https://github.com/python/cpython/blob/3.14/Lib/argparse.py. However I want to add one or two things. First I want to add a status like active, deprecated und removed. if a certain keyword is then chosen either nothing happens, a warning is printed but its accepted or a message is printed that this feature has been removed. Another thing that should be added is a visibility status. it should be controllable via a bolean expression to control if this keyword is shown when the automatically generated help page is generated, so that I allow hidden keywords so to speak


# fclap
fclap, a Fortran command line argument parser.


This project is an effort to create a simple to use, modern library implementation of a command line argument parser for fortran projects.

## Building from source

To build *tblite* from the source code in this repository you need to have
a Fortran compiler supporting Fortran 2008 and one of the supported build systems:

- [meson](https://mesonbuild.com) version 0.57.2 or newer, with
  a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.10 or newer
- [cmake](https://cmake.org) version 3.14 or newer, with
  a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.10 or newer
- [fpm](https://github.com/fortran-lang/fpm) version 0.3.0 or newer

To build this project from the source code in this repository you need to have
- a Fortran compiler supporting Fortran 2008
- [meson](https://mesonbuild.com) version 0.57.2 or newer
- a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.10 or newer
- a LAPACK / BLAS provider, like MKL or OpenBLAS

Meson is the primary build system and provides feature-complete functionality of this project.
CMake and fpm support are available but the functionality of the project is limited.
Currently, *tblite* support GCC 8 and newer or Intel 18 and newer.

Detailed installation instruction are available in the project documentation under the [installation category](https://tblite.readthedocs.io/en/latest/installation.html).


#### Building with meson

Optional dependencies are
- asciidoctor to build the manual page
- C compiler to test the C-API and compile the Python extension module
- Python 3.6 or newer with the CFFI package installed to build the Python API

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
For more details see the [meson installation instructions](https://tblite.readthedocs.io/en/latest/installation.html#meson-based-build).

## Documentation

The user documentation is available at [readthedocs](https://fclap.readthedocs.io).
Additionally, the [doxygen](https://doxygen.nl) generated API documentation is available [here](https://fclap.github.io/fclap).

To build the user documentation locally we use sphinx, install the dependencies you can use the *mamba* package manager

```
mamba create -n sphinx --file doc/requirements.txt
mamba activate sphinx
```

The documentation is build with

```
sphinx-build doc _doc
```

You can inspect the generated documentation by starting a webserver

```
python3 -m http.server -d _doc
```

And open the down URL in a browser.


## Contributing

This is a volunteer open source projects and contributions are always welcome.
Please, take a moment to read the [contributing guidelines](CONTRIBUTING.md) on how to get involved in fclap.

## License

The MIT License (MIT)

Copyright (c) 2026 Christian Selzer

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.