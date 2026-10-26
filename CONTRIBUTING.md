# Contributing to fclap

First off, thank you for considering contributing to fclap.
Please take a moment to review this guidelines to make the contribution process
simple and effective for all involved.

Respecting these guidelines helps communicate that you respect the time of
the developers who manage and develop this open source project.
In return, they should return this respect by addressing your problem,
evaluating changes, and helping you handle your pull requests.


## Reporting a Bug

A bug is a _demonstratable problem_ caused by the code in this repository.
Good bug reports are extremely valuable for us - thank you!

Before opening a bug report:

1. Check if the issue has already been reported.
2. Check if it still is an issue or has already been fixed?
   Try to reproduce it with the latest version from the default branch.
3. Isolate the problem and create a reduced test case.

A good bug report should not leave others needing to chase you up for more
information. So please try to be as detailed as possible in your report,
answer at least these questions:

1. Which version of fclap are you using? The current version is always
   a subject to change, so be more specific.
2. What steps will reproduce the issue?
   We have to reproduce the issue, so we need all the input files.
3. What would be the expected outcome?
4. What did you see instead?

All these details will help people to fix any potential bugs.


## Suggesting a New Feature

Feature requests are welcome. But take a moment to find out if your idea fits
the scope and goals of the project. It is up to you to provide a strong
argument to convince the project's developers of the benefits of this feature.
Please provide as much detail and context as possible.


## Implementing a New Feature

Contributions are welcome via GitHub pull requests.

- Each pull request should implement _one_ feature or fix _one_ bug.
  If you want to add or fix more than one thing, submit more than one
  pull request.
- Do not commit changes to files that are irrelevant to your feature or
  bugfix (_e.g._ `.gitignore`).
- Be willing to accept criticism and work on improving your code.


### Building and Testing

fpm is the main development workflow; CMake and Meson must keep working too:

```sh
fpm test
fpm run --example basic -- input.dat --threads 2

cmake -S . -B _build -DCMAKE_BUILD_TYPE=Debug
cmake --build _build
ctest --test-dir _build --output-on-failure

meson setup _meson
meson test -C _meson --print-errorlogs
```

When you add or remove a source file, list it in `src/fclap/CMakeLists.txt`
and `meson.build` as well (`python3 config/check-manifests.py` checks this),
and register new test suites in `test/unit/main.f90`,
`test/unit/CMakeLists.txt`, and `test/unit/meson.build`.

- Expected failures are tested through `parse_tokens` by checking error codes
  and structured fields, never by calling `parse_args`, which terminates.
- Changes to the public interface update `doc/source/api/fclap.rst` and
  `doc/design/api-contract.md`; build the documentation with
  `sphinx-build -W -b html doc/source doc/build/html`.


### Pre-commit checks

Install the hooks once in your checkout:

```sh
python -m pip install 'pre-commit>=4,<5'
pre-commit install
pre-commit run --all-files
```

CI runs the same checks on every push and pull request. The hooks validate YAML
and TOML, detect merge conflicts, run Fortitude on the project's Fortran files,
and check the CMake and Meson source manifests. Fortitude uses the same rule
set as toml-f (`fortitude.toml`: all default and preview rules except C002,
C003, and C121, lines up to 132 columns) and fixes what it can automatically;
review and stage its changes before committing again. Its fix for inline `if`
continuations indents the new block by two spaces; re-indent it to the
project's four. Generated and third-party sources are excluded.

### Code coverage

The coverage CI job runs the CMake unit suite and CLI policy checks with GNU
Fortran instrumentation, then uploads a gcovr XML report to Codecov. Only files
under `src/` contribute to the reported coverage.

To reproduce it locally with GNU Fortran 14:

```sh
python -m pip install 'gcovr>=8.6,<9'
cmake -S . -B _coverage -GNinja \
  -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_Fortran_FLAGS="--coverage -O0"
cmake --build _coverage --parallel 2
ctest --test-dir _coverage --output-on-failure
gcovr --root . --filter src/ --gcov-executable gcov-14 \
  --include-internal-functions \
  --xml-pretty --output _coverage/coverage.xml --print-summary _coverage
```

Keep `--include-internal-functions`: gfortran names module procedures
`__module_MOD_name`, which gcovr otherwise mistakes for compiler internals and
drops from the report.

Use matching compiler and gcov versions. Remove the `_coverage` build directory
before starting a fresh measurement so old execution counts are not accumulated.

Maintainers must enable `chselz/fclap` in Codecov and grant its GitHub app
access to the repository for pull request comments and status checks. The
upload authenticates through GitHub OIDC, so no `CODECOV_TOKEN` secret is
needed. Pull requests from forks get no OIDC token; their upload may fail
without failing the job. The badge and
coverage comparisons become available after the first successful upload; upload
the default branch before expecting meaningful pull request comparisons.

### For New Contributors

If you never created a pull request before, welcome :tada:.
You can learn how from
[this great tutorial](https://egghead.io/series/how-to-contribute-to-an-open-source-project-on-github).

Don't know where to start?
You can start by looking through these
[help-wanted issues](https://github.com/chselz/fclap/issues).


## Sign Your Work

The sign-off is a simple line at the end of the explanation for a commit. All 
commits needs to be signed. Your signature certifies that you wrote the patch or
otherwise have the right to contribute the material. The rules are pretty simple,
if you can certify the below (from [developercertificate.org](https://developercertificate.org/)):

```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.

Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

Then you just add a line to every git commit message:

```
Signed-off-by: Joe Smith <joe.smith@example.com>
```

Use your real name (sorry, no pseudonyms or anonymous contributions.)

If you set your `user.name` and `user.email` git configs, you can sign your
commit automatically with `git commit -s`.
