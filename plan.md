# fclap implementation plan

## Objective

Build a modular Fortran command-line argument parser with a user interface inspired by Python's `argparse`. The implementation should use plain data types for parser state and polymorphic abstract types only at genuine extension points such as actions, validators, groups, and formatters.

The code in `old/` is a behavioral reference, not a code base to port directly. Useful behavior must first be captured in tests and then reimplemented through the new module structure.

## Design principles

1. `ArgumentParser` is a facade. Registration, token consumption, conversion, validation, actions, and formatting belong in separate modules.
2. The parser owns one flat array of argument definitions. Groups store integer indices into that array and never own copies of arguments.
3. An `Argument` is predominantly metadata. It does not parse command lines or write to a namespace itself.
4. Actions describe how accepted values affect the namespace. Conversion and validation happen before an action executes.
5. The parsing core does not print, call `stop`, or call `error stop`. It returns a structured result containing the namespace, errors, and any help/version request.
6. Only the high-level `parse_args` convenience routine may implement argparse-like printing and termination. Non-terminating callers use `parse_tokens` or `try_parse_args` and receive a `ParseResult`.
7. Formatter implementations consume a data-only help model. They must not receive `class(*)` parser objects or depend on parser internals.
8. Abstract types are used where users may add alternative behavior. `Argument`, `Namespace`, and `ArgumentParser` remain concrete types.
9. New functionality is implemented as tested vertical slices. The project should remain buildable at every completed phase.

## Target module structure

```text
src/fclap/
|-- argparser.f90
|-- argument.f90
|-- nargs.f90
|-- namespace.f90
|-- value/
|   |-- abstract.f90
|   `-- builtin.f90
|-- actions/
|   |-- abstract.f90
|   |-- store.f90
|   |-- boolean.f90
|   |-- accumulate.f90
|   |-- control.f90
|   `-- builtin.f90
|-- validators/
|   |-- abstract.f90
|   |-- choices.f90
|   `-- bounds.f90
|-- groups/
|   |-- abstract.f90
|   |-- argument.f90
|   `-- mutex.f90
|-- parse/
|   |-- context.f90
|   |-- result.f90
|   |-- consumer.f90
|   `-- engine.f90
|-- formatter/
|   |-- model.f90
|   |-- abstract.f90
|   `-- standard.f90
|-- error/
|   |-- codes.f90
|   |-- entry.f90
|   `-- stack.f90
`-- utils/
    |-- accuracy.f90
    |-- system.f90
    `-- version.f90
```

Files should contain coherent families of related types. A separate file is not required for every very small concrete type.

## Type responsibilities

### Value model and namespace

Define an abstract `ValueType` for values stored in a namespace. Initial concrete implementations are:

- `StringValue`
- `IntegerValue`
- `RealValue`
- `LogicalValue`
- `ListValue`

Because Fortran cannot directly create a heterogeneous allocatable array of abstract values, use a small `ValueBox` containing `class(ValueType), allocatable :: item`. A `NamespaceEntry` contains a destination key and a `ValueBox`; `Namespace` owns an allocatable array of entries.

The namespace provides generic, type-safe `set`, `append`, and `get` bindings. Retrieval reports missing keys and type mismatches explicitly rather than silently returning arbitrary zero values. Initial numeric support may use the project's default integer kind and `wp` real kind; additional numeric kinds can be added after the parser behavior is stable.

### Argument definitions

`Argument` owns:

- All positional or optional names and aliases.
- The derived or explicit destination name.
- Normalized `nargs`.
- Conversion metadata.
- A polymorphic action.
- Zero or more polymorphic validators.
- A typed default value.
- Choices and help-display metadata.
- Required, visible, deprecated, and removed state.

Arguments are appended to the parser only after registration validation succeeds. Registration validates naming rules, alias and destination uniqueness, `nargs`, action compatibility, defaults, and choices.

Internal name storage must allocate using the longest supplied alias. Fixed limits such as `MAX_NAMES`, `MAX_ACTIONS`, and `MAX_CHOICES` should be removed in favor of allocatable arrays where practical.

### Nargs

`fclap_nargs` is the only module that defines and interprets normalized `nargs` values. It supports:

- No values.
- One value, which is the normal store default.
- An exact positive integer count.
- `?` for zero or one.
- `*` for zero or more.
- `+` for one or more.
- A remainder mode that consumes all remaining tokens.

The public interface should permit both `nargs=2` and `nargs="+"`. Prefer type-bound generic registration wrappers for integer and character `nargs`, both delegating to one normalized registration routine. If compiler portability makes those wrappers ambiguous, unlimited polymorphism may be used only at the public facade boundary and normalized immediately inside `fclap_nargs`.

### Actions

`ActionType` is abstract and defines how converted values change a namespace. It does not own argument names, destination names, type conversion rules, or help text.

Initial concrete actions are:

- `StoreAction`
- `StoreConstAction`
- `StoreTrueAction`
- `StoreFalseAction`
- `AppendAction`
- `CountAction`
- `HelpAction`
- `VersionAction`

Public factory functions such as `store_true()`, `append()`, and `count()` return the corresponding concrete type. `add_argument` accepts `class(ActionType), intent(in)` and stores a polymorphic copy with `allocate(..., source=action)`.

### Validators

`ValidatorType` is abstract and validates already-converted values without storing them. Initial concrete validators are:

- `ChoicesValidator`
- `LowerBoundValidator`
- `UpperBoundValidator`

The existing `not_less_than` and future `not_bigger_than` concepts belong here rather than in the action hierarchy. Their intended interface is, for example:

```fortran
call parser%add_argument("--threads", data_type="integer", default=1, &
                         validator=not_less_than(1))
```

An optional compatibility adapter for the former action-based spelling can be considered only after the new API is stable.

### Groups

`GroupType` is abstract and contains common title, description, and argument-index membership handling. Concrete types are:

- `ArgumentGroup`, used only to organize help output.
- `MutexGroup`, used to enforce at-most-one or exactly-one constraints.

Both types extend `GroupType` directly. A mutex group is not a subtype of a help-page group. Heterogeneous group storage uses a `GroupBox` with an allocatable polymorphic component.

Group validation receives the parse context's explicit `seen` state. A default value does not count as an argument supplied by the user.

Initially, group creation should return a stable integer handle accepted by `add_argument`. A more Python-like `group%add_argument(...)` handle can be introduced later only after pointer lifetime, parser copying, and container reallocation are tested across supported compilers.

### Formatters

`FormatterType` is abstract and declares `format_usage` and `format_help`. Both routines accept a concrete, data-only `HelpModel` containing lightweight parser, argument, group, and subcommand views.

`StandardFormatter` is the first concrete formatter. It should support:

- Usage generation.
- Positional and optional sections.
- Aliases, metavars, and `nargs` notation.
- Description and epilog text.
- Optional defaults and choices.
- Deprecation markers and hidden arguments.
- Groups, mutually exclusive notation, and later subcommands.
- Configurable output width and deterministic line wrapping.

Additional argparse-like formatters can be added as concrete types or factory-configured variants after the standard formatter is complete.

### Parse context and result

`ParseContext` holds all state for one parse operation:

- Input tokens and current cursor.
- Explicitly seen arguments.
- Positional argument cursor.
- Namespace under construction.
- Error stack.
- Selected subcommand path.

Keeping this state outside `ArgumentParser` permits safe repeated parsing with the same parser definition.

`ParseResult` contains:

- The parsed `Namespace`.
- The `ErrorStack`.
- An outcome code: success, failure, help requested, or version requested.
- Optional text associated with help or version outcomes.

The non-terminating parser entry point is conceptually:

```fortran
result = parser%parse_tokens(argv)
```

`parse_args` reads the process command line and applies the configured output and exit policy around the same core engine.

## Parsing pipeline

For each invocation, the parse engine performs these stages:

1. Refuse to parse if parser configuration contains fatal errors.
2. Create a fresh parse context and initialize typed defaults.
3. Classify each token using exact registered-option matching and positional state.
4. Honor `--` as the end-of-options marker.
5. Consume raw values according to `nargs`.
6. Convert raw tokens to typed values.
7. Validate choices, bounds, and other registered validators.
8. Execute the argument action.
9. Record that the argument was explicitly seen.
10. After token consumption, validate required arguments and groups.
11. Return a `ParseResult`; do not print or terminate from the engine.

Token classification must not assume that every token beginning with `-` is an option. In particular, negative integer and real values must be consumable when a value is expected.

## Implementation phases

### Phase 0: behavior inventory and API contract

Status: completed. The frozen Phase 0 artifacts are:

- [`docs/design/feature-matrix.md`](docs/design/feature-matrix.md), which records legacy behavior and classifies initial, later, and unsupported features.
- [`docs/design/api-contract.md`](docs/design/api-contract.md), which defines the initial public vocabulary, parsing outcomes, errors, and edge-case semantics.

1. Extract the useful behavior of `old/` into a feature matrix.
2. Classify features as initial release, later compatibility, or intentionally unsupported.
3. Freeze initial public spelling for parser initialization, argument registration, parsing, actions, validators, namespace access, and errors.
4. Record edge-case decisions for option aliases, duplicate destinations, optional values, defaults, negative numbers, and `--`.

Initial scope should include positionals, options, aliases, typed values, defaults, choices, required arguments, all supported `nargs` forms, built-in actions, help, version, and non-terminating error results.

Combined short options, option abbreviation, parent parsers, and subparsers may be deferred until the single-parser implementation is stable.

### Phase 1: compilable foundations

Status: completed. The Phase 1 foundation now includes normalized `nargs`,
structured errors, typed value boxes and lists, type-safe `Namespace`
accessors, corrected module/file names, and synchronized fpm/CMake/Meson source
graphs. The Test Drive runner covers successful and expected-failure results;
all 19 tests pass with each build system using GNU Fortran 13.3 and Test Drive
0.6.0.

1. Implement `fclap_nargs` and its unit tests.
2. Complete error codes, error entries, and the error stack.
3. Implement typed values, value boxes, namespace entries, and generic accessors.
4. Rename `namsspace.f90` to `namespace.f90` and correct the `flcap` module-name typo.
5. Replace stale CMake and Meson source lists with the new module graph.
6. Establish a minimal `test-drive` test executable that builds with fpm, CMake, and Meson.

Acceptance criterion: all foundation modules compile and their unit tests pass without requiring the parser.

### Phase 2: argument registration

Status: completed. Argument definitions now own dynamically allocated aliases,
normalized `nargs`, typed defaults, constants and choices, and polymorphic copies
of actions and validators. Registration is transactional: invalid definitions
append structured configuration errors without changing the parser's argument
array, while later valid registrations can still succeed. Built-in store, flag,
append, count, help, and version actions are implemented, and parser
initialization installs help and version automatically. The Test Drive suite now
contains 32 tests, including expected registration and conversion failures, and
passes through the fpm-built test executable, CMake/CTest, and Meson with GNU
Fortran 13.3 and Test Drive 0.6.0.

1. Complete `Argument` initialization and derived-name helpers.
2. Implement normalized registration in `ArgumentParser`.
3. Add dynamic argument-array growth.
4. Detect invalid names, duplicate aliases, duplicate destinations, invalid `nargs`, and incompatible actions.
5. Convert and validate defaults before committing the argument definition.
6. Add automatic help and version arguments using the appropriate concrete actions.

Acceptance criterion: parser definitions can be built and inspected, and invalid definitions produce structured configuration errors without partially modifying the parser.

### Phase 3: minimal vertical parse slice

Status: completed. `ParseContext` now owns all mutable state for one invocation,
and the public `ParseResult` returns an independent namespace, error stack, and
outcome. `ArgumentParser%parse_tokens` performs exact option and alias matching,
parses scalar positionals and options through their registered actions, applies
typed defaults without marking arguments seen, and accumulates post-parse
required errors. The engine reports unknown options, missing option values, and
extra positionals with structured token context; it also honors `--` and accepts
valid signed integer and real tokens where numeric values are expected. Nine new
Test Drive cases bring the suite to 41 passing tests through fpm, CMake/CTest,
and Meson with GNU Fortran 13.3. Multi-value and symbolic `nargs` consumption
was intentionally left for Phase 4.

1. Implement `ParseContext` and `ParseResult`.
2. Implement parsing from an explicit token array.
3. Support one string positional, one string option, aliases, and the default store action.
4. Apply defaults and validate required arguments.
5. Add unknown-option, missing-value, and extra-positional errors.
6. Implement `--` and negative-value handling from the start.

Acceptance criterion: a small real program can register arguments, parse an array, retrieve values, and inspect errors without any direct I/O or process termination from the core.

### Phase 4: actions, conversion, validation, and nargs parity

Status: completed. The parse engine now consumes every exact and symbolic
`nargs` form, creates typed empty lists, reserves the minimum cardinality of
later positionals, and treats remainder text verbatim after activation. String,
integer, real, and logical tokens are converted and checked per value before a
single atomic action call. Runtime choices use a concrete `ChoicesValidator`,
and the public `not_less_than` and `not_bigger_than` factories provide inclusive
integer and real bounds. Repeated store, constant, boolean, count, and append
actions are covered along with conversion, choice, validation, lifecycle, and
arity failures. Eleven new Test Drive cases bring the suite to 52 passing tests
through fpm, CMake/CTest, and Meson with GNU Fortran 13.3.

1. Add integer, real, and logical converters.
2. Add all exact and symbolic `nargs` forms.
3. Add store-const, boolean, count, and append actions.
4. Add choice and bound validators.
5. Add scalar and list namespace retrieval.
6. Cover repeated options, empty lists, exact-count failures, and conversion failures.

Acceptance criterion: all non-group, non-subparser behavior selected from the legacy feature matrix is represented by passing tests.

### Phase 5: help, version, and error policy

Status: completed. Formatters now consume a public, data-only `HelpModel`
snapshot through the deferred `FormatterType` contract. `StandardFormatter`
generates deterministic usage and help text with the required sections,
aliases, metavars, all `nargs` spellings, custom usage, quoted string defaults
and choices, visibility, lifecycle notes, and configurable wrapping. The
facade exposes side-effect-free `format_usage`, `format_help`, `parse_tokens`,
and `try_parse_args` paths plus output wrappers. Help and version text is carried
by `ParseResult`; only `parse_args` writes to the standard units and terminates,
using status zero for help/version and status two for fatal errors. A test-only
external formatter verifies the extension contract without private downcasts.
Eight new Test Drive cases bring the suite to 60 passing unit tests through
fpm, CMake/CTest, and Meson with GNU Fortran 13.3. Separate fixture processes
verify success, warning, help, version, and fatal output policy; CTest also
checks the exact fatal exit status.

1. Implement the help model.
2. Implement standard usage and help formatting.
3. Represent help and version as parse outcomes.
4. Implement high-level `parse_args` output behavior.
5. Implement the terminating `parse_args` policy only at the outer facade; structured entry points remain non-terminating.
6. Test custom usage, defaults, choices, visibility, deprecation, and line wrapping.

Acceptance criterion: formatting is deterministic and the same parse engine supports both argparse-like command-line behavior and non-terminating library use.

### Phase 6: argument and mutually exclusive groups

Status: completed. `GroupHandle` now carries a private parser owner identity
and stable group index, while heterogeneous `GroupBox` storage owns ordinary
`ArgumentGroup` and `MutexGroup` instances that both extend `GroupType`
directly. Arguments commit their index to a group only after all registration
validation succeeds; default, stale, and cross-parser handles produce
structured `ERR_INVALID_GROUP` configuration failures. Ordinary groups affect
only help organization through their concrete `help_snapshot` implementation.
The parser and engine dispatch through the abstract group protocol rather than
testing concrete dynamic types. Mutex groups produce grouped usage notation
and are validated by `MutexGroup%validate_presence` from explicit
`ParseContext%seen` state after parsing, so defaults do not satisfy required
groups and repeated use of one member does not conflict.
Help and version outcomes bypass post-parse group requirements. Seven new Test
Drive cases bring the suite to 67 passing tests through fpm, CMake/CTest, and
Meson with GNU Fortran 13.3, including structured expected failures for
optional conflicts, missing required choices, and invalid handles.

1. Implement group membership using argument indices.
2. Implement normal help groups.
3. Implement optional and required mutex validation.
4. Add group information to usage and help models.
5. Verify that explicit-presence checks are independent of namespace defaults.

Acceptance criterion: groups affect only their documented validation and formatting responsibilities.

### Phase 7: subparsers and parent parsers

Status: completed. Parsers now own deep snapshots of child parser trees and
dispatch recursively after the flat parse engine reports a recognized command
boundary. `ParseResult%selected_command_path` records the root-to-leaf path;
child namespaces deep-merge into parent results with child values taking
precedence, while the selected name is also stored under each collection's
configured destination. Required and unknown commands use structured
`ERR_MISSING_SUBCOMMAND` and `ERR_UNKNOWN_SUBCOMMAND` failures. Standard help
models and formatting include command choices, summaries, titles, and
descriptions. `init_with_parents` deep-copies non-special arguments, concrete
actions, validators, defaults, and remapped groups, rejects inheritance
conflicts, and remains independent after the source parent is reinitialized.
Ten subparser cases plus a namespace-merge case bring the suite to 78 Test
Drive tests, including nested dispatch, namespace precedence, owned-child
mutation isolation, child-local groups/formatters/errors, expected command
failures, help snapshots, and deep parent copies.

The complete suite passes with fpm 0.12.0, a fresh CMake/CTest build, and a
fresh Meson build using GNU Fortran 13.3; the existing CLI fixture policies,
including the expected status-two failure process, continue to pass.

1. Add hierarchical child-parser ownership.
2. Dispatch remaining tokens recursively to the selected child.
3. Record the selected command name and command path.
4. Merge child namespace values into the returned result according to a documented policy.
5. Give every child independent groups, formatters, and errors.
6. Add parent-parser copying with deep-copy tests for polymorphic components.

Acceptance criterion: nested subparsers work without adding subcommand-specific branches to the normal option parser.

### Phase 8: documentation, compatibility, and cleanup

Status: completed. Three facade-only parser examples replace the placeholder
stdlib program and are built and smoke-tested by fpm. The tutorial and public
API reference now use the tested factories, handles, result types, parent
copying, and subcommand interfaces. A migration guide and explicit legacy
routine-to-Test-Drive map cover every retained success and formerly disabled
failure behavior; after all 78 mapped tests passed, `old/` and its fixed-size
capacity constants were removed. Temporary error-code aliases and obsolete
planning fragments were also removed.

Package metadata now installs the fpm library, CMake exports only
`fclap::fclap`, Meson filters installed modules to project-owned compiler
dependencies, and standalone CMake/pkg-config consumers compile using only the
`fclap` facade. The active CI baseline checks debug and release fpm builds,
CMake, Meson, fpm-owned examples, installation consumers, source manifests, process
exit policies, and Sphinx documentation. The documented OS/compiler expansion
remains the long-term rollout plan.

Final Phase 8 verification passed all 78 Test Drive cases under GNU runtime
checks, three fpm example smoke runs, two fresh CTest tests, six fresh Meson
tests including one expected-failure process, fpm/CMake/Meson installation,
separate CMake and pkg-config consumers, the 31-source manifest audit, and a
Sphinx build with warnings treated as errors.

1. Replace the placeholder example with compiling parser examples.
2. Update the tutorial and API reference from tested public interfaces.
3. Add a migration guide for behavior that differed in `old/`.
4. Remove fixed-size legacy constants no longer needed.
5. Remove `old/` only after every retained behavior has a corresponding test.
6. Confirm that installed module files and package metadata expose only the supported public API.

## Testing strategy

All Fortran unit and parser-behavior tests must use the `test-drive` framework. The current procedural legacy test program should be split into focused test modules collected by one test driver.

Recommended suites are:

```text
test/unit/
|-- main.f90
|-- test_nargs.f90
|-- test_errors.f90
|-- test_values.f90
|-- test_namespace.f90
|-- test_argument.f90
|-- test_actions.f90
|-- test_validators.f90
|-- test_registration.f90
|-- test_parse_engine.f90
|-- test_formatter.f90
|-- test_groups.f90
`-- test_subparsers.f90
```

Each test should exercise one observable contract. Tests should not depend on printed diagnostics when the same behavior can be asserted through `ParseResult` and `ErrorStack`.

### Expected-failure coverage

"Expected failure" means that invalid input is deliberately supplied and the test passes only when the parser returns the expected structured failure. It does not mean leaving a test marked as a known failing or skipped test.

Expected-failure cases include at least:

- Empty or malformed argument names.
- Duplicate aliases and duplicate destinations.
- Invalid integer and character `nargs` specifications.
- Incompatible action and `nargs` combinations.
- Default values whose dynamic type does not match the declared argument type.
- Defaults that violate choices or validators.
- Unknown options.
- Missing option values.
- Missing required positionals and required options.
- Extra positional arguments.
- Invalid integer, real, and logical conversions.
- Invalid choices for every supported value type.
- Exact-count, one-or-more, and optional-value consumption errors.
- Mutually exclusive conflicts.
- Missing members of required mutually exclusive groups.
- Removed arguments.
- Unknown and missing required subcommands.
- Namespace missing-key and wrong-type retrieval.
- Unsupported numeric kinds when kind-specific support is added.

These tests call the non-terminating `parse_tokens` interface, then assert:

- The parse outcome is failure.
- The error stack contains the expected number and severity of entries.
- The expected stable error code is present.
- The relevant argument name is attached.
- The namespace does not contain a partially converted or invalid value.

Help and version requests are tested as successful non-error outcomes. Tests for the high-level terminating facade should be few and isolated because a direct `error stop` cannot be safely caught inside the same `test-drive` process. If process exit codes must be verified, build small fixture executables driven by the build system while keeping all parser semantics covered by `test-drive`.

### Test quality requirements

1. Every bug fix adds a regression test before or with the fix.
2. Every new concrete action, validator, group, or formatter has direct unit tests and at least one parser integration test.
3. Formatter tests compare complete deterministic strings, including newlines and indentation.
4. Parsing tests use explicit token arrays and never depend on the developer's actual command line.
5. Tests cover empty arrays, single tokens, long token lists, repeated parsing with one parser, and parsing with two independent parsers.
6. Public documentation examples are compiled as part of testing where practical.
7. No semantic test is disabled merely because the legacy implementation terminated the process.

## Build-system verification

fpm is the primary developer workflow, but CMake and Meson are supported interfaces and must compile the same source and test sets.

At every phase boundary, verify:

```text
fpm:    build and test
CMake:  configure, build, and ctest
Meson:  setup, compile, and test
```

Source lists and compiler options should be generated from a single documented module inventory where possible. If generation is not practical, a CI check should detect when a Fortran source is absent from either the CMake or Meson manifest.

## Long-term continuous integration matrix

Establish CI after the first compilable vertical slice. Expand it gradually so early development is not blocked by a large unstable matrix.

### Initial CI

- Linux with GNU Fortran.
- fpm build and `test-drive` tests on every push and pull request.
- A debug configuration with runtime checks, warnings, and backtraces.
- A release configuration to catch optimization-sensitive behavior.

### Expanded operating-system and compiler matrix

Target operating systems:

- Linux
- macOS
- Windows

Target compilers, subject to availability on each runner:

- GNU Fortran (`gfortran`)
- Intel Fortran (`ifx`; retain `ifort` only while it remains relevant)
- LLVM Flang
- NVIDIA HPC SDK Fortran (`nvfortran`) as an optional scheduled job

Compiler jobs should state and enforce the minimum supported versions. Unsupported compiler/OS combinations should not be added merely to make the matrix rectangular.

### Build-system matrix

Exercise:

- fpm
- CMake plus CTest
- Meson plus its test runner

Avoid running every build system against every compiler and OS on every commit. A practical matrix is:

1. Run fpm with all primary compilers and operating systems.
2. Run CMake and Meson with GNU Fortran on Linux for every pull request.
3. Run additional CMake/Meson compiler and OS combinations on the default branch or a scheduled workflow.
4. Run the full supported matrix on release tags.

### Additional CI jobs

- Documentation build and link checking.
- Formatting or lint checks once project formatting rules are selected.
- Example-program compilation and smoke tests.
- Installation followed by compilation of a separate consumer project.
- Build-manifest consistency checks.
- Code coverage on one Linux GNU job.
- Sanitizer or compiler runtime-check jobs where supported.

CI should fail on warnings only in explicitly designated strict jobs until warning behavior is understood across compilers.

## Initial public-interface target

The first stable interface should support code resembling:

```fortran
program example
    use fclap, only : ArgumentParser, Namespace, store_true, not_less_than
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: args

    call parser%init(prog="solver", description="Run a calculation")
    call parser%add_argument("input", help="Input file")
    call parser%add_argument("-v", "--verbose", action=store_true(), &
                             help="Enable verbose output")
    call parser%add_argument("--threads", data_type="integer", default=1, &
                             validator=not_less_than(1))

    args = parser%parse_args()
end program example
```

Applications that must not terminate use `parse_tokens` or `try_parse_args` and inspect the returned `ParseResult`, as defined by the Phase 0 API contract.

## Completion criteria

The modular rewrite is complete when:

1. fpm, CMake, and Meson all build the library and run the `test-drive` tests.
2. The core parsing engine performs no direct printing or termination.
3. All retained success and expected-failure behavior from `old/` is represented by tests.
4. Reusing a parser for multiple explicit token arrays is safe.
5. Defaults preserve their declared types and do not count as explicitly supplied values.
6. `nargs`, `--`, and negative numeric values behave consistently.
7. Custom actions, validators, groups, and formatters can be added without modifying the token-consumption engine.
8. Help output is deterministic and snapshot-tested.
9. Public examples compile against only the `fclap` facade module.
10. The initial CI workflow passes, with the expanded OS/compiler/build-system matrix documented and scheduled for incremental rollout.
