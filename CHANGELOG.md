# Changelog

All notable changes to fclap are recorded here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow
[Semantic Versioning](https://semver.org/).

## [Unreleased]

First release of the modular rewrite. The previous single-module
implementation is replaced; see `doc/design/feature-matrix.md` for every
feature and its decision.

### Added

- Modular architecture with a pure parse engine; only `parse_args` and
  `parse_known_args` print or stop (see `ARCHITECTURE.md`).
- Non-terminating library mode: `parse_tokens`, `parse_known_tokens`,
  `try_parse_args`, `try_parse_known_args`, returning `ParseResult`.
- Structured diagnostics with stable `ERR_*` codes, warnings, and
  "did you mean" suggestions; invalid definitions exit with status 70.
- Typed values (string, 64-bit integer, real64, logical) with `int32`/
  `real32` retrieval, `nargs` (`?`, `*`, `+`, counts, remainder), and the
  actions `store`, `store_const`, `store_true`, `store_false`, `append`,
  `count`, and `boolean_optional`.
- Validators `not_less_than`, `not_bigger_than`, `in_range`, and custom
  validators, actions, and formatters through abstract types.
- Help and mutually exclusive groups, `requires`/`conflicts`/
  `required_unless` rules, parent parsers, nested subcommands with aliases,
  global options, environment-variable fallback with value sources, value
  delimiters, per-choice help, and `help_on_empty`.
- `--option=value`, `-j4`, combined short flags, and `--`.
- `parse_tokens(..., lengths=)` and `parse_known_tokens(..., lengths=)` give
  the exact length of each token. `parse_args` passes the real lengths, so a
  value such as `--sep ", "` keeps its trailing blank and `""` stays an empty
  value.

### Changed

- `add_parser` takes `help=` instead of `help_text=`, matching
  `add_argument`.
- Registration rejects definitions that could never work: names or
  destinations containing blanks or `=`, `---` prefixes, single-dash names
  that read as negative numbers, zero-value actions or `nargs=0` on
  positionals, `required=.false.` on a positional needing a value, empty or
  repeated `choices`, choices failing the argument's validator, validators
  that accept nothing (inverted range, NaN bound), a child argument using its
  parent's subparser `dest`, a destination shared between levels with
  different types, and an empty required mutex group.
- A numeric-looking token (`-1.5e3`, `-9223372036854775809`) given where a
  number is expected is converted and reported as an invalid value instead
  of an unknown option; delimited numeric options accept `--l -1,2`.
- Real literals beyond the `real64` range are invalid values instead of
  infinity, and `get` into `real32` reports `ERR_NAMESPACE_OUT_OF_RANGE` for
  a nonzero value that would become zero.
- `real32` defaults, constants, choices, and bounds keep their decimal value
  (`0.1` is stored and shown as `0.1`); `store_const` and `new_value` accept
  `real32`.
- Delimited items are kept verbatim, an empty token is an empty list, and too
  many items or environment values are `ERR_UNEXPECTED_VALUE`. Value-count
  errors name both counts (`expected 2 values (got 1)`).
- An environment variable for a `count()` option holds the number of
  occurrences.
- Invalid choices list the allowed values, mutex errors no longer name hidden
  arguments, and errors from a subcommand are prefixed with its program name
  (`tool go: error:`).
- Fixed counts above ten are shown numbered in usage (`N1 N2 ... N100`); a
  custom `usage=` is printed verbatim; help wraps by UTF-8 characters, never
  ends lines in blanks, omits empty groups, and raises a `help_width` below
  20 to 20 instead of resetting it to 80.
- Values are copied with sourced allocation rather than through the
  `clone` binding, so a custom `ValueType` relying on its own `clone` for deep
  copies of pointer components is copied shallowly.

### Fixed

- Test literals that were silently truncated by their array length; the
  conflict-rule test now checks for `ERR_ARGUMENT_CONFLICT`.
- Every parse leaked the temporary of each copied value; long-running
  programs that reuse a parser no longer grow.
- A custom validator that rejected a default without setting `message`
  aborted the program in `add_argument`.
- `prog cmd --help` failed when the parent had an unmet required argument,
  group, or rule, and an error before the command made a later `--help` or
  `--version` meant for the command print the parent's text.
- A bad value no longer adds false "required argument was not supplied",
  required-group, or missing-subcommand errors, and repeated or mirrored
  conflict rules report a pair once.
- Quadratic time and memory: appending values, splitting delimited values,
  collecting unknown tokens, rendering usage for large fixed `nargs` (which
  hung for `nargs=huge(0)`), and wrapping long help text are now linear.
- `-vn=3` stores `3` like `-n=3`; `--l ""` for a delimited option no longer
  leaves a stray positional; a child argument no longer overwrites the
  selected command name.
- Empty `add_subparsers(title="")` and lifecycle messages no longer print a
  bare `:` or `(removed:)`; error tokens are reported verbatim.
