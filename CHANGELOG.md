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

### Fixed

- Test literals that were silently truncated by their array length; the
  conflict-rule test now checks for `ERR_ARGUMENT_CONFLICT`.
