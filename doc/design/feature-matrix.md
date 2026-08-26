# Phase 0 feature and compatibility matrix

## Purpose

This document records the behavior found in the legacy implementation, tests,
and user documentation, and assigns every feature to an implementation scope.
It is the feature baseline for the modular rewrite; it is not a claim that the
legacy implementation behaves correctly in every edge case.

The historical inventory was taken from:

- `old/fclap_parser.f90`
- `old/fclap_actions.f90`
- `old/fclap_namespace.f90`
- `old/fclap_formatter.f90`
- `test/unit/old_main.f90`
- `docs/source/tutorial.rst`
- `docs/source/api/fclap.rst`
- The incomplete modules under `src/fclap/`

The `old/` sources and procedural test driver were removed in Phase 8 after
every retained behavior was mapped to a named Test Drive case in
`docs/design/legacy-test-map.md`.

## Scope labels

- **Initial**: required before the first stable single-parser release.
- **Later**: architecturally anticipated, but implemented after the initial
  single-parser release.
- **Unsupported**: deliberately excluded unless a later design decision changes
  the contract.

## Parser construction and registration

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| Explicit program name | Implemented and exercised | Initial | `prog=` overrides the name obtained from command argument zero. |
| Automatic program name | Implemented | Initial | Move platform path stripping to `utils/system.f90`. |
| Description and epilog | Implemented and documented | Initial | Stored verbatim after trimming fixed-length padding. |
| Custom usage text | Present only in the new skeleton | Initial | Overrides generated usage content while retaining the formatter's `usage:` prefix. |
| Automatic `-h`, `--help` | Implemented | Initial | Registered with a concrete `HelpAction`; aliases must participate in collision checks. |
| Automatic `--version` | Implemented | Initial | Added only when `version=` is supplied. |
| Custom formatter | Sketched in new implementation | Initial | Accept a polymorphic `FormatterType` and store a clone. |
| Reinitializing a parser | Legacy `init` clears state | Initial | `init` resets arguments, groups, configuration errors, and parser metadata. |
| Parent parsers | Implemented and lightly tested | Later | Implemented in Phase 7. `init_with_parents` deep-copies arguments, actions, validators, and groups; automatic help/version and parent subcommand trees are not inherited. Conflicts are fatal rather than silently replaced. |
| Configuration errors | Legacy keeps one `last_error` | Initial | Accumulate structured configuration errors and never commit an invalid argument. |
| Unlimited registered arguments | Legacy has fixed limits | Initial | Use allocatable arrays; allocation failure is outside normal parser error handling. |

## Argument identity and metadata

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| Positional arguments | Implemented and tested | Initial | Exactly one non-option name; consumed in registration order. |
| Optional arguments | Implemented and tested | Initial | Every alias begins with `-`; aliases are case-sensitive. |
| Up to four aliases in facade | Implemented | Initial | Retain `name1` through `name4` initially; store names dynamically without an internal four-name limit. |
| Array-of-names registration | Not present | Later | May supplement the four-name facade when compiler support and ergonomics are evaluated. |
| Automatic destination | Implemented | Initial | Prefer the longest `--long` alias, otherwise the longest alias; strip leading hyphens and replace internal hyphens with underscores. Ties select the first registered alias. |
| Explicit destination | Implemented | Initial | Must be non-empty and unique in a parser. |
| Metavar | Implemented and tested | Initial | Default is the upper-case destination. |
| Required option | Implemented; fail test disabled | Initial | Presence means explicitly supplied, regardless of whether a default exists. |
| Required positional | Implemented | Initial | Derived from minimum `nargs`; `?` and `*` positionals are optional. |
| Hidden argument | Implemented and tested | Initial | Hidden from usage/help, but still parsed normally. |
| Deprecated argument | Implemented by printing directly | Initial | Append a warning entry when explicitly used; no direct output from the parse engine. |
| Removed argument | Implemented; fail test disabled | Initial | Produce a fatal parse error and do not execute its action. |
| Duplicate option alias | Not validated consistently | Initial | Fatal registration error. |
| Duplicate destination | Parent copying can replace by destination | Initial | Fatal for normal registration and parent copying; inherited definitions are never silently replaced. |
| Mixed positional/optional aliases | Not rejected | Initial | Fatal registration error. |
| Empty or special names | Not validated | Initial | Empty names, `-`, and `--` are invalid argument names. `--` is reserved as a parse marker. |

## Value types, defaults, and choices

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| String values | Implemented and tested | Initial | Canonical name `string`; default when no type is specified. |
| Integer values | Implemented and tested | Initial | Canonical name `integer`; accept `int` as an alias. Initially use `ip`. |
| Real values | Implemented and tested | Initial | Canonical name `real`; accept `float` and `double` aliases. Initially use `wp`. |
| Logical values | Partially implemented | Initial | Canonical name `logical`; accept `bool`. Tokens are case-insensitive `true`, `false`, `t`, `f`, `1`, `0`, `yes`, `no`, `on`, and `off`. |
| Typed defaults | Implemented and tested | Initial | Convert and validate at registration; the intrinsic type category must match, while supported kinds are converted with range checking. |
| Character defaults for numeric types | Legacy parses strings | Initial | Retained as a convenience; failure is a configuration error. |
| Missing optional value without default | Legacy omits namespace key | Initial | Keep the key absent except for actions with defined implicit defaults. |
| String choices | Implemented | Initial | Compare case-sensitively after token conversion. |
| Numeric choices | Implemented by raw strings | Initial | Compare converted typed values, so integer spellings such as `01` and `1` are equivalent. |
| Logical choices | Not meaningfully covered | Initial | Compare converted logical values. |
| Default satisfies choices/validators | Not consistently checked | Initial | Required at registration. |
| Print default toggle | Implemented and extensively tested | Initial | Per-argument default is true under `StandardFormatter`. |
| Print choices toggle | Implemented and tested | Initial | Per-argument default is false under `StandardFormatter`. |
| Multiple integer/real kinds | Discussed but not implemented | Later | Add concrete value/converter variants after default-kind behavior is stable. |
| Arbitrary user-defined value types | Not present | Later | Requires a public converter/value construction contract. |

## Nargs and token consumption

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| Zero values | Used internally by flag actions | Initial | Valid only for actions that do not consume values. |
| One value | Implemented | Initial | Default for `StoreAction` and `AppendAction`. |
| Exact positive count | Implemented but not tested | Initial | Store a list when count is greater than one. |
| `?` zero or one | Implemented but not tested | Initial | Optional options require `const=` for the present-without-value case; optional positionals use their default or remain absent. |
| `*` zero or more | Implemented but not tested | Initial | Produces a list. A present option with no values produces an empty list. |
| `+` one or more | Implemented but not tested | Initial | Produces a list and fails when no value is available. |
| Remainder | Implemented but not tested | Initial | Consumes every remaining token and must be the final positional consumer. |
| Integer and character public spelling | New skeleton uses `class(*)` | Initial | Prefer generic facade wrappers for `nargs=2` and `nargs="+"`, normalized immediately by `fclap_nargs`. |
| `--` end-of-options marker | Absent | Initial | Marker is not stored; remaining tokens are positional even when they begin with `-`. |
| Negative numeric values | Legacy misclassifies them as options | Initial | When a numeric value is expected, a valid signed numeric token is a value. A known option still terminates variable consumption. |
| Variable positional followed by positionals | Legacy consumes greedily | Initial | Reserve the minimum cardinality required by later positional arguments. |
| Multiple ambiguous variable positionals | Undefined | Unsupported | Registration rejects layouts without a deterministic allocation. |

## Actions and validators

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| Store | Implemented and tested | Initial | Default action; last repeated occurrence wins. |
| Store constant | Not exposed separately | Initial | Required primitive for `?`, booleans, and custom flags. |
| Store true | Implemented and tested | Initial | Implicit absent default is false unless explicitly overridden. |
| Store false | Implemented and tested | Initial | Implicit absent default is true unless explicitly overridden. |
| Count | Implemented and tested | Initial | Implicit absent default is zero; repeated occurrences increment. |
| Append | Implemented and tested | Initial | Repeated occurrences accumulate values in encounter order. |
| Help | Implemented by magic namespace key and `stop` | Initial | Return a successful help-request outcome. |
| Version | Implemented by direct output and `stop` | Initial | Return a successful version-request outcome. |
| Lower bound / `not_less_than` | Encoded into an action string | Initial | Recast as a `ValidatorType` factory operating on converted values. |
| Upper bound / `not_bigger_than` | Encoded into an action string | Initial | Recast as a `ValidatorType` factory operating on converted values. |
| String length bounds | Legacy overloads numeric bound actions | Later | Add explicit length validators rather than inferring behavior from `data_type`. |
| Custom actions | Not supported by old string dispatch | Initial | Supported through extension of `ActionType`. |
| Custom validators | Not supported | Initial | Supported through extension of `ValidatorType`; one validator per argument initially. |
| Validator composition | Not supported | Later | Add a composite validator instead of exposing heterogeneous arrays directly. |

## Parsing and results

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| Parse process arguments | Implemented | Initial | `parse_args()` is the argparse-like convenience operation. |
| Parse explicit token array | Implemented and used by tests | Initial | `parse_tokens()` is the non-terminating core-facing public operation. |
| Non-terminating process parse | Not present | Initial | `try_parse_args()` reads process arguments and returns `ParseResult`. |
| Structured parse result | Not present | Initial | Contains namespace, error stack, outcome, and optional help/version text. |
| Unknown option | Hard-exit path; disabled fail test | Initial | Fatal structured error. |
| Missing option value | Hard-exit path; disabled fail test | Initial | Fatal structured error. |
| Extra positional | Hard-exit path; disabled fail test | Initial | Fatal structured error. |
| Missing required argument | Hard-exit path; disabled fail test | Initial | Fatal structured error. |
| Repeated parse with one parser | Mutable legacy seen-state | Initial | Every call uses a fresh `ParseContext`; parser definition remains unchanged. |
| Parse known arguments | Not present | Later | May return unmatched tokens without weakening strict `parse_tokens()`. |
| Parse a shell command string | Not present | Unsupported | Shell quoting is platform-dependent; callers supply an already-tokenized array. |

## Namespace

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| Generic scalar `get` | Implemented | Initial | Generic by output type and capable of returning a status code. |
| Generic list `get` | Separate string/integer routines | Initial | Generic by output type and rank; returned list is allocatable. |
| `contains` / `has_key` | Implemented | Initial | Canonical public name is `contains`; retain `has_key` as an alias if inexpensive. |
| Merge | Implemented for subparsers | Later | Implemented in Phase 7 as a deep merge. Incoming child values replace parent values with the same key; unrelated parent values remain. |
| Show/debug print | Implemented | Later | Formatting convenience, not required by parsing. |
| Missing-key retrieval | Legacy returns a default value | Initial | Report `ERR_NAMESPACE_MISSING_KEY`; output remains unchanged. |
| Wrong-type retrieval | Legacy behavior is weakly defined | Initial | Report `ERR_NAMESPACE_TYPE_MISMATCH`; no implicit numeric conversion. |

## Help and formatting

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| Generated usage | Implemented | Initial | Deterministic and side-effect free. |
| Full help | Implemented and tested | Initial | Deterministic and side-effect free. |
| Print usage/help | Implemented | Initial | Thin output wrappers around formatting functions. |
| Positional/options sections | Implemented | Initial | Standard formatter uses `positional arguments:` and `options:` headings. |
| Default formatting | Extensively tested | Initial | Centralized in value types; no formatter-side type switches. |
| Choice formatting | Tested | Initial | String choices quoted; numeric/logical choices unquoted. |
| Width and wrapping | Fields exist; behavior limited | Initial | Default width 80 and deterministic wrapping. |
| Raw description formatter | Mentioned in skeleton | Later | Concrete formatter or configuration factory. |
| Raw text formatter | Mentioned in skeleton | Later | Concrete formatter or configuration factory. |
| Metavar type formatter | Mentioned in skeleton | Later | Concrete formatter or configuration factory. |
| Colored help | Absent | Unsupported | Reconsider only with explicit terminal-capability design. |

## Groups and subcommands

| Feature | Legacy state | Scope | Rewrite decision |
|---|---|---:|---|
| Help-only argument group | Implemented and tested | Initial | Store member argument indices. |
| Optional mutex group | Implemented; success tested | Initial | At most one explicitly seen member. |
| Required mutex group | Implemented; fail test disabled | Initial | Exactly one explicitly seen member. |
| Python-like group object facade | Not present | Later | Start with a stable `GroupHandle`; add `group%add_argument` only after lifetime tests. |
| One-level subparsers | Implemented and tested | Later | Implemented in Phase 7 with owned child-parser snapshots. |
| Nested subparsers | Suggested in docs/design only | Later | Implemented in Phase 7 through recursive facade dispatch and `selected_command_path`. |
| Required subparser | Not implemented | Later | Implemented as the explicit `required=` property of `add_subparsers`. |
| Subparser namespace merge | Implemented | Later | Implemented with deeper/child values taking precedence on duplicate keys. |
| Callback dispatch | Not implemented | Later | Convenience layer after hierarchical parsing. |

## Python argparse features deliberately not promised

| Feature | Scope | Reason |
|---|---:|---|
| Long-option abbreviation | Unsupported | Ambiguous interfaces can change when a new option is added. |
| `fromfile_prefix_chars` | Unsupported | File I/O and tokenization policy belong outside the core parser initially. |
| `argument_default` parser-wide value | Unsupported initially | Per-argument typed defaults are clearer; reconsider after the value model is stable. |
| Conflict handlers such as `resolve` | Unsupported initially | Duplicate aliases and destinations are configuration errors. |
| Arbitrary prefix characters | Unsupported initially | The initial grammar uses `-` and `--`. |
| Automatic shell parsing | Unsupported | The operating system or caller supplies tokens. |
| Python callback/callable `type` objects | Unsupported as such | Fortran uses typed converter/action/validator extension points. |

The following syntax is useful but deferred rather than rejected permanently:

- `--option=value`
- Combined short flags such as `-vvv` or `-abc`
- Attached short-option values such as `-I/usr/include`
- `parse_known_args`

## Legacy test migration inventory

The successful cases from the legacy procedural driver are normal Test Drive
tests. The former `test_nargs` placeholder was replaced by tests for every
supported form. The exact historical-routine mapping is frozen in
`docs/design/legacy-test-map.md`.

The disabled legacy failure cases become expected-failure assertions against
`ParseResult` and `ErrorStack`:

- Missing required positional.
- Missing required option.
- Missing option value.
- Invalid integer, real, and logical values.
- Invalid string and integer choices.
- Mutex conflict and missing required mutex member.
- Lower- and upper-bound violations.
- Unknown option.
- Extra positional.
- Unknown subcommand when subparsers are implemented.
- Removed argument use.
- Append action without a value.

Additional required failure coverage comes from gaps in the legacy suite:

- Invalid and duplicate argument definitions.
- Invalid `nargs` and action/arity combinations.
- Invalid defaults against choices and validators.
- Namespace missing-key and wrong-type access.
- Ambiguous positional layouts.
- Group handles from the wrong parser.

No expected-failure semantic test is allowed to call a parser path that exits
the `test-drive` process. Process-exit smoke tests, if needed, use small fixture
executables outside the semantic unit-test process.

## CI baseline

`.github/workflows/ci.yml` is active for pushes, pull requests, and manual
runs. It exercises debug/runtime-checked and optimized fpm builds, GNU CMake
and Meson builds, all Test Drive and process-policy tests, fpm-owned public
examples, installed-package consumers, source-manifest consistency, and a
warning-clean Sphinx build.

The operating-system and compiler expansion remains staged according to
`plan.md`: fpm is the primary cross-platform/compiler axis, while broader
CMake and Meson combinations are added on default-branch, scheduled, or
release workflows rather than multiplying every combination on each commit.
