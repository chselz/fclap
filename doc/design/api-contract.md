# Phase 0 public API and behavior contract

## Status of this contract

This document freezes the initial public vocabulary and observable behavior of
the modular rewrite. Procedure implementation details may change, but changes
to the names or semantics recorded here require an explicit design update and
corresponding test changes.

The initial contract covers ordinary and mutually exclusive argument groups.
Phase 7 extends the frozen surface with parent parsers, owned subparsers, and
recursive command dispatch as specified below.

## Public module

Normal applications import from `fclap` rather than internal modules.

The initial facade exports:

- `ArgumentParser`
- `Argument`
- `Namespace`
- `ParseResult`
- `GroupHandle`
- `ErrorEntry`
- `ErrorStack`
- `ActionType`
- `ValidatorType`
- `FormatterType`
- Built-in action factories.
- Built-in validator factories.
- Parse outcome, nargs, error, and severity constants.
- Help-model and concrete value types required by extension interfaces.
- The public storage kinds `ip` and `wp`.
- `get_fclap_version`, `fclap_version_string`, and
  `fclap_version_compact`.

Implementation-only containers such as polymorphic array boxes are private
unless they are required by the custom-action or custom-validator interfaces.

## Parser lifecycle

### Construction

A parser is declared and initialized explicitly:

```fortran
type(ArgumentParser) :: parser

call parser%init( &
    prog="solver", &
    usage="solver [options] INPUT", &
    description="Run a calculation", &
    epilog="See the manual for examples", &
    version="solver 1.2.0", &
    formatter=StandardFormatter(help_width=100), &
    add_help=.true. &
)
```

The conceptual signature is:

```fortran
subroutine init(self, prog, usage, description, epilog, version, &
                formatter, add_help)
```

All arguments after `self` are optional.

- `prog` defaults to the basename of command argument zero, or `program` when
  it cannot be obtained.
- `usage` is the usage body. The formatter supplies the `usage:` prefix.
- `description` and `epilog` default to absent, not allocated empty strings.
- `version` causes `--version` to be registered.
- `formatter` is cloned polymorphically. The default is `StandardFormatter`.
- `add_help` defaults to true and registers `-h` and `--help`.
- Calling `init` again clears the complete prior parser definition.

Parser components are private. Applications use type-bound procedures rather
than reading or modifying internal arrays.

`get_argument(index)` returns an owned `Argument` snapshot for a valid
one-based index. It permits definition inspection without exposing the
parser's internal arrays or returning a pointer into them.

There is deliberately no `exit_on_error` switch. The caller chooses between an
argparse-like terminating facade and a structured non-terminating parse entry
point. This avoids a function whose result contract changes according to a
boolean setting.

### Configuration validity

Registration routines never terminate. A rejected definition adds an entry to
the parser's configuration error stack and leaves all previously valid
definitions unchanged.

The public queries are:

```fortran
logical = parser%is_valid()
errors = parser%get_config_errors()
```

`init` clears configuration errors. A later valid registration does not clear
an earlier error. Parsing an invalid parser returns a failed `ParseResult`; the
result includes a copy of all configuration errors and consumes no tokens.

## Argument registration

### Common calls

```fortran
call parser%add_argument("input", help="Input file")

call parser%add_argument( &
    "-o", "--output", &
    default="result.dat", &
    metavar="FILE", &
    help="Output file" &
)

call parser%add_argument( &
    "-v", "--verbose", &
    action=store_true(), &
    help="Enable verbose output" &
)

call parser%add_argument( &
    "--threads", &
    data_type="integer", &
    default=1, &
    choices=[1, 2, 4, 8], &
    validator=not_less_than(1), &
    help="Number of worker threads" &
)

call parser%add_argument("files", nargs="+", help="Input files")
call parser%add_argument("--point", nargs=3, data_type="real")
```

The public keyword set is:

```text
name1                         required character
name2, name3, name4           optional character aliases
action                        optional class(ActionType)
nargs                         optional integer or character
data_type                     optional character
default                       optional intrinsic scalar or compatible list
const                         optional intrinsic scalar
choices                       optional homogeneous intrinsic array
validator                     optional class(ValidatorType)
required                      optional logical
help                          optional character
metavar                       optional character
dest                          optional character
visible                       optional logical
deprecated_msg                optional character
removed_msg                   optional character
print_default                 optional logical
print_choices                 optional logical
group                         optional GroupHandle
```

The integer- and character-`nargs` forms are facade overloads that immediately
create one normalized internal `NargsSpec`. Omitting `nargs` selects the
action's default arity.

`action` and `validator` dummies are non-allocatable `class(...)`, `intent(in)`
objects. The parser owns polymorphic clones created with `allocate(...,
source=...)`; it never retains pointers to caller-owned factory results.

Only one explicit validator is accepted initially. Choices are an independent
built-in constraint. Multiple general validators will later use a composite
validator factory so callers do not need to construct heterogeneous arrays.

### Names and destinations

- A positional definition has exactly one name and that name does not begin
  with `-`.
- Every alias of an optional definition begins with `-`.
- Empty names, `-`, and `--` are invalid.
- Positional and optional spellings cannot be mixed in one definition.
- Names and aliases are case-sensitive.
- An option alias must be unique across the parser.
- A destination must be non-empty and unique across the parser.
- An explicit `dest` is trimmed but otherwise retained.
- An implicit positional destination equals its name.
- An implicit optional destination is derived from the longest `--long` alias,
  or the longest alias when no long alias exists. A tie selects the first
  registered alias. Leading hyphens are removed and remaining hyphens become
  underscores.

For example, `-q, --dry-run` has destination `dry_run`.

### Data types

Accepted type names are:

| Canonical | Accepted aliases | Initial storage |
|---|---|---|
| `string` | `character`, `char` | Deferred-length character value |
| `integer` | `int` | `integer(ip)` |
| `real` | `float`, `double` | `real(wp)` |
| `logical` | `bool`, `boolean` | Default logical |

Type names are case-insensitive and normalized at registration. An unknown type
name is a configuration error. The default type is `string`.

Logical value tokens are case-insensitive. The accepted true spellings are
`true`, `t`, `1`, `yes`, and `on`; false spellings are `false`, `f`, `0`, `no`,
and `off`. The formatter emits `.true.` and `.false.` for logical values.

Additional integer and real kinds are deferred. The design must not prevent
adding kind-specific converters and value types later.

### Defaults and constants

- A non-character default must have the declared intrinsic type category.
  Supported integer or real kinds are converted to the initial storage kind
  with range checking. There is no implicit integer-to-real,
  real-to-integer, logical-to-numeric, or numeric-to-logical conversion.
- A character default may be parsed using the declared type as a convenience.
- Defaults are converted and checked against choices and validators during
  registration.
- An invalid default rejects the entire argument definition.
- A default inserted into a namespace is not marked explicitly seen.
- An absent optional argument without a default has no namespace entry.
- `store_true`, `store_false`, and `count` provide implicit defaults of false,
  true, and zero respectively, unless an explicit compatible default is given.
- `const` is required for an optional value-taking argument using `nargs="?"`.
  It is used when the option is present without a value.

`default` and `const` list support is restricted to types supported by the
initial `Namespace`. Exact accepted list forms are fixed while implementing the
value layer and must be tested before being documented as public examples.

### Required state

- Optional arguments default to `required=.false.`.
- Positionals with minimum cardinality at least one are required.
- Positional `?` and `*` arguments are not required.
- Explicit `required=.true.` is invalid for a positional whose `nargs` permits
  zero values.
- A required option is satisfied only by explicit command-line presence; its
  default does not satisfy the requirement.

### Visibility and lifecycle

- `visible` defaults to true and affects formatting only.
- Supplying `deprecated_msg` marks the argument deprecated. Explicit use adds a
  warning with `ERR_DEPRECATED_ARGUMENT` and parsing continues.
- Supplying `removed_msg` marks the argument removed. Explicit use adds a fatal
  `ERR_REMOVED_ARGUMENT` and its action is not executed.
- Supplying both lifecycle messages is a configuration error.
- Deprecation and removal state never apply merely because a default was used.

### Nargs semantics

| Public value | Minimum | Maximum | Result shape |
|---|---:|---:|---|
| `0` | 0 | 0 | Action-defined scalar/control |
| omitted / `1` | 1 | 1 | Scalar |
| integer `N > 1` | N | N | List |
| `?` | 0 | 1 | Scalar |
| `*` | 0 | unbounded | List |
| `+` | 1 | unbounded | List |
| `NARGS_REMAINDER` | 0 | all remaining | String or converted list |

- Negative integers and unsupported strings are configuration errors.
- Zero is valid only for actions declaring zero-value compatibility.
- Boolean, count, help, and version actions default to zero and reject
  incompatible explicit arity.
- `AppendAction` defaults to one but can append each value from a supported
  multi-value occurrence.
- A remainder consumer must be the last positional argument.
- Ambiguous combinations of multiple variable positional consumers are
  registration errors in the initial implementation.

## Built-in actions

The public factories are:

```fortran
store()
store_const(value)
store_true()
store_false()
append()
count()
```

Help and version action factories may remain private because normal users get
them through `init`.

Action behavior is:

- `StoreAction`: store the converted scalar or list; a repeated occurrence
  replaces the earlier value.
- `StoreConstAction`: store its typed constant and consume no values.
- `StoreTrueAction`: store true and consume no values.
- `StoreFalseAction`: store false and consume no values.
- `AppendAction`: append converted values in encounter order.
- `CountAction`: increment the current integer count and consume no values.
- `HelpAction`: request help and stop further token processing without error.
- `VersionAction`: request version output and stop further token processing
  without error.

The abstract action contract consists of:

1. An `apply` deferred binding receiving destination, converted values,
   namespace, action outcome, and error stack.
2. An overridable query for default `nargs`.
3. An overridable query validating explicit `nargs` compatibility.

Actions do not convert raw text, enforce choices, own argument names, format
help, print, or terminate the process.

## Built-in validators

The initial factories are generic over supported integer and real types:

```fortran
not_less_than(lower_bound)
not_bigger_than(upper_bound)
```

Choices are represented internally by a validator but remain a dedicated
`choices=` registration keyword because that interface is concise and their
values must also be available to formatters.

Validators receive converted values. A multi-value argument applies its normal
per-value validators to every value and reports the failing value and argument
destination. Validation failure prevents action execution.

String-length bounds are not inferred from numeric bound validators. Explicit
length validators may be added later.

## Groups

A group handle is obtained and passed during registration:

```fortran
type(GroupHandle) :: output_group, mode_group

output_group = parser%add_argument_group( &
    "output options", description="Control generated files")
call parser%add_argument("--output", group=output_group)

mode_group = parser%add_mutually_exclusive_group(required=.true.)
call parser%add_argument("--fast", action=store_true(), group=mode_group)
call parser%add_argument("--safe", action=store_true(), group=mode_group)
```

`GroupHandle` is a small value type containing a private owner identity and
group index. Registration rejects invalid handles and handles belonging to a
different parser.

- An ordinary group changes help organization only.
- An optional mutex group permits zero or one explicitly seen members.
- A required mutex group requires exactly one explicitly seen member.
- Namespace defaults do not count as seen.
- One argument belongs to at most one help group in the initial design.
- A mutex group may optionally be associated with one help group when group
  nesting is implemented; it is otherwise displayed in the standard options
  section.

The first facade does not return a pointer into the parser and does not expose
`group%add_argument`. This avoids invalid pointers after allocatable-array
growth or parser copying.

## Parent parsers and subparsers

### Parent definitions

A parser can start from reusable parent definitions:

```fortran
type(ArgumentParser) :: common, command

call common%init(add_help=.false.)
call common%add_argument("--verbose", action=store_true())
call command%init_with_parents([common], prog="tool run")
```

`init_with_parents` accepts the same optional metadata, formatter, version, and
help arguments as `init`. It initializes the receiving parser and then copies
parents in array order.

- Arguments, polymorphic actions and validators, defaults, and groups are deep
  copied. Later reinitialization of a parent cannot affect the child.
- Parent automatic help and version actions are skipped; the receiving parser
  owns its own special actions.
- Parent subcommand trees and formatters are not inherited. The receiving
  parser's `formatter=` controls its formatting.
- Duplicate names or destinations across parents are fatal configuration
  errors. Parent copying never silently overrides an earlier definition.

### Subparser construction and ownership

One final subparser collection can be registered per parser:

```fortran
type(ArgumentParser) :: parser, clone_parser

call clone_parser%init(add_help=.true.)
call clone_parser%add_argument("repository")

call parser%init(prog="tool")
call parser%add_subparsers( &
    title="commands", description="available operations", &
    dest="command", required=.true.)
call parser%add_parser("clone", clone_parser, &
    help_text="clone a repository")
```

`title`, `description`, and `dest` are optional. The defaults are `commands`,
no description, and `command`. `required` defaults to false, matching modern
Python `argparse`. Command names are exact, case-sensitive, nonempty tokens and
must not begin with `-`.

`add_parser` stores a deep snapshot of the supplied parser. It rebases that
snapshot's program name, and all nested child program names, to `parent-prog
command`. Later edits to the source parser do not affect the registered tree.
No pointer or mutable child handle escapes the parent. Calling `add_parser`
without a parser creates an otherwise empty child with automatic help.

The collection is the parser's final positional consumer. Parent arguments are
parsed before the command token; every remaining token is passed recursively to
the selected child. A remainder positional cannot coexist with a subparser
collection because both would own the remaining token stream. Parent options
therefore precede the command; options after it belong to the selected child.

Unknown commands return `ERR_UNKNOWN_SUBCOMMAND`. Omitting a required
collection returns `ERR_MISSING_SUBCOMMAND`. Help and version requests bypass
required-subcommand checks at the level where they occur.

### Hierarchical results and namespace merge

The returned namespace is flat for compatibility with the single-parser API.
Each level first produces its own values, writes the selected name to that
level's `dest`, then deep-merges the selected child's namespace. Incoming child
values replace an existing value with the same key; unrelated parent values
remain. Consequently, the deepest definition wins a destination collision,
including when nested collections reuse `dest="command"`.

The complete, unambiguous hierarchy is retained separately in
`ParseResult%selected_command_path` in root-to-leaf order. Each child owns and
uses its own groups, formatter, defaults, runtime errors, help, and version.
When a nested parse fails, the terminating facade prints the selected child's
usage rather than the root usage.

## Parsing entry points

### Non-terminating token parsing

```fortran
type(ParseResult) :: result
character(len=256) :: tokens(3)

tokens = [character(len=256) :: "input.dat", "--threads", "4"]
result = parser%parse_tokens(tokens)
```

Conceptual signature:

```fortran
function parse_tokens(self, tokens) result(parsed)
    class(ArgumentParser), intent(in) :: self
    character(len=*), intent(in) :: tokens(:)
    type(ParseResult) :: parsed
end function
```

This operation never writes output and never terminates the process.

### Non-terminating process parsing

```fortran
result = parser%try_parse_args()
```

This obtains arguments 1 through `command_argument_count()` and delegates to
`parse_tokens`. Command argument zero is used only to derive `prog`.

### Argparse-like convenience parsing

```fortran
type(Namespace) :: args

args = parser%parse_args()
```

This delegates to `try_parse_args` and applies only the outer process policy:

- Success returns the namespace without output.
- Help requested writes formatted help to `output_unit` and exits with status
  zero.
- Version requested writes the version text to `output_unit` and exits with
  status zero.
- Failure writes concise usage and formatted fatal errors to `error_unit` and
  exits with status two.
- Warnings are written to `error_unit` but do not change a successful result.

The exact Fortran mechanism used to provide a quiet portable exit status is an
implementation detail. The observable status and selected output unit are part
of the contract.

Tests exercise parser semantics through `parse_tokens`. Separate fixture
executables may smoke-test the terminating wrapper and exit codes.

## Parse result

`ParseResult` exposes read-only-by-convention components or equivalent getters:

```fortran
type :: ParseResult
    type(Namespace) :: namespace
    type(ErrorStack) :: errors
    integer :: outcome = PARSE_SUCCESS
    character(len=:), allocatable :: text
    character(len=:), allocatable :: selected_command_path(:)
end type
```

Public outcome constants are:

```text
PARSE_SUCCESS
PARSE_FAILURE
PARSE_HELP
PARSE_VERSION
```

- Warning entries do not turn success into failure.
- Help and version outcomes do not contain fatal errors.
- A fatal token matching, conversion, or validation error stops further token
  consumption to avoid cascading diagnostics.
- Independent post-parse required/group checks may append multiple errors.
- Values already accepted before an error may remain in the result namespace.
  The value belonging to the failing argument is never partially stored.
- A `ParseResult` is self-contained; a later parse does not modify it.
- `selected_command_path` is unallocated when no command was selected and is a
  root-to-leaf array for recursive command dispatch.

## Token grammar and edge cases

### Option recognition

- Matching is exact and case-sensitive.
- Long-option abbreviation is not supported.
- `--option=value`, combined short options, and attached short-option values are
  deferred.
- Options may appear before, between, or after fixed positionals until `--` is
  encountered.
- A known option token terminates variable value consumption unless remainder
  mode is active.
- An unknown token beginning with `-` is an unknown-option error, except when a
  numeric value is currently expected and the token converts successfully.
- Consequently, initial optional string values beginning with `-` are not
  supported until `--option=value` is implemented. Such strings can still be
  supplied as positionals after `--`.

### End-of-options marker

`--` is consumed by the parser and is never stored. Every following token is
treated as positional data, including known option spellings and tokens
beginning with `-`. It has no special meaning inside remainder mode because
remainder mode consumes all text verbatim after its own activation point.

### Positionals

- Fixed positionals consume in registration order.
- A variable positional consumes as many values as possible while reserving the
  minimum number required by every later positional.
- An unknown non-option token after all positional capacity is exhausted is an
  extra-positional error.
- Registration rejects positional layouts for which this allocation rule is
  ambiguous.

### Repeated options

- Store replaces the previous occurrence.
- Append accumulates.
- Count increments.
- Store-true, store-false, and store-const are idempotent except for their final
  stored value.
- A required option needs at least one successful explicit occurrence.
- Any occurrence of a removed option is fatal.

### Subcommand recognition

- A command is recognized exactly after the current parser's positional
  consumers are satisfied.
- Known command names stop variable non-remainder consumption so the command
  token is not swallowed by a preceding `*` or `+` argument.
- An unclaimed non-option token at the command position is an unknown
  subcommand, not an extra positional.
- `--` disables option recognition but does not disable command recognition.

## Namespace contract

`Namespace` is a typed key/value collection. Keys are case-sensitive parser
destinations.

Primary operations are:

```fortran
logical = args%contains("threads")
call args%get("threads", threads, stat)
call args%get("files", files, stat)
```

`get` is a generic subroutine selected by the output argument's intrinsic type,
kind, and rank. `stat` is optional for concise use when the caller knows the
parser contract, but robust applications and all failure tests provide it.

- `stat=FCLAP_OK` on success.
- A missing key returns `ERR_NAMESPACE_MISSING_KEY`.
- A type or kind mismatch returns `ERR_NAMESPACE_TYPE_MISMATCH`.
- On failure, an already allocated or initialized output remains unchanged.
- No getter performs implicit numeric or scalar/list conversion.
- List outputs are allocatable arrays and are allocated or replaced on success.
- Fixed-length character outputs follow normal Fortran assignment and can
  truncate; an allocatable-character convenience getter may be added without
  changing stored values.
- Namespace getters do not print or terminate.

Internal namespace mutation bindings used by actions are not re-exported from
the top-level `fclap` module unless required for custom action implementations.
`Namespace%merge(other, overwrite)` is public. `overwrite` defaults to true;
false preserves values already present in the receiving namespace. Both modes
deep-copy polymorphic stored values.

## Error contract

`ErrorEntry` contains at least:

```text
code
severity
message
argument destination, when applicable
offending token, when applicable
token index, when applicable
```

Error codes and structured context are stable API. Full human-readable message
text is not stable except where a formatter snapshot test explicitly owns it.

Initial severity constants are:

```text
ERROR_FATAL
ERROR_WARNING
```

`ErrorStack` provides `add`, `clear`, `count`, `has_errors`,
`has_fatal_errors`, `has_warnings`, indexed inspection, merging, and explicit
format/print operations. `has_errors` means the stack has at least one entry of
any severity; use `has_fatal_errors` to determine parse failure.

### Stable initial codes

| Code | Value | Meaning |
|---|---:|---|
| `FCLAP_OK` | 0 | Successful operation. |
| `ERR_INVALID_ARGUMENT_NAME` | 1001 | Invalid or mixed argument names. |
| `ERR_DUPLICATE_OPTION` | 1002 | Option alias already registered. |
| `ERR_DUPLICATE_DEST` | 1003 | Destination already registered. |
| `ERR_INVALID_NARGS` | 1004 | Unsupported or inconsistent nargs. |
| `ERR_INCOMPATIBLE_ACTION` | 1005 | Action and argument configuration conflict. |
| `ERR_INVALID_DEFAULT` | 1006 | Default type, conversion, choice, or validation failure. |
| `ERR_INVALID_GROUP` | 1007 | Invalid group handle or membership. |
| `ERR_INVALID_TYPE_NAME` | 1008 | Unknown data type name. |
| `ERR_INVALID_LIFECYCLE` | 1009 | Conflicting deprecation/removal configuration. |
| `ERR_UNKNOWN_ARGUMENT` | 2001 | Unknown option token. |
| `ERR_MISSING_VALUE` | 2002 | An argument did not receive its minimum values. |
| `ERR_EXTRA_POSITIONAL` | 2003 | Positional capacity was exceeded. |
| `ERR_MISSING_REQUIRED` | 2004 | Required argument was not explicitly supplied. |
| `ERR_INVALID_VALUE` | 2005 | Token could not be converted. |
| `ERR_INVALID_CHOICE` | 2006 | Converted value is outside choices. |
| `ERR_VALIDATION_FAILED` | 2007 | A general validator rejected a value. |
| `ERR_MUTEX_CONFLICT` | 2008 | More than one mutex member was supplied. |
| `ERR_MUTEX_REQUIRED` | 2009 | No member of a required mutex group was supplied. |
| `ERR_REMOVED_ARGUMENT` | 2010 | A removed argument was used. |
| `ERR_DEPRECATED_ARGUMENT` | 2011 | Warning that a deprecated argument was used. |
| `ERR_UNKNOWN_SUBCOMMAND` | 2101 | Unknown subcommand at the current parser level. |
| `ERR_MISSING_SUBCOMMAND` | 2102 | Required subcommand missing at the current parser level. |
| `ERR_NAMESPACE_MISSING_KEY` | 3001 | Namespace key not present. |
| `ERR_NAMESPACE_TYPE_MISMATCH` | 3002 | Requested type, kind, or rank does not match. |

Configuration codes occupy 1000-1999, single-parser runtime codes 2000-2099,
subparser codes 2100-2199, namespace codes 3000-3099, and internal invariant
codes 9000-9999.

## Formatting contract

The parser exposes:

```fortran
usage = parser%format_usage()
help = parser%format_help()
call parser%print_usage(unit)
call parser%print_help(unit)
```

The optional output unit defaults to `output_unit`. Formatting functions have
no I/O side effects.

`StandardFormatter` initially guarantees:

- Lowercase `usage:` prefix.
- `positional arguments:` and `options:` headings.
- Registration order within each section and group.
- Alias spelling separated by comma and space.
- Hidden arguments omitted from usage and help.
- String defaults and choices quoted; numeric and logical values unquoted.
- Per-argument `print_default` default true.
- Per-argument `print_choices` default false.
- Default width 80.
- Deterministic newline and indentation behavior tested by complete-string
  `test-drive` assertions.
- Subcommand choices appear as `{name,...}` in usage, bracketed when the
  collection is optional, and as rows under the configured command heading.

The exact wrapping algorithm is fixed by formatter tests during Phase 5. Other
formatters do not change parsing behavior.

## Abstract extension contracts

The following types are intended extension points:

- `ActionType`
- `ValidatorType`
- `FormatterType`

Their deferred procedure signatures become public API once implemented. Before
the first stable release, at least one test-only concrete extension outside the
implementation module must compile and work for each abstract type. This proves
that the contract does not rely on private downcasts or parser internals.

`GroupType` is an internal polymorphic design initially. Public group
subclassing is deferred because externally implemented groups would need a
stable parse-state view contract.

## Testing contract

All Fortran unit and parser-semantic tests use `test-drive`.

- Success tests assert the parse outcome, namespace contents, and absence or
  presence of warnings.
- Expected-failure tests deliberately provide invalid definitions or tokens and
  pass only when stable error codes and structured context match.
- Expected-failure tests are not disabled, skipped, or tests that themselves
  unexpectedly terminate.
- Parser semantics use `parse_tokens`; actual process command-line and exit-code
  behavior is limited to small fixture executable smoke tests.
- Formatter tests compare complete strings.
- Every retained behavior in `docs/design/feature-matrix.md` must be mapped to at
  least one named test before `old/` is removed.

Phase 8 satisfied this gate in `docs/design/legacy-test-map.md`; the mapped
suite passed before the legacy directory was removed.

## Installed package surface

Applications use only the `fclap` facade module. Compiler-generated
`fclap_*.mod` files installed beside `fclap.mod` are implementation
dependencies needed to describe re-exported derived types; their module names
are not separate compatibility promises.

CMake exports one supported target, `fclap::fclap`. Meson exports one
pkg-config dependency named `fclap`, and fpm installs the library. A standalone
consumer that imports only the facade is compiled and run after CMake and
Meson installation in CI.

## Deferred decisions

These decisions are intentionally postponed until their implementation phase:

- Callback dispatch for subcommands.
- Kind-specific integer and real type spelling.
- Composite validator syntax.
- `--option=value`, combined short flags, and attached short values.
- Public array-of-aliases registration.

Deferring these items does not permit the initial implementation to introduce
ownership or dependency cycles that would prevent them later.
