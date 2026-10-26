# Public API and behavior contract

## Status of this contract

This document records the public vocabulary and observable behavior of fclap.
Procedure implementation details may change, but changes to the names or
semantics recorded here require an explicit design update and corresponding
test changes.

The contract covers argument registration, ordinary and mutually exclusive
groups, parent parsers, owned subparsers, and recursive command dispatch.

## Public module

Normal applications import from `fclap` rather than internal modules.

The facade exports:

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
- Built-in action factories and their concrete types (`StoreAction`,
  `StoreConstAction`, `StoreTrueAction`, `StoreFalseAction`,
  `BooleanOptionalAction`, `AppendAction`, `CountAction`).
- Built-in validator factories and their concrete types
  (`LowerBoundValidator`, `UpperBoundValidator`, `RangeValidator`).
- Parse outcome, nargs, error, and severity constants.
- Help-model and concrete value types required by extension interfaces.
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
    add_help=.true., &
    help_on_empty=.false. &
)
```

`help_on_empty=.true.` answers an empty token list at this parser's level with
`PARSE_FAILURE`, a single `ERR_NO_ARGUMENTS` entry, and the parser's help in
`text`; `parse_args` prints only the help to `error_unit` with status 2.

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
case_insensitive_choices      optional logical
env                           optional character
global                        optional logical
delimiter                     optional character
choice_help                   optional character array
```

The integer- and character-`nargs` forms are facade overloads that immediately
create one normalized internal `NargsSpec`. Omitting `nargs` selects the
action's default arity.

`action` and `validator` dummies are non-allocatable `class(...)`, `intent(in)`
objects. The parser owns polymorphic clones created with `allocate(...,
source=...)`; it never retains pointers to caller-owned factory results.

`env="NAME"` declares an environment fallback for an option. Precedence is
command line, then environment, then default; an empty variable is unset. The
value passes through the same conversion, choices, and validator as a token,
with diagnostics prefixed by `environment variable NAME: `. Leading and
trailing blanks of the value are ignored. Zero-value actions read a logical:
true applies the action once, false applies nothing, except
`boolean_optional`, which stores false, and `count`, which reads a
non-negative integer as the number of occurrences. Options producing a list
split the value at blanks and must receive an accepted count; otherwise too
few values are `ERR_MISSING_VALUE` and too many `ERR_UNEXPECTED_VALUE`, both
reporting `expected N values (got M)`. An environment value marks the
argument as explicitly present for required checks, groups, and rules, even
when it is rejected, so only its own error is reported. Conflict, dependency,
and mutex errors involving an environment value name the variable.
Positionals and removed options cannot declare `env` (`ERR_INVALID_ENV`).

`global=.true.` copies the option into every subcommand subtree when a
command is registered and into existing subtrees when the global option is
added later. Values merge by the hierarchical rule below, so a value given
before the command survives unless the option is repeated after it. Global
options must be optional, not required, outside mutex groups, and not part
of rules (`ERR_INCOMPATIBLE_ACTION`, `ERR_INVALID_GROUP`,
`ERR_INVALID_RULE`); a subtree defining the same name or destination is
`ERR_DUPLICATE_OPTION`.

`delimiter` (one non-blank character other than `-` and `=`) makes each
occurrence read exactly one token and split it, keeping every item verbatim,
including empty items and their blanks; nargs must produce a list and bounds
the item count (too few items are `ERR_MISSING_VALUE`, too many
`ERR_UNEXPECTED_VALUE`, both reporting `expected N values separated by 'D'
(got M)`). An empty token is an empty list. For numeric items a separate
token such as `-1,2` is a value, not an option. Environment values use the
delimiter instead of blanks. Positionals cannot declare a delimiter.

`choice_help` must have exactly one entry per choice (`ERR_INVALID_DEFAULT`)
and is rendered one choice per line below the argument help.

Only one explicit validator is accepted per argument. Choices are an independent
built-in constraint. Multiple general validators will later use a composite
validator factory so callers do not need to construct heterogeneous arrays.

### Names and destinations

- A positional definition has exactly one name and that name does not begin
  with `-`.
- Every alias of an optional definition begins with `-`.
- Empty names, `-`, and `--` are invalid.
- Names and destinations must not contain blanks or `=`, and an option alias
  must not begin with `---`; such spellings could never be matched.
- A single-dash alias must not read as a negative number (`-1`, `-.5`,
  `-1e3`), because numeric tokens are values where one is expected.
- A name or destination colliding with the automatic help or version
  argument is reported as such a collision.
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
| `integer` | `int` | `integer(int64)` range; read as default `integer` or `int64` |
| `real` | `float`, `double` | `real(real64)`; read as `real64` or `real32` |
| `logical` | `bool`, `boolean` | Default logical |

Type names are case-insensitive and normalized at registration. An unknown type
name is a configuration error. The default type is `string`.

Logical value tokens are case-insensitive. The accepted true spellings are
`true`, `t`, `1`, `yes`, and `on`; false spellings are `false`, `f`, `0`, `no`,
and `off`. The formatter emits `.true.` and `.false.` for logical values.

Integers are stored with `int64` range and reals as `real64`. Generic
interfaces accept 32-bit and 64-bit integers and `real32`/`real64` reals, so
default `integer` literals and a project's own double-precision kind need no
fclap kind parameter. Narrowing on retrieval is range-checked.

### Defaults and constants

- A non-character default must have the declared intrinsic type category.
  Supported integer or real kinds are converted to the storage kind (`int64` or `real64`)
  with range checking. A `real32` value (default, const, choice, or bound) is
  widened through its shortest decimal text, so `0.1` is stored as
  `0.1_real64` rather than `0.10000000149011612`. There is no implicit integer-to-real,
  real-to-integer, logical-to-numeric, or numeric-to-logical conversion.
- A character default may be parsed using the declared type as a convenience.
- Defaults are converted and checked against choices and validators during
  registration.
- An invalid default rejects the entire argument definition.
- A default inserted into a namespace is not marked explicitly seen.
- An absent optional argument without a default has no namespace entry.
- `store_true`, `boolean_optional`, `store_false`, and `count` provide
  implicit defaults of false, false, true, and zero respectively, unless an
  explicit compatible default is given.
- `const` is required for an optional value-taking argument using `nargs="?"`.
  It is used when the option is present without a value.

A `default` may be a rank-one character, integer, real, or logical array for
list-producing arguments; `const` is always a scalar.

### Required state

- Optional arguments default to `required=.false.`.
- Positionals with minimum cardinality at least one are required.
- Positional `?` and `*` arguments are not required.
- Explicit `required=.true.` is invalid for a positional whose `nargs` permits
  zero values, and explicit `required=.false.` is invalid for a positional
  needing at least one value (`ERR_INCOMPATIBLE_ACTION`).
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
- Zero is valid only for actions declaring zero-value compatibility, and
  only for options: a positional that can never take a value (a zero-value
  action or `nargs=0`) is `ERR_INCOMPATIBLE_ACTION`.
- Boolean, count, help, and version actions default to zero and reject
  incompatible explicit arity.
- `AppendAction` defaults to one but can append each value from a supported
  multi-value occurrence.
- A remainder consumer must be the last positional argument.
- Ambiguous combinations of multiple variable positional consumers are
  registration errors.

## Built-in actions

The public factories are:

```fortran
store()
store_const(value)
store_true()
store_false()
boolean_optional()
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
- `BooleanOptionalAction`: register `--no-name` beside every long `--name`;
  store true for the positive spelling and false for the negated one, consume
  no values, and let the last occurrence win.
- `AppendAction`: append converted values in encounter order; the first
  explicit occurrence replaces a `default=` list.
- `CountAction`: increment the current integer count and consume no values.
- `HelpAction`: request help and stop further token processing without error.
- `VersionAction`: request version output and stop further token processing
  without error.

The abstract action contract consists of:

1. An `apply` deferred binding receiving destination, converted values,
   namespace, action outcome, and error stack.
2. Overridable queries for the default `nargs` (`default_nargs`), explicit
   `nargs` compatibility (`accepts_nargs`), list results (`produces_list`),
   the stored value type (`value_type`), and an implicit default
   (`implicit_default`).

Actions do not convert raw text, enforce choices, own argument names, format
help, print, or terminate the process.

## Built-in validators

The factories are generic over supported integer and real types, and all
bounds are inclusive:

```fortran
not_less_than(lower_bound)
not_bigger_than(upper_bound)
in_range(lower_bound, upper_bound)
```

A bound whose type does not match the argument's `data_type`, or a
validator that accepts no value at all (an `in_range` whose lower bound
exceeds its upper bound, or a NaN bound), is rejected at registration with
`ERR_INCOMPATIBLE_VALIDATOR` and a message naming the problem. `real32`
bounds are widened through their shortest decimal text.

Choices are represented internally by a validator but remain a dedicated
`choices=` registration keyword because that interface is concise and their
values must also be available to formatters.

String choices compare exactly unless `case_insensitive_choices=.true.`. Then
ASCII letter case is ignored and an accepted value, default, or const is
stored with the declared choice's spelling. The option requires string
choices (`ERR_INCOMPATIBLE_ACTION` otherwise), and choices that differ only by
case are rejected with `ERR_INVALID_DEFAULT`.

Choices must be non-empty and free of repeats, and every choice must pass
the argument's validator; otherwise the definition is rejected with
`ERR_INVALID_DEFAULT`. An invalid value reports the allowed values
(`invalid choice: choose from 'a', 'b'`), shortened with `...` when long.

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
- A required mutex group without members can never be satisfied; it makes
  the parser invalid (`is_valid()` is false) with `ERR_INVALID_GROUP`.
- Hidden members are not named in mutex diagnostics; when every member is
  hidden the message names the group instead.
- Mutex members must be omissible. A `required=.true.` option or a positional
  needing at least one value is rejected with `ERR_INVALID_GROUP`.
- One argument belongs to at most one help group.
- A mutex group may optionally be associated with one help group when group
  nesting is implemented; it is otherwise displayed in the standard options
  section.

The first facade does not return a pointer into the parser and does not expose
`group%add_argument`. This avoids invalid pointers after allocatable-array
growth or parser copying.

## Argument rules

Presence relationships beyond mutex groups are registered on the parser:

```fortran
call parser%add_requires("--restart-file", "--restart")
call parser%add_conflict("--dry-run", [character(len=8) :: "--submit", "--queue"])
call parser%add_required_unless("input", "--generate")
```

- Arguments are named by destination or by any registered option spelling
  and must be registered before the rule. The second argument is a scalar or
  a rank-one character array.
- Rules are stored as argument indices, like group members, and checked after
  required arguments and mutex groups, against explicit presence only.
- `add_requires`: the subject being given requires every listed argument;
  one `ERR_MISSING_DEPENDENCY` lists all missing ones.
- `add_conflict`: symmetric; one `ERR_ARGUMENT_CONFLICT` per listed argument
  given together with the subject. A repeated entry, or a pair declared in
  both directions, is reported once.
- `add_required_unless`: the subject is required unless at least one listed
  argument is given; reported as `ERR_MISSING_REQUIRED`.
- `ERR_INVALID_RULE` rejects unknown names, an empty list, a rule naming its
  own subject, a `required_unless` subject that is always present, and a
  conflict involving an argument that is always present. A rejected rule is
  not stored.
- `init_with_parents` copies rules whose arguments were copied; `add_parser`
  snapshots keep them.
- Value-dependent rules (`requires_if`, `required_if_eq`, `default_value_if`
  in clap) are deferred.

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
    help="clone a repository")
```

`title`, `description`, and `dest` are optional. The defaults are `commands`,
no description, and `command`. `required` defaults to false, matching modern
Python `argparse`. Command names are exact, case-sensitive, nonempty tokens and
must not begin with `-`; trailing blanks of a command-line token are ignored,
so `'run '` selects `run`.

`add_parser(name, subparser, help, aliases)` accepts an optional rank-one
array of aliases. Aliases follow the command-name rules, must not repeat any
registered name or alias, and are validated before anything is registered.
An alias resolves to its command: the destination and
`selected_command_path` record the registered name. Help lists aliases after
the name in parentheses; usage lists registered names only. Unknown-command
suggestions consider aliases too.

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

Unknown commands return `ERR_UNKNOWN_SUBCOMMAND`, without an additional
`ERR_MISSING_SUBCOMMAND`. Omitting a required collection returns
`ERR_MISSING_SUBCOMMAND`. Help and version requests bypass
required-subcommand checks at the level where they occur.

A selected command is still parsed when its parent fails only presence
checks (required arguments, groups, rules) or an environment value: a help
or version request inside the command then wins, otherwise the errors of
both levels are reported. A token, conversion, or validation failure in the
parent stops before the command.

### Hierarchical results and namespace merge

The returned namespace is flat for compatibility with the single-parser API.
Each level first produces its own values, writes the selected name to that
level's `dest`, then deep-merges the selected child's namespace. Incoming child
values replace an existing value with the same key unless the existing value
has a higher-precedence value source (command line > environment > default);
unrelated parent values remain. Consequently, between equally sourced values
the deepest definition wins a destination collision, including when nested
collections reuse `dest="command"`, but a child default never hides a value
the user supplied to a parent. An argument of a command tree must not use
its parent's subparser `dest`, whose value would replace the selected command
name, and a destination shared between levels must have the same data type
and rank; both are `ERR_DUPLICATE_DEST` at registration.

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
function parse_tokens(self, tokens, environment) result(parsed)
    class(ArgumentParser), intent(in) :: self
    character(len=*), intent(in) :: tokens(:)
    character(len=*), intent(in), optional :: environment(:)
    type(ParseResult) :: parsed
end function
```

This operation never writes output and never terminates the process. When
`environment` (`NAME=value` entries, last one wins) is present it replaces the
process environment for `env=` options, including inside subcommands.

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
- An invalid parser definition writes `prog: invalid parser definition: ...`
  lines to `error_unit` and exits with status 70, before reading any input.
- `ArgumentParser%error(message)` writes usage and `prog: error: message` to
  `error_unit` and exits with status two, for checks the program makes after
  parsing.
- Failure writes concise usage and formatted fatal errors to `error_unit` and
  exits with status two through normal termination, so no runtime backtrace
  is printed.
- Warnings are written to `error_unit` but do not change a successful result.
- Diagnostics use the argparse form `prog: error: argument -j/--threads:
  message`, produced by `ArgumentParser%format_diagnostic`.
- When an entry carries a suggestion, the diagnostic ends with
  `(did you mean 'SUGGESTION'?)`.
- A `help_on_empty` request prints only the selected parser's help to
  `error_unit` and exits with status two.

`parse_known_args(extras)`, `try_parse_known_args()`, and
`parse_known_tokens(tokens, environment)` behave like their strict
counterparts, except that unknown options and surplus positionals, at any
command level, are appended in order to `extras` / `ParseResult%extra_tokens`
(allocated, possibly empty) instead of failing. A known option still needs
its values, and a value following an unknown option is treated like any
other token. Strict parsing leaves `extra_tokens` unallocated.

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
contains
    procedure :: value_source  ! integer function (dest)
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
- Independent post-parse required, group, and rule checks may append
  multiple errors. They are skipped after a fatal token error, because the
  unconsumed tokens might have satisfied them, and an argument that was
  supplied but rejected counts as present.
- Values already accepted before an error may remain in the result namespace.
  The value belonging to the failing argument is never partially stored.
- A `ParseResult` is self-contained; a later parse does not modify it.
- `selected_command_path` is unallocated when no command was selected and is a
  root-to-leaf array for recursive command dispatch.
- `value_source(dest)` returns `SOURCE_COMMAND_LINE`, `SOURCE_ENVIRONMENT`,
  `SOURCE_DEFAULT` (also for implicit action defaults), or `SOURCE_NONE` when
  no value is stored. The integer values ascend by precedence. Sources of a
  selected subcommand are merged with the child taking precedence, and the
  subparser destination itself is `SOURCE_COMMAND_LINE`.
- As in argparse, an exact `-h`/`--help` or `--version` later on the command
  line (before `--` and before a command name) wins over an unknown-option or extra-positional failure:
  the outcome is `PARSE_HELP` or `PARSE_VERSION` and only warnings are kept.
  Conversion and validation failures still win.

## Token grammar and edge cases

### Option recognition

- Matching is case-sensitive. An exact registered spelling always wins,
  including multi-letter single-dash options such as `-np`.
- Long-option abbreviation is not supported.
- Trailing blanks never change a name: `'-v '`, `'--quiet '`, and `'-- '`
  name `-v`, `--quiet`, and the end-of-options marker even when exact token
  lengths are given. Values keep their exact length, so `--sep ", "` stores
  the trailing blank.
- `--name=value` and `-n=value` split at the first `=` when the text before it
  is a registered option. The attached text is the only value of that
  occurrence, so it may begin with `-`. Flags reject an attached value with
  `ERR_UNEXPECTED_VALUE`; options needing more than one value report
  `ERR_MISSING_VALUE`.
- `-nVALUE` attaches the rest of the token to a one-letter option that takes
  values; in a cluster the value may also follow `=` (`-vn=3`).
- A token whose first letter is a one-letter flag is a cluster such as `-vvn`.
  Every letter must be a registered one-letter option; the first one that
  takes values ends the cluster and receives the remaining text, or the
  following tokens when nothing remains.
- A lone `-` is an ordinary value, conventionally standard input or output.
- Options may appear before, between, or after fixed positionals until `--` is
  encountered.
- A known option token, in any of the spellings above, terminates variable
  value consumption unless remainder mode is active.
- An unknown token beginning with `-` is an unknown-option error, except when a
  numeric value is currently expected and the token looks numeric: a sign
  followed by a digit, by `.` and a digit, or by `inf`, `infinity`, or `nan`.
  Such a token is converted, and an invalid number is `ERR_INVALID_VALUE`.
- String values beginning with `-` are supplied with the attached form
  (`--pattern=-x`) or as positionals after `--`.

### Numeric tokens

- Integers are an optional sign followed by decimal digits and are stored with
  64-bit range; out-of-range text is `ERR_INVALID_VALUE`.
- Reals are decimal literals with an optional `e`, `E`, `d`, or `D` exponent,
  or the case-insensitive `inf`, `infinity`, and `nan`, each with an optional
  sign. A literal beyond the `real64` range is `ERR_INVALID_VALUE`; one too
  small to represent reads as zero.
- Surrounding blanks are ignored. Any other text, including Fortran
  list-directed forms such as `2*7`, `4,5`, `5/`, or a lone `,`, is rejected.

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
- Append accumulates. The first explicit occurrence replaces a `default=`
  list (a deliberate difference from argparse, which extends it).
- `boolean_optional` stores true for `--name` and false for the generated
  `--no-name`; the last occurrence wins.
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
n = args%size()
call args%remove("threads")
call args%clear()
```

`get` is a generic subroutine selected by the output argument's intrinsic type,
kind, and rank. `stat` follows Fortran's `iostat` convention: when present it
reports the outcome; when absent, any failure stops the program with a message
naming the key, because it indicates a programming error.

- `stat=FCLAP_OK` on success.
- A missing key returns `ERR_NAMESPACE_MISSING_KEY`.
- A type or rank mismatch returns `ERR_NAMESPACE_TYPE_MISMATCH`.
- A value outside a narrower requested kind returns
  `ERR_NAMESPACE_OUT_OF_RANGE`, including a nonzero real that would become
  zero as `real32`.
- String outputs are `character(len=:), allocatable` and receive the exact
  stored length; fixed-length character variables do not match `get`.
- On failure, an already allocated or initialized output remains unchanged.
- No getter performs scalar/list conversion; numeric kinds convert only within
  range.
- List outputs are allocatable arrays and are allocated or replaced on success.
- With `stat` present, namespace getters do not print or terminate.

The mutation bindings `set`, `append`, `set_value`, `append_value`, and
`remove` are public so that custom actions and tests can populate a namespace.
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
offending token, verbatim, when applicable
token index, when applicable
suggested spelling, when applicable
program name of the reporting parser level (`prog`), when applicable
```

Diagnostics from a selected command are prefixed with that command's program
name (`tool go: error: ...`).

A suggestion is attached to unknown options, unknown subcommands, and invalid
string choices when a known spelling has a Jaro similarity above 0.7 to the
offending token. Option names are compared without their leading hyphens or
an attached `=value`; single-character names, hidden options, and removed
options are never suggested; numeric and logical choices are not suggested.
Ties select the first registered candidate.

Error codes and structured context are stable API. Full human-readable message
text is not stable except where a formatter snapshot test explicitly owns it.

Severity constants are:

```text
ERROR_FATAL
ERROR_WARNING
```

`ErrorStack` provides `add`, `append`, `clear`, `count`, `has_errors`,
`has_fatal_errors`, `has_warnings`, indexed inspection, merging,
`offset_token_indices`, and explicit format/print operations. `has_errors` means the stack has at least one entry of
any severity; use `has_fatal_errors` to determine parse failure.

### Stable codes

| Code | Value | Meaning |
|---|---:|---|
| `FCLAP_OK` | 0 | Successful operation. |
| `ERR_INVALID_ARGUMENT_NAME` | 1001 | Invalid or mixed argument names. |
| `ERR_DUPLICATE_OPTION` | 1002 | Option alias already registered. |
| `ERR_DUPLICATE_DEST` | 1003 | Destination already registered. |
| `ERR_INVALID_NARGS` | 1004 | Unsupported or inconsistent nargs. |
| `ERR_INCOMPATIBLE_ACTION` | 1005 | Action and argument configuration conflict. |
| `ERR_INVALID_DEFAULT` | 1006 | Default type, conversion, choice, or validation failure. |
| `ERR_INVALID_GROUP` | 1007 | Invalid group handle or membership, or a group no parse can satisfy. |
| `ERR_INVALID_TYPE_NAME` | 1008 | Unknown data type name. |
| `ERR_INVALID_LIFECYCLE` | 1009 | Conflicting deprecation/removal configuration. |
| `ERR_INCOMPATIBLE_VALIDATOR` | 1010 | Validator cannot check the argument type, or accepts no value (empty range, NaN bound). |
| `ERR_INVALID_RULE` | 1011 | Argument rule names unknown arguments or can never be satisfied. |
| `ERR_INVALID_ENV` | 1012 | Invalid environment variable name, or `env=` on a positional or removed option. |
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
| `ERR_UNEXPECTED_VALUE` | 2012 | An attached value was given to an option that takes none, or more values than the argument accepts. |
| `ERR_MISSING_DEPENDENCY` | 2013 | An argument was given without an argument it requires. |
| `ERR_ARGUMENT_CONFLICT` | 2014 | Two arguments declared as conflicting were given together. |
| `ERR_NO_ARGUMENTS` | 2015 | Empty command line at a `help_on_empty` parser; `text` holds its help. |
| `ERR_UNKNOWN_SUBCOMMAND` | 2101 | Unknown subcommand at the current parser level. |
| `ERR_MISSING_SUBCOMMAND` | 2102 | Required subcommand missing at the current parser level. |
| `ERR_NAMESPACE_MISSING_KEY` | 3001 | Namespace key not present. |
| `ERR_NAMESPACE_TYPE_MISMATCH` | 3002 | Requested type, kind, or rank does not match. |
| `ERR_NAMESPACE_OUT_OF_RANGE` | 3004 | Stored value does not fit the requested narrower kind. |

Configuration codes occupy 1000-1999, single-parser runtime codes 2000-2099,
subparser codes 2100-2199, and namespace codes 3000-3099.

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

`StandardFormatter` guarantees:

- Lowercase `usage:` prefix; usage lists options before positionals.
- Choices shown as the metavar (`{a,b}`) unless an explicit `metavar` is given.
- A fixed count repeats the metavar per value (`--grid N N N`, with a
  delimiter `--grid N,N,N`); counts above 10 number the values instead
  (`N1 N2 ... N100`, `L1,L2,...,L100`). An explicit `metavar` that already contains the
  separator (a blank, or the delimiter) describes every value and is shown
  once (`metavar="X Y Z"`, `metavar="EX,EY,EZ"`).
- Implicit action defaults (e.g. a flag's `.false.`) are not annotated.
- Reals printed in their shortest round-trip form: fixed notation for
  magnitudes from 1e-3 up to 1e6 and zero (`0.1`, `1.0`, `0.001`),
  scientific notation with one leading digit otherwise (`1e-6`, `2.5e10`),
  and `inf`, `-inf`, `nan` for special values.
- `positional arguments:` and `options:` headings.
- Registration order within each section and group.
- Alias spelling separated by comma and space.
- Hidden arguments omitted from usage and help.
- String defaults and choices quoted; numeric and logical values unquoted.
- Per-argument `print_default` default true.
- Per-argument `print_choices` default false.
- Default width 80; a `help_width` from 1 to 19 is raised to the minimum of
  20, and zero or a negative width selects the default.
- Wrapping and alignment count UTF-8 characters, not bytes, and never split a
  word.
- A user-supplied `usage=` is printed verbatim after `usage: `; only generated
  usage is wrapped.
- Help lines carry no trailing blanks, and whitespace-only help counts as no
  help.
- Groups without visible members and without a description are omitted; an
  empty group or command title falls back to `arguments:` or `commands:`.
- An empty deprecation or removal message renders `(deprecated)` or
  `(removed)`.
- Deterministic newline and indentation behavior tested by complete-string
  `test-drive` assertions.
- Subcommand choices appear as `{name,...}` in usage, bracketed when the
  collection is optional, and as rows under the configured command heading.

The exact wrapping algorithm is fixed by the formatter tests. Other
formatters do not change parsing behavior.

## Abstract extension contracts

The following types are intended extension points:

- `ActionType`
- `ValidatorType`
- `FormatterType`

Their deferred procedure signatures become public API once implemented.
`ActionType` also offers overridable hooks with neutral defaults:
`option_names` (add spellings at registration), `apply_option` (react to the
matched spelling), and `replaces_default` (start empty on the first explicit
occurrence). `ValidatorType` offers `accepts_type(type_name)`, which rejects
argument types the validator cannot check, and `is_consistent(reason)`,
which reports a configuration that accepts no value; the defaults accept
every type and are consistent. A custom validator that rejects a value
without setting `message` is reported with the generic message.
Before
the first stable release, at least one test-only concrete extension outside the
implementation module must compile and work for each abstract type. This proves
that the contract does not rely on private downcasts or parser internals.

`GroupType` is internal and not exported. Public group
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
- Every implemented behavior in `doc/design/feature-matrix.md` is covered by
  at least one named test; `doc/design/legacy-test-map.md` records the mapping
  for behavior carried over from the original prototype.

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
- Composite validator syntax beyond the inclusive `in_range` validator.
- Public array-of-aliases registration.

Deferring these items does not permit the implementation to introduce
ownership or dependency cycles that would prevent them later.
