# fclap architecture

A map of how fclap works internally, for maintainers. It traces one command
line through the code and names the function at every step, so you can read
along in the source. Function names are used instead of line numbers because
they survive edits; search for them.

## The big picture

fclap does two separate jobs at two separate times:

1. **Definition** (`init`, `add_argument`, `add_parser`, ...): the program
   describes its command line. Every call is validated immediately and turned
   into plain data stored in the `ArgumentParser`. Nothing is parsed yet.
2. **Parsing** (`parse_args`, `parse_tokens`, ...): a token array is matched
   against that stored data. The parser is `intent(in)` here: parsing never
   changes the definition, so one parser can parse many command lines.

```
 user program
     │  call parser%init(...), parser%add_argument(...)
     ▼
 ┌──────────────────────── ArgumentParser (argparser.f90) ───────────────────┐
 │ arguments(:)  flat array of Argument      ◄── built by registration.f90   │
 │ groups(:)     member indices into arguments(:)                            │
 │ rules(:)      requires/conflicts, also indices                            │
 │ subparsers(:) complete child ArgumentParsers (copies)                     │
 │ config_errors ErrorStack of definition mistakes                           │
 └───────────────────────────────────────────────────────────────────────────┘
     │  args = parser%parse_args()
     ▼
 parse_level ──► parse_definitions (parse/engine.f90) ──► ParseResult
     │             pure: no printing, no stop          namespace + errors
     │                                                 + outcome + text
     ├─ subcommand selected? recurse into the child's parse_level
     ▼
 finish_process_parse: print help / diagnostics, `stop` with exit status
     ▼
 Namespace returned to the user program ──► call args%get("threads", n)
```

The most important design rule: **only the outermost layer prints or
stops.** Everything below `finish_process_parse` returns data. That is what
makes library mode (`parse_tokens`, `try_parse_args`) possible and what makes
the code testable without running programs.

## Module map

Grouped by role. Arrows mean "uses".

| Role | Modules | What lives there |
|---|---|---|
| Public surface | `fclap.f90` | Re-exports everything users may touch. Users `use fclap` only. |
| Facade | `argparser.f90` | `ArgumentParser`: owns all state, registration API, parse entry points, subcommand dispatch, diagnostics formatting. |
| Definition checking | `registration.f90`, `argument.f90`, `nargs.f90` | Turn `add_argument` keywords into one validated `Argument`, or one configuration error. |
| Parsing | `parse/engine.f90`, `parse/context.f90`, `parse/result.f90` | The token loop (`parse_definitions`), its scratch state (`ParseContext`), and its output (`ParseResult`). |
| Values | `value/abstract.f90`, `value/builtin.f90`, `value/convert.f90`, `namespace.f90` | Polymorphic values (`ValueBox`), text-to-value conversion, and the key/value result store (`Namespace`). |
| Extension points | `actions/*`, `validators/*`, `formatter/*` | Abstract types users can extend: what an argument does, how a value is checked, how help looks. |
| Constraints | `groups/*`, `rules.f90` | Mutually exclusive groups, help sections, requires/conflicts/required-unless. |
| Errors | `error/codes.f90`, `error/entry.f90`, `error/stack.f90` | Stable `ERR_*` codes, one diagnostic (`ErrorEntry`), a list of them (`ErrorStack`). |
| Utilities | `utils/*` | Kind parameters, version, string similarity for "did you mean". |

Dependency direction, roughly bottom to top: `utils`, `error` → `value`,
`nargs`, `namespace` → `actions`, `validators` → `argument`, `groups`,
`rules`, `formatter` → `registration`, `parse/*` → `argparser` → `fclap`.
Lower layers never use higher ones, so the engine knows nothing about
`ArgumentParser`.

## Walkthrough: `fclap-basic in.dat -j 4 --verbose`

This follows `example/basic.f90`, the smallest real program.

### 1. `parser%init(prog=..., description=...)`

`parser_init` in `argparser.f90`:

- `clear()` resets all state, so `init` can be called again.
- `new_group_owner_id()` gives this parser a unique number. Group handles
  carry it, so a handle from another parser is rejected.
- A `StandardFormatter` is allocated unless the user passed a formatter.
- `-h/--help` is registered through the normal `add_argument` path with
  `action=help_action()`. Help is not special-cased in the engine; it is an
  ordinary argument whose action returns `ACTION_HELP_REQUESTED`.
  `--version` works the same way when `version=` is given.

### 2. `parser%add_argument("-j", "--threads", data_type="integer", default=1, validator=not_less_than(1))`

`parser_add_argument` is a pipeline of checks. Each step either passes or
calls `self%reject(code, message, ...)` and **returns without registering
anything**:

1. `check_names` (registration.f90): collect the names, let the action add
   spellings (e.g. `boolean_optional` adds `--no-x`), check syntax and
   collisions with existing arguments.
2. Group handle checks, if `group=` was given.
3. `build_argument` (registration.f90): the bulk of the work. In a fixed
   order: derive `dest` (`--threads` → `threads`), pick the action (default
   `store`), normalize `nargs` (`nargs.f90`), resolve the data type, check
   action/nargs/type compatibility, convert `default`, `const`, and `choices`
   into `ValueBox`es and validate them. Result: a complete `Argument`.
4. Extra feature checks: `check_env_source`, `check_delimiter`,
   `choice_help`, `check_global`.
5. `append_argument` stores the `Argument` in `self%arguments`. Group
   membership is recorded as the argument's index.

**Registration never stops the program.** `reject` adds an entry to
`self%config_errors`, so a program can register everything and inspect all
mistakes at once (`is_valid`, `get_config_errors`). The program only stops
when it tries to parse with a broken definition (step 3).

`Argument` (argument.f90) is plain data: names, `dest`, `nargs`, data type,
action, validator, default/const/choices as `ValueBox`es, and help metadata.
It does not parse anything itself.

### 3. `args = parser%parse_args()`

`parser_parse_args`:

1. `is_valid()` false → `abort_invalid_definition` prints the configuration
   errors and stops with status 70. That signals a bug in the program, not
   the user's input, which uses status 2.
2. `try_parse_args` → `process_tokens` reads the real command line with
   `get_command_argument` → `parse_tokens(tokens)` → `parse_level(tokens)`.
3. The `ParseResult` goes to `finish_process_parse` (step 6).

`parse_tokens` is the same path without step 1 and step 3. Library mode
and almost every unit test call it directly with a token array.

### 4. `parse_level`: one parser's share of the line

`parser_parse_level` handles `help_on_empty`, collects the subcommand names,
and calls the engine:

```fortran
parsed = parse_definitions(definitions, self%config_errors, tokens, groups, ...)
```

The engine only receives arrays of data (arguments, groups, rules, command
names). It never sees the `ArgumentParser`.

### 5. `parse_definitions`: the token loop (parse/engine.f90)

Setup:

- `context%init` creates a `ParseContext`: the tokens, a `cursor`, a
  `positional_cursor`, per-argument `seen(:)`/`completed(:)` flags, a fresh
  `Namespace`, and an `ErrorStack`. All mutable state of one parse lives
  here and is discarded afterwards.
- `apply_defaults` writes every default into the namespace first, so
  explicit values simply overwrite them.

Then one loop over the tokens. For each token:

| Token | Handling |
|---|---|
| `--` | Options are switched off; everything after is positional. |
| `-j`, `--threads` | `resolve_option` finds an exact match → `consume_option`. |
| `--threads=4`, `-j4` | `resolve_option` splits off the attached value → `consume_option` with `attached`. |
| `-vx` | One-letter flags combined → `consume_cluster`. |
| `in.dat` | `next_positional` finds the next unfilled positional → `consume_positional`. |
| a subcommand name | `context%select_command`, then **leave the loop**; the rest belongs to the child. |
| anything else starting with `-` | unknown option error (or kept in `extra_tokens` for `parse_known_*`). |

The loop also stops at the first fatal error, or when an action asks to stop
(help, version).

**Consuming `-j 4`** (`consume_option`):

1. `record_lifecycle`: a `removed_msg` argument is an error, a
   `deprecated_msg` one adds a *warning* and continues.
2. `scan_available_values` counts how many following tokens can be values.
   It stops at `--`, at a known option, at an unknown option, or when
   `nargs` is satisfied. Too few → missing-value error.
3. `convert_token_range` → `convert_and_validate_text` for every value:
   `convert_value` turns `"4"` into an `IntegerValue` (value/convert.f90),
   then `choices`, then the `validator` (`not_less_than(1)`) are checked.
   Several values produce one `ListValue`.
4. `execute_action` calls `definition%action%apply_option(...)`. For the
   default `StoreAction` that is just `namespace%set_value("threads", 4)`.
   The argument is marked `seen` (explicitly given).

`--verbose` takes zero values (`NARGS_ZERO`), so step 3 is skipped and the
`store_true` action stores `.true.`.

**Consuming `in.dat`** (`consume_positional`) is the same idea, plus one
subtlety: for `?`, `*`, `+` positionals it reserves enough tokens for later
positionals (`later_positional_minimum`), like argparse.

After the loop:

1. `materialize_empty_positionals`: `*` positionals that received nothing
   get an empty list.
2. `apply_environment`: options not given on the line are filled from their
   `env=` variable, converted and validated like typed input.
3. If an error happened, `find_later_exit_request` checks whether `-h`
   appears later on the line; help wins over an earlier typo, as in argparse.
4. Help/version requested → return `PARSE_HELP`/`PARSE_VERSION` without
   further checks.
5. Whole-line checks: `check_required`, `validate_groups` (mutex),
   `validate_rules` (requires/conflicts), missing subcommand.
6. `finish_result` packs namespace, errors, outcome and `extra_tokens` into
   the `ParseResult`, and `record_sources` notes for every value whether it
   came from a default, the environment, or the command line.

### 6. Back up: subcommands and the process policy

Back in `parse_level`:

- `PARSE_HELP` → the help text is rendered and stored in `parsed%text`.
- A selected subcommand → the child's `parse_level` is called **recursively**
  on the remaining tokens. Its namespace is merged into the parent's
  (`merge_child`), its errors are shifted to absolute token positions and
  merged, and its outcome becomes the overall outcome.

Then `finish_process_parse` applies the only side effects in the library:

| Outcome | Effect |
|---|---|
| `PARSE_SUCCESS` | Print warnings (e.g. deprecated option), return the namespace. |
| `PARSE_HELP`, `PARSE_VERSION` | Print `parsed%text` to stdout, `stop 0`. |
| `PARSE_FAILURE` | Print usage and all diagnostics to stderr, `stop 2`. |

Diagnostics are rendered by `format_diagnostic`, which needs the parser to
map a `dest` like `threads` back to `-j/--threads`. `print_diagnostics` and
`format_diagnostics` do this for a whole `ErrorStack`; library mode uses the
same functions, so both modes print identical messages.

### 7. `call args%get("threads", threads)`

`Namespace` (namespace.f90) is an ordered list of `key → ValueBox`. `get` is
a generic interface with one specific per Fortran type and kind; the
compiler picks the one matching the variable you pass. Without `stat=`, a
missing key or wrong type stops with a message naming the key; with `stat=`
it returns an error code instead.

## Help output

```
format_help → get_help_model → HelpModel → formatter%format_help(model)
```

`get_help_model` copies everything needed for display out of the parser
into `HelpModel` (formatter/model.f90), a data-only snapshot. The formatter
only sees that snapshot, so a custom formatter (extend `FormatterType`) can
change the look without access to parser internals. `StandardFormatter`
builds usage, description, positional and option sections, groups,
subcommands and epilog, and joins them with blank lines.

## Key types at a glance

| Type | Module | Lifetime | Holds |
|---|---|---|---|
| `ArgumentParser` | argparser | whole program | the definition: arguments, groups, rules, children, config errors |
| `Argument` | argument | whole program | one normalized argument definition |
| `ParseContext` | parse/context | one parse | cursor, seen flags, namespace and errors under construction |
| `ParseResult` | parse/result | returned to caller | namespace, errors, outcome, help text, extra tokens, value sources |
| `Namespace` | namespace | returned to caller | `dest → value` results |
| `ValueBox` / `ValueType` | value/abstract | everywhere | one polymorphic value: string, integer, real, logical, or list |
| `ErrorStack` / `ErrorEntry` | error/* | everywhere | diagnostics with code, severity, argument, token, position, suggestion |
| `GroupBox` / `GroupHandle` | groups/abstract | whole program | groups as member indices; handles users pass to `add_argument` |
| `ArgumentRule` | rules | whole program | requires/conflicts/required-unless as indices |
| `HelpModel` | formatter/model | one help render | display-only snapshot for formatters |

## Extension points

Users extend fclap by subclassing three abstract types. Everything else is
deliberately concrete.

| Abstract type | Contract | Built-in examples |
|---|---|---|
| `ActionType` (actions/abstract) | `apply(dest, value, namespace, outcome, errors)`; optional overrides for default nargs, implicit default, extra spellings | `store`, `store_const`, `store_true`, `append`, `count`, `boolean_optional`, help, version |
| `ValidatorType` (validators/abstract) | `validate(value, valid, message)` on an already converted value | `not_less_than`, `not_bigger_than`, `in_range`; `choices=` uses an internal one |
| `FormatterType` (formatter/abstract) | `format_help(model)`, `format_usage(model)` returning text | `StandardFormatter` |

## Rules worth knowing before you change things

- **The engine is pure.** `parse/engine.f90` must not print, `stop`, or
  touch an `ArgumentParser`. New behaviour that needs parser state is passed
  in as data (as `rules`, `subcommand_names`, `environment` are).
- **Arguments are referenced by index.** Groups and rules store positions in
  `self%arguments`. Code that copies or reorders arguments (parents,
  `clone_from`, global options) must remap those indices (`ArgumentRule%remap`).
- **Subcommands are copied, not referenced.** `add_parser` stores a clone of
  the child parser. Arguments added to the child variable afterwards are not
  seen. Global options are copied into every child (`inherit_global`).
- **Definition errors accumulate, parse errors stop early.** Registration
  records every mistake. The token loop stops at its first fatal error, then
  still runs the whole-line checks, which is why a typo can be reported
  together with a missing required argument.
- **Severity is separate from code.** `ERROR_FATAL` makes a parse fail;
  `ERROR_WARNING` (deprecation) is printed but the parse succeeds.
- **Error codes are stable API.** Ranges: 1000s definition, 2000s parsing,
  2100s subcommands, 3000s namespace access. Tests assert on codes, not
  message text, wherever possible.

## Where to look when you want to…

| Change | Start at |
|---|---|
| a new `add_argument` keyword | `parser_add_argument`, `build_argument`, `Argument` |
| how a token is recognized | `resolve_option`, `token_starts_option`, `token_is_numeric_value` |
| how many values an option takes | `nargs.f90`, `scan_available_values` |
| text-to-value conversion, a new data type | `value/convert.f90`, `value/builtin.f90`, `namespace.f90` |
| a new action | subclass `ActionType`, see `actions/store.f90` |
| a new check on values | subclass `ValidatorType`, see `validators/bounds.f90` |
| help layout | `get_help_model`, `formatter/standard.f90` |
| error message wording | `parser_format_diagnostic`, and the `errors%add` call that raises it |
| subcommand behaviour | `add_subparsers`, `add_parser`, `parse_level` |
| exit codes, what gets printed | `finish_process_parse` |

## Suggested reading order

1. `example/basic.f90`, then `example/library_mode.f90`.
2. `parser_parse_args` → `parse_level` → `parse_definitions` (top half of the
   loop). Skip the helpers on the first pass.
3. `consume_option` and `convert_and_validate_text`.
4. `actions/abstract.f90` and `actions/store.f90`: how a value reaches the
   namespace.
5. `parser_add_argument` → `build_argument`: how a definition is checked.
6. `parse_level`'s subcommand half, then `add_parser`.
7. A test file such as `test/unit/test_cli_syntax.f90`: tests call
   `parse_tokens` with literal token arrays and check codes and messages,
   which is the quickest way to try a hypothesis about behaviour.
