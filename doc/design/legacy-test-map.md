# Legacy behavior to Test Drive coverage

This is the Phase 8 deletion gate for the former `old/` implementation. Every
retained successful behavior and every formerly disabled failure routine is
mapped to at least one named Test Drive case. The legacy sources were removed
only after this table was complete and the mapped suite passed.

Test names below are the strings reported by `test/unit/main.f90`; the module
column identifies their source file.

## Successful legacy routines

| Legacy routine | Current Test Drive case | Module |
|---|---|---|
| `test_basic_parsing` | `parse positional option and alias` | `test_parse_engine` |
| `test_store_true_false` | `repeated built-in actions` | `test_parse_actions` |
| `test_count_action` | `repeated built-in actions` | `test_parse_actions` |
| `test_default_values` | `defaults and independent results` | `test_parse_engine` |
| `test_default_roundtrip_types` | `typed scalar conversion` and `scalar round trip` | `test_parse_actions`, `test_namespace` |
| `test_help_generation` | `standard help snapshot` | `test_formatter` |
| `test_help_default_display` | `defaults choices and lifecycle` | `test_formatter` |
| `test_help_choices_display` | `defaults choices and lifecycle` | `test_formatter` |
| `test_print_default_matrix` | `defaults choices and lifecycle` | `test_formatter` |
| `test_print_choices_matrix` | `defaults choices and lifecycle` | `test_formatter` |
| `test_choices_format_by_type` | `defaults choices and lifecycle` | `test_formatter` |
| `test_real_default_format_edges` | `scalar values` and `defaults choices and lifecycle` | `test_values`, `test_formatter` |
| `test_wp_real_precision_display` | `scalar values` and `defaults and independent results` | `test_values`, `test_parse_engine` |
| `test_mismatched_default_rejected` | `defaults choices and validators` | `test_registration` |
| `test_rejected_default_does_not_leak` | `defaults choices and validators` | `test_registration` |
| `test_type_conversion` | `typed scalar conversion` | `test_parse_actions` |
| `test_append` | `repeated built-in actions` | `test_parse_actions` |
| `test_nargs` placeholder | `exact option and positional counts`, `optional star and plus options`, `variable positional allocation`, `remainder positional`, and `nargs consumption failures` | `test_parse_nargs` |
| `test_deprecated_warning` | `deprecated and removed arguments` | `test_validators` |
| `test_hidden_arguments` | `custom usage and visibility` | `test_formatter` |
| `test_mutex_groups` | `optional mutex permits zero or one` | `test_groups` |
| `test_parent_parsers` | `parent definitions are deep copied` | `test_subparsers` |
| `test_argument_groups` | `ordinary group help snapshot` | `test_groups` |
| `test_subparsers` | `one-level dispatch and merged namespace` and `subcommands appear in usage and help` | `test_subparsers` |

## Formerly disabled failure routines

These are expected-failure assertions: invalid input is supplied and the Test
Drive case passes only if a structured failure with the expected stable code is
returned.

| Legacy routine | Current Test Drive case | Module |
|---|---|---|
| `test_fail_missing_required_positional` | `missing required arguments` | `test_parse_engine` |
| `test_fail_missing_required_optional` | `missing required arguments` | `test_parse_engine` |
| `test_fail_option_missing_value` | `missing option value` | `test_parse_engine` |
| `test_fail_invalid_int_value` | `conversion failures are atomic` | `test_parse_actions` |
| `test_fail_invalid_real_value` | `conversion failures are atomic` | `test_parse_actions` |
| `test_fail_invalid_logical_value` | `conversion failures are atomic` | `test_parse_actions` |
| `test_fail_invalid_choice_string` | `runtime choices by type` | `test_validators` |
| `test_fail_invalid_choice_integer` | `runtime choices by type` | `test_validators` |
| `test_fail_mutex_conflict` | `mutex conflict is structured failure` | `test_groups` |
| `test_fail_required_mutex_missing` | `required mutex ignores defaults` | `test_groups` |
| `test_fail_not_less_than_violation` | `numeric bound validators` | `test_validators` |
| `test_fail_not_bigger_than_violation` | `numeric bound validators` | `test_validators` |
| `test_fail_unknown_option` | `unknown option` | `test_parse_engine` |
| `test_fail_extra_positional` | `extra positional` | `test_parse_engine` |
| `test_fail_unknown_subcommand` | `unknown subcommand is structured failure` | `test_subparsers` |
| `test_fail_removed_argument_used` | `deprecated and removed arguments` | `test_validators` |
| `test_fail_append_missing_value` | `nargs consumption failures` | `test_parse_nargs` |

## Additional rewrite coverage

The current suite also covers gaps that had no meaningful legacy assertion:
transactional registration failures, duplicate aliases and destinations,
invalid action/arity combinations, typed list shape, namespace type errors,
the `--` marker, signed numeric values, repeated parser use, external action,
validator, and formatter extensions, stale group handles, nested subcommands,
child snapshot ownership, namespace merge precedence, and strict parent-copy
conflicts.
