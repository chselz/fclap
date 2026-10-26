module test_usability
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, GroupHandle, &
        Namespace, ParseResult, StandardFormatter, FCLAP_OK, &
        PARSE_SUCCESS, PARSE_FAILURE, PARSE_HELP, PARSE_VERSION, &
        ERR_INVALID_GROUP, ERR_DUPLICATE_OPTION, ERR_UNKNOWN_ARGUMENT, &
        ERR_INVALID_VALUE, ERR_UNEXPECTED_VALUE, ERR_NAMESPACE_MISSING_KEY, &
        ERROR_WARNING, append, boolean_optional, store_true
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_usability

contains

    subroutine collect_usability(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("mutex members must be optional", &
                test_mutex_members_optional), &
            new_unittest("subcommand errors use absolute token positions", &
                test_subcommand_token_positions), &
            new_unittest("definition errors name the option", &
                test_definition_diagnostics), &
            new_unittest("boolean optional flags", test_boolean_optional), &
            new_unittest("append replaces its default", &
                test_append_replaces_default), &
            new_unittest("later help wins over unknown tokens", &
                test_help_after_unknown), &
            new_unittest("help hides implicit defaults and shows choices", &
                test_help_presentation), &
            new_unittest("namespace remove", test_namespace_remove), &
            new_unittest("error token offsets", test_error_offsets) &
        ]
    end subroutine collect_usability

    subroutine test_mutex_members_optional(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(GroupHandle) :: group
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        group = parser%add_mutually_exclusive_group()
        call parser%add_argument("--x", required=.true., group=group)
        call parser%add_argument("input", group=group)
        ! Omissible members remain valid, including a '?' positional.
        call parser%add_argument("--y", group=group)
        call parser%add_argument("maybe", nargs="?", group=group)

        call check(error, parser%argument_count(), 2)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 2)
        if (allocated(error)) return
        entry = errors%get(1)
        call check(error, entry%code, ERR_INVALID_GROUP)
        if (allocated(error)) return
        call check(error, entry%message, &
            "mutually exclusive arguments must be optional")
        if (allocated(error)) return
        call check(error, entry%arg_name, "x")
        if (allocated(error)) return
        entry = errors%get(2)
        call check(error, entry%arg_name, "input")
    end subroutine test_mutex_members_optional

    subroutine test_subcommand_token_positions(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, run
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--verbose", action=store_true())
        call parser%add_subparsers()
        call run%init(add_help=.false.)
        call run%add_argument("--count", data_type="integer")
        call parser%add_parser("run", run)

        result = parser%parse_tokens([character(len=9) :: &
            "--verbose", "run", "--count", "x"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, entry%token, "x")
        if (allocated(error)) return
        call check(error, entry%token_index, 4)
    end subroutine test_subcommand_token_positions

    subroutine test_definition_diagnostics(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-o", "--output")
        call parser%add_argument("-x", "--output")
        errors = parser%get_config_errors()
        entry = errors%get(1)
        call check(error, entry%code, ERR_DUPLICATE_OPTION)
        if (allocated(error)) return
        call check(error, entry%token, "-x/--output")
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: invalid parser definition: argument -x/--output: " // &
            "an option or positional name is already registered")
    end subroutine test_definition_diagnostics

    subroutine test_boolean_optional(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        type(ErrorStack) :: errors
        logical :: cache, color
        integer :: stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-c", "--cache", action=boolean_optional(), &
            default=.true., help="reuse cached results")
        call parser%add_argument("--color", action=boolean_optional())
        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        call check(error, parser%has_option("--no-cache"), .true.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=1) ::])
        call result%namespace%get("cache", cache, stat)
        call check(error, cache, .true.)
        if (allocated(error)) return
        call result%namespace%get("color", color, stat)
        call check(error, color, .false.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=10) :: &
            "--no-cache", "--color"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("cache", cache, stat)
        call check(error, cache, .false.)
        if (allocated(error)) return
        call result%namespace%get("color", color, stat)
        call check(error, color, .true.)
        if (allocated(error)) return

        ! The last spelling wins, and the short alias means "true".
        result = parser%parse_tokens([character(len=10) :: "--no-cache", "-c"])
        call result%namespace%get("cache", cache, stat)
        call check(error, cache, .true.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=12) :: "--no-color=1"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNEXPECTED_VALUE)
        if (allocated(error)) return

        call check(error, parser%format_help(), &
            "usage: tool [-c] [--color]" // new_line("a") // new_line("a") // &
            "options:" // new_line("a") // &
            "  -c, --cache, --no-cache" // new_line("a") // &
            repeat(" ", 24) // "reuse cached results (default: .true.)" // &
            new_line("a") // "  --color, --no-color")
        if (allocated(error)) return

        ! The generated spelling takes part in collision checks.
        call parser%add_argument("--no-color", action=store_true())
        errors = parser%get_config_errors()
        entry = errors%get(1)
        call check(error, entry%code, ERR_DUPLICATE_OPTION)
    end subroutine test_boolean_optional

    subroutine test_append_replaces_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: tags(:)
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--tag", action=append(), &
            default=[character(len=4) :: "base", "core"])

        result = parser%parse_tokens([character(len=1) ::])
        call result%namespace%get("tag", tags, stat)
        call check(error, size(tags), 2)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=5) :: &
            "--tag", "x", "--tag", "y"])
        call result%namespace%get("tag", tags, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(tags), 2)
        if (allocated(error)) return
        call check(error, trim(tags(1)), "x")
        if (allocated(error)) return
        call check(error, trim(tags(2)), "y")
    end subroutine test_append_replaces_default

    subroutine test_help_after_unknown(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result

        call parser%init(prog="tool", version="tool 1.0")
        call parser%add_argument("--count", data_type="integer")
        call parser%add_argument("--old", deprecated_msg="use --count")

        result = parser%parse_tokens([character(len=9) :: "--bogus", "-h"])
        call check(error, result%outcome, PARSE_HELP)
        if (allocated(error)) return
        call check(error, result%errors%has_fatal_errors(), .false.)
        if (allocated(error)) return
        call check(error, allocated(result%text), .true.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: &
            "--old", "1", "stray", "--version"])
        call check(error, result%outcome, PARSE_VERSION)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        call check(error, result%errors%has_warnings(), .true.)
        if (allocated(error)) return

        ! A value error is reported even when help follows, as in argparse.
        result = parser%parse_tokens([character(len=9) :: &
            "--count", "x", "-h"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return

        ! Help after '--' is data, not a request.
        result = parser%parse_tokens([character(len=9) :: "--bogus", "--", "-h"])
        call check(error, result%outcome, PARSE_FAILURE)
    end subroutine test_help_after_unknown

    subroutine test_help_presentation(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(StandardFormatter) :: formatter
        character(len=1), parameter :: nl = new_line("a")

        formatter = StandardFormatter(help_width=100)
        call parser%init(prog="tool", formatter=formatter, add_help=.false.)
        call parser%add_argument("mode", choices=[character(len=4) :: &
            "fast", "safe"], help="execution mode")
        call parser%add_argument("-v", "--verbose", action=store_true(), &
            help="talk more")
        call parser%add_argument("--level", data_type="integer", &
            choices=[1, 2], default=1, metavar="N", help="level")

        call check(error, parser%format_help(), &
            "usage: tool [-v] [--level N] {fast,safe}" // nl // nl // &
            "positional arguments:" // nl // &
            "  {fast,safe}           execution mode" // nl // nl // &
            "options:" // nl // &
            "  -v, --verbose         talk more" // nl // &
            "  --level N             level (default: 1)")
    end subroutine test_help_presentation

    subroutine test_namespace_remove(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        integer :: value
        integer :: stat

        call args%set("a", 1)
        call args%set("b", 2)
        call args%set("c", 3)
        call args%remove("b")
        call args%remove("missing")
        call check(error, args%size(), 2)
        if (allocated(error)) return
        call check(error, args%contains("b"), .false.)
        if (allocated(error)) return
        call args%get("c", value, stat)
        call check(error, value, 3)
        if (allocated(error)) return
        call args%get("b", value, stat)
        call check(error, stat, ERR_NAMESPACE_MISSING_KEY)
    end subroutine test_namespace_remove

    subroutine test_error_offsets(error)
        type(error_type), allocatable, intent(out) :: error
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry

        call errors%add("first", token="x", token_index=2)
        call errors%add("no position", severity=ERROR_WARNING)
        call errors%offset_token_indices(3)
        entry = errors%get(1)
        call check(error, entry%token_index, 5)
        if (allocated(error)) return
        entry = errors%get(2)
        call check(error, entry%token_index, 0)
    end subroutine test_error_offsets

end module test_usability
