module test_cli_syntax
    use, intrinsic :: iso_fortran_env, only : wp => real64
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, GroupHandle, &
        ParseResult, FCLAP_OK, PARSE_SUCCESS, PARSE_FAILURE, PARSE_HELP, &
        ERR_UNKNOWN_ARGUMENT, ERR_UNEXPECTED_VALUE, ERR_MISSING_VALUE, &
        ERR_INCOMPATIBLE_VALIDATOR, ERR_VALIDATION_FAILED, ERR_MUTEX_CONFLICT, &
        ERR_MUTEX_REQUIRED, append, count, store_true, &
        boolean_optional, not_less_than, in_range
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_cli_syntax

contains

    subroutine collect_cli_syntax(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("long option with equals value", test_long_equals), &
            new_unittest("short option with attached value", &
                test_short_attached), &
            new_unittest("attached value errors", test_attached_errors), &
            new_unittest("combined short flags", test_flag_clusters), &
            new_unittest("lone dash is a value", test_lone_dash), &
            new_unittest("validator type mismatch is a config error", &
                test_validator_type_mismatch), &
            new_unittest("range validator", test_range_validator), &
            new_unittest("mutex diagnostics name options", test_mutex_messages), &
            new_unittest("command-line diagnostic format", test_cli_diagnostics), &
            new_unittest("whole stack uses the diagnostic format", &
                test_stack_diagnostics), &
            new_unittest("usage wrapping keeps items intact", &
                test_usage_wrapping), &
            new_unittest("token lengths keep trailing blanks", &
                test_token_lengths), &
            new_unittest("trailing blanks do not change a name", &
                test_trailing_blank_names), &
            new_unittest("equals after a cluster's value option", &
                test_cluster_equals) &
        ]
    end subroutine collect_cli_syntax

    subroutine test_long_equals(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: text
        character(len=:), allocatable :: tags(:)
        integer :: threads
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("-j", "--threads", data_type="integer")
        call parser%add_argument("--pattern")
        call parser%add_argument("--name")
        call parser%add_argument("--tag", action=append())
        result = parser%parse_tokens([character(len=16) :: &
            "--threads=4", "--pattern=-x=y", "--name=", "--tag=a", "--tag=b"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return

        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 4)
        if (allocated(error)) return
        call result%namespace%get("pattern", text, stat)
        call check(error, text, "-x=y")
        if (allocated(error)) return
        call result%namespace%get("name", text, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, len(text), 0)
        if (allocated(error)) return
        call result%namespace%get("tag", tags, stat)
        call check(error, size(tags), 2)
        if (allocated(error)) return
        call check(error, trim(tags(2)), "b")
    end subroutine test_long_equals

    subroutine test_short_attached(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: text
        integer :: threads
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("-j", "--threads", data_type="integer")
        call parser%add_argument("-I", dest="include")
        call parser%add_argument("-np", data_type="integer")
        result = parser%parse_tokens([character(len=16) :: &
            "-j4", "-I/usr/include", "-np", "8"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 4)
        if (allocated(error)) return
        call result%namespace%get("include", text, stat)
        call check(error, text, "/usr/include")
        if (allocated(error)) return
        ! A registered multi-letter single-dash option still matches exactly.
        call result%namespace%get("np", threads, stat)
        call check(error, threads, 8)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=16) :: "-j=-3"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, -3)
    end subroutine test_short_attached

    subroutine test_attached_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(add_help=.false.)
        call parser%add_argument("--verbose", action=store_true())
        call parser%add_argument("--pair", nargs=2)

        result = parser%parse_tokens([character(len=16) :: "--verbose=yes"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNEXPECTED_VALUE)
        if (allocated(error)) return
        call check(error, entry%arg_name, "verbose")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=16) :: "--pair=a", "b"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=16) :: "--unknown=1"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_ARGUMENT)
    end subroutine test_attached_errors

    subroutine test_flag_clusters(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        integer :: verbosity, threads
        logical :: dry_run
        integer :: stat

        call parser%init()
        call parser%add_argument("-v", action=count(), dest="verbosity")
        call parser%add_argument("-n", "--dry-run", action=store_true())
        call parser%add_argument("-j", data_type="integer", dest="threads")

        result = parser%parse_tokens([character(len=8) :: "-vvn", "-vj", "3"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("verbosity", verbosity, stat)
        call check(error, verbosity, 3)
        if (allocated(error)) return
        call result%namespace%get("dry_run", dry_run, stat)
        call check(error, dry_run, .true.)
        if (allocated(error)) return
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 3)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: "-vj12"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 12)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: "-vh"])
        call check(error, result%outcome, PARSE_HELP)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: "-vx"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_ARGUMENT)
        if (allocated(error)) return
        call check(error, entry%token, "-vx")
    end subroutine test_flag_clusters

    subroutine test_lone_dash(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: input, output
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("input")
        call parser%add_argument("-o", "--output")
        result = parser%parse_tokens([character(len=2) :: "-", "-o", "-"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("input", input, stat)
        call check(error, trim(input), "-")
        if (allocated(error)) return
        call result%namespace%get("output", output, stat)
        call check(error, trim(output), "-")
    end subroutine test_lone_dash

    subroutine test_validator_type_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorEntry) :: entry
        type(ErrorStack) :: errors

        call parser%init(add_help=.false.)
        call parser%add_argument("--tolerance", data_type="real", &
            validator=not_less_than(0))
        call parser%add_argument("--name", validator=not_less_than(0.0_wp))
        call check(error, parser%is_valid(), .false.)
        if (allocated(error)) return
        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 2)
        if (allocated(error)) return
        entry = errors%get(1)
        call check(error, entry%code, ERR_INCOMPATIBLE_VALIDATOR)
        if (allocated(error)) return
        call check(error, entry%arg_name, "tolerance")
    end subroutine test_validator_type_mismatch

    subroutine test_range_validator(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, inverted
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(add_help=.false.)
        call parser%add_argument("--threads", data_type="integer", &
            validator=in_range(1, 64))
        call parser%add_argument("--ratio", data_type="real", &
            validator=in_range(0.0_wp, 1.0_wp))
        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: &
            "--threads", "64", "--ratio", "0"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "--threads", "65"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_VALIDATION_FAILED)
        if (allocated(error)) return
        call check(error, entry%message, "value must be between 1 and 64")
        if (allocated(error)) return

        call inverted%init(add_help=.false.)
        call inverted%add_argument("--threads", data_type="integer", &
            validator=in_range(8, 1))
        call check(error, inverted%is_valid(), .false.)
    end subroutine test_range_validator

    subroutine test_mutex_messages(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        type(GroupHandle) :: group

        call parser%init(add_help=.false.)
        group = parser%add_mutually_exclusive_group(required=.true.)
        call parser%add_argument("-q", "--quiet", action=store_true(), &
            group=group)
        call parser%add_argument("--loud", action=store_true(), group=group)

        result = parser%parse_tokens([character(len=7) :: "--quiet", "--loud"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MUTEX_CONFLICT)
        if (allocated(error)) return
        call check(error, entry%message, &
            "argument --loud: not allowed with argument -q/--quiet")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=1) ::])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MUTEX_REQUIRED)
        if (allocated(error)) return
        call check(error, entry%message, &
            "one of the arguments -q/--quiet --loud is required")
    end subroutine test_mutex_messages

    subroutine test_cli_diagnostics(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("input")
        call parser%add_argument("-j", "--threads", data_type="integer", &
            validator=not_less_than(1))
        call parser%add_argument("--old", deprecated_msg="use --new")

        result = parser%parse_tokens([character(len=9) :: "in", "-j", "0"])
        call check(error, parser%format_diagnostic(result%errors%get(1)), &
            "tool: error: argument -j/--threads: value must not be less " // &
            "than 1 (got '0')")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "in", "-j", "x"])
        call check(error, parser%format_diagnostic(result%errors%get(1)), &
            "tool: error: argument -j/--threads: invalid integer value " // &
            "(got 'x')")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "in", "--bogus"])
        call check(error, parser%format_diagnostic(result%errors%get(1)), &
            "tool: error: unknown option '--bogus'")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "in", "--old", "x"])
        call check(error, parser%format_diagnostic(result%errors%get(1)), &
            "tool: warning: argument --old: use --new")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=1) ::])
        call check(error, parser%format_diagnostic(result%errors%get(1)), &
            "tool: error: argument INPUT: required argument was not supplied")
    end subroutine test_cli_diagnostics

    subroutine test_stack_diagnostics(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, broken
        type(ParseResult) :: result

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("input")
        call parser%add_argument("--old", deprecated_msg="use --new")

        result = parser%parse_tokens([character(len=7) :: "in", "--old", "x", &
            "--bogus"])
        call check(error, parser%format_diagnostics(result%errors), &
            "tool: warning: argument --old: use --new" // new_line("a") // &
            "tool: error: unknown option '--bogus'")
        if (allocated(error)) return

        call broken%init(prog="tool", add_help=.false.)
        call broken%add_argument("-x", "--output")
        call broken%add_argument("--output")
        call check(error, broken%format_diagnostics(broken%get_config_errors()), &
            "tool: invalid parser definition: argument --output: " // &
            "an option or positional name is already registered")
    end subroutine test_stack_diagnostics

    subroutine test_usage_wrapping(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: expected

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--alpha-option")
        call parser%add_argument("--beta-option")
        call parser%add_argument("--gamma-option")
        call parser%add_argument("--k", dest="k")

        expected = "usage: tool [--alpha-option ALPHA_OPTION] " // &
            "[--beta-option BETA_OPTION]" // new_line("a") // &
            "       [--gamma-option GAMMA_OPTION] [--k K]"
        call check(error, parser%format_usage(), expected)
    end subroutine test_usage_wrapping

    subroutine test_token_lengths(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, child
        type(ParseResult) :: result
        character(len=:), allocatable :: text

        call parser%init(prog="tool")
        call parser%add_argument("--sep")
        call parser%add_argument("name", nargs="?")
        call parser%add_subparsers()
        call child%init(prog="run")
        call child%add_argument("--sep")
        call parser%add_parser("run", child)

        ! Without lengths, trailing blanks are padding of the array.
        result = parser%parse_tokens([character(len=5) :: "--sep", ", "])
        call result%namespace%get("sep", text)
        call check(error, text, ",")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=5) :: "--sep", ", "], &
            lengths=[5, 2])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("sep", text)
        call check(error, text, ", ")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: "--sep=, "], &
            lengths=[8])
        call result%namespace%get("sep", text)
        call check(error, text, ", ")
        if (allocated(error)) return

        ! An empty token is an empty value, not a missing one.
        result = parser%parse_tokens([character(len=5) :: "--sep", ""], &
            lengths=[5, 0])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("sep", text)
        call check(error, len(text), 0)
        if (allocated(error)) return

        ! The subcommand sees the lengths of its slice of the line.
        result = parser%parse_tokens( &
            [character(len=5) :: "run", "--sep", " x "], lengths=[3, 5, 3])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("sep", text)
        call check(error, text, " x ")
    end subroutine test_token_lengths

    !> Trailing blanks are ignored when a token names an option, the
    !> end-of-options marker, or a command, even with exact lengths, so a
    !> stray blank in a script still selects what the user meant.
    subroutine test_trailing_blank_names(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, commands
        type(ParseResult) :: result
        character(len=:), allocatable :: rest(:)
        logical :: verbose, quiet, cache

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-v", action=store_true())
        call parser%add_argument("--quiet", action=store_true())
        call parser%add_argument("--cache", action=boolean_optional())
        call parser%add_argument("rest", nargs="*")

        result = parser%parse_tokens([character(len=11) :: &
            "-v ", "--quiet ", "--no-cache "], lengths=[3, 8, 11])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("v", verbose)
        call check(error, verbose, .true.)
        if (allocated(error)) return
        call result%namespace%get("quiet", quiet)
        call check(error, quiet, .true.)
        if (allocated(error)) return
        call result%namespace%get("cache", cache)
        call check(error, cache, .false.)
        if (allocated(error)) return

        ! `"-- "` still ends option processing.
        result = parser%parse_tokens([character(len=3) :: "-- ", "-v"], &
            lengths=[3, 2])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("rest", rest)
        call check(error, size(rest), 1)
        if (allocated(error)) return
        call check(error, rest(1), "-v")
        if (allocated(error)) return

        call commands%init(prog="tool", add_help=.false.)
        call commands%add_subparsers()
        call commands%add_parser("run")
        result = commands%parse_tokens([character(len=4) :: "run "], &
            lengths=[4])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%selected_command_path(1), "run")
    end subroutine test_trailing_blank_names

    !> `-vn=3` passes `3` to `-n`, as `-n=3` does.
    subroutine test_cluster_equals(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        integer :: number, verbosity

        call parser%init(add_help=.false.)
        call parser%add_argument("-v", action=count())
        call parser%add_argument("-n", data_type="integer")

        result = parser%parse_tokens([character(len=5) :: "-vn=3"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("n", number)
        call check(error, number, 3)
        if (allocated(error)) return
        call result%namespace%get("v", verbosity)
        call check(error, verbosity, 1)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=5) :: "-vn3"])
        call result%namespace%get("n", number)
        call check(error, number, 3)
    end subroutine test_cluster_equals

end module test_cli_syntax
