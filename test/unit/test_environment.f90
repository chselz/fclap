module test_environment
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, GroupHandle, &
        ParseResult, PARSE_SUCCESS, PARSE_FAILURE, SOURCE_NONE, &
        SOURCE_DEFAULT, SOURCE_ENVIRONMENT, SOURCE_COMMAND_LINE, &
        ERR_INVALID_ENV, ERR_INVALID_VALUE, ERR_VALIDATION_FAILED, &
        ERR_MISSING_VALUE, ERR_UNEXPECTED_VALUE, ERR_ARGUMENT_CONFLICT, &
        ERR_MUTEX_CONFLICT, boolean_optional, count, not_less_than, store_true
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_environment

    character(len=1), parameter :: no_tokens(0) = [character(len=1) ::]

contains

    subroutine collect_environment(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("command line, environment, default precedence", &
                test_precedence), &
            new_unittest("environment values are validated", &
                test_invalid_values), &
            new_unittest("flags read logical values", test_flags), &
            new_unittest("list options split at blanks", test_lists), &
            new_unittest("environment values count as given", &
                test_presence), &
            new_unittest("invalid env definitions", test_invalid_definitions), &
            new_unittest("help names the variable", test_help), &
            new_unittest("subcommands read and report sources", &
                test_subcommands), &
            new_unittest("counts read a number of occurrences", test_count), &
            new_unittest("wrong value counts name both counts", &
                test_list_counts), &
            new_unittest("conflicts name the variable", test_conflict_source), &
            new_unittest("invalid values are not also missing", &
                test_invalid_required) &
        ]
    end subroutine collect_environment

    subroutine test_precedence(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        integer :: threads, stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-j", "--threads", data_type="integer", &
            default=1, env="TOOL_THREADS")
        call parser%add_argument("--label")

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_THREADS=8"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 8)
        if (allocated(error)) return
        call check(error, result%value_source("threads"), SOURCE_ENVIRONMENT)
        if (allocated(error)) return
        call check(error, result%value_source("label"), SOURCE_NONE)
        if (allocated(error)) return
        call check(error, result%value_source("no_such_dest"), SOURCE_NONE)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "--threads", "2"], &
            environment=[character(len=16) :: "TOOL_THREADS=8"])
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 2)
        if (allocated(error)) return
        call check(error, result%value_source("threads"), SOURCE_COMMAND_LINE)
        if (allocated(error)) return

        ! An empty explicit environment hides the process environment.
        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=1) ::])
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 1)
        if (allocated(error)) return
        call check(error, result%value_source("threads"), SOURCE_DEFAULT)
        if (allocated(error)) return

        ! An empty variable counts as unset; the last definition wins.
        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_THREADS="])
        call check(error, result%value_source("threads"), SOURCE_DEFAULT)
        if (allocated(error)) return
        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_THREADS=3", &
            "TOOL_THREAD=5", "TOOL_THREADS=4"])
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 4)
    end subroutine test_precedence

    subroutine test_invalid_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-j", "--threads", data_type="integer", &
            validator=not_less_than(1), env="TOOL_THREADS")
        call parser%add_argument("--mode", choices=[character(len=4) :: &
            "fast", "safe"], env="TOOL_MODE")

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_THREADS=abc"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument -j/--threads: environment variable " // &
            "TOOL_THREADS: invalid integer value (got 'abc')")
        if (allocated(error)) return

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_THREADS=0"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_VALIDATION_FAILED)
        if (allocated(error)) return

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_MODE=fsat"])
        entry = result%errors%get(1)
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --mode: environment variable TOOL_MODE: " // &
            "invalid choice: choose from 'fast', 'safe' (got 'fsat') " // &
            "(did you mean 'fast'?)")
    end subroutine test_invalid_values

    subroutine test_flags(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        logical :: flag
        integer :: level, stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--verbose", action=store_true(), &
            env="TOOL_VERBOSE")
        call parser%add_argument("--color", action=boolean_optional(), &
            env="TOOL_COLOR")
        call parser%add_argument("-d", action=count(), dest="debug", &
            env="TOOL_DEBUG")

        result = parser%parse_tokens(no_tokens, environment=[ &
            character(len=16) :: "TOOL_VERBOSE=yes", "TOOL_COLOR=off", &
            "TOOL_DEBUG=1"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("verbose", flag, stat)
        call check(error, flag)
        if (allocated(error)) return
        call check(error, result%value_source("verbose"), SOURCE_ENVIRONMENT)
        if (allocated(error)) return
        ! A negatable flag stores false instead of staying unset.
        call result%namespace%get("color", flag, stat)
        call check(error, stat, 0)
        if (allocated(error)) return
        call check(error, flag, .false.)
        if (allocated(error)) return
        call check(error, result%value_source("color"), SOURCE_ENVIRONMENT)
        if (allocated(error)) return
        call result%namespace%get("debug", level, stat)
        call check(error, level, 1)
        if (allocated(error)) return

        ! A false value leaves an ordinary flag at its default.
        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_VERBOSE=0"])
        call result%namespace%get("verbose", flag, stat)
        call check(error, flag, .false.)
        if (allocated(error)) return
        call check(error, result%value_source("verbose"), SOURCE_DEFAULT)
        if (allocated(error)) return

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=18) :: "TOOL_VERBOSE=maybe"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --verbose: environment variable " // &
            "TOOL_VERBOSE: invalid logical value (got 'maybe')")
    end subroutine test_flags

    subroutine test_lists(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        integer, allocatable :: dims(:)
        integer :: stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--dims", nargs="+", data_type="integer", &
            env="TOOL_DIMS")
        call parser%add_argument("--pair", nargs=2, env="TOOL_PAIR")

        result = parser%parse_tokens(no_tokens, environment=[ &
            character(len=24) :: "TOOL_DIMS=  64 64" // achar(9) // "128 "])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("dims", dims, stat)
        call check(error, stat, 0)
        if (allocated(error)) return
        call check(error, size(dims), 3)
        if (allocated(error)) return
        call check(error, all(dims == [64, 64, 128]))
        if (allocated(error)) return

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_PAIR=a b c"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNEXPECTED_VALUE)
    end subroutine test_lists

    subroutine test_presence(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--token", required=.true., env="TOOL_TOKEN")
        call parser%add_argument("--dry-run", action=store_true(), &
            env="TOOL_DRY_RUN")
        call parser%add_argument("--submit", action=store_true())
        call parser%add_conflict("--dry-run", "--submit")

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=17) :: "TOOL_TOKEN=secret"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: "--submit"], &
            environment=[character(len=17) :: "TOOL_TOKEN=secret", &
            "TOOL_DRY_RUN=1"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_ARGUMENT_CONFLICT)
    end subroutine test_presence

    subroutine test_invalid_definitions(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry
        integer :: index

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("input", env="TOOL_INPUT")
        call parser%add_argument("--a", env="")
        call parser%add_argument("--b", env="X=Y")
        call parser%add_argument("--c", removed_msg="gone", env="TOOL_C")

        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 4)
        if (allocated(error)) return
        do index = 1, errors%count()
            entry = errors%get(index)
            call check(error, entry%code, ERR_INVALID_ENV)
            if (allocated(error)) return
        end do
    end subroutine test_invalid_definitions

    subroutine test_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-j", "--threads", data_type="integer", &
            default=1, env="TOOL_THREADS", help="worker count")
        help = parser%format_help()
        call check(error, index(help, &
            "worker count (env: TOOL_THREADS) (default: 1)") > 0, &
            "help does not name the variable: " // help)
    end subroutine test_help

    subroutine test_subcommands(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, run_parser
        type(ParseResult) :: result
        integer :: nodes, stat

        call run_parser%init(add_help=.false.)
        call run_parser%add_argument("--nodes", data_type="integer", &
            default=1, env="TOOL_NODES")
        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--verbose", action=store_true())
        call parser%add_subparsers(dest="command")
        call parser%add_parser("run", run_parser)

        result = parser%parse_tokens([character(len=9) :: "--verbose", "run"], &
            environment=[character(len=16) :: "TOOL_NODES=4"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("nodes", nodes, stat)
        call check(error, nodes, 4)
        if (allocated(error)) return
        call check(error, result%value_source("nodes"), SOURCE_ENVIRONMENT)
        if (allocated(error)) return
        call check(error, result%value_source("command"), SOURCE_COMMAND_LINE)
        if (allocated(error)) return
        call check(error, result%value_source("verbose"), SOURCE_COMMAND_LINE)
    end subroutine test_subcommands


    !> A count reads the number of occurrences.
    subroutine test_count(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        integer :: level, stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-v", action=count(), dest="verbose", &
            env="TOOL_VERBOSE")

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=14) :: "TOOL_VERBOSE=3"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("verbose", level, stat)
        call check(error, level, 3)
        if (allocated(error)) return
        call check(error, result%value_source("verbose"), SOURCE_ENVIRONMENT)
        if (allocated(error)) return

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=14) :: "TOOL_VERBOSE=0"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%value_source("verbose"), SOURCE_DEFAULT)
        if (allocated(error)) return

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=15) :: "TOOL_VERBOSE=-1"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
    end subroutine test_count

    subroutine test_list_counts(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--pair", nargs=2, env="TOOL_PAIR")

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_PAIR=a b c"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNEXPECTED_VALUE)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --pair: environment variable " // &
            "TOOL_PAIR: expected 2 values (got 3)")
        if (allocated(error)) return

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=16) :: "TOOL_PAIR=a"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --pair: environment variable " // &
            "TOOL_PAIR: expected 2 values (got 1)")
    end subroutine test_list_counts

    !> A conflict or dependency error caused by an environment value names
    !> the variable, which the command line does not show.
    subroutine test_conflict_source(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, mutex_parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        type(GroupHandle) :: group

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--a", env="A_ENV")
        call parser%add_argument("--b", action=store_true())
        call parser%add_conflict("--a", "--b")
        result = parser%parse_tokens([character(len=3) :: "--b"], &
            environment=[character(len=7) :: "A_ENV=1"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_ARGUMENT_CONFLICT)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --a: not allowed with argument --b " // &
            "(--a from environment variable A_ENV)")
        if (allocated(error)) return

        call mutex_parser%init(prog="tool", add_help=.false.)
        group = mutex_parser%add_mutually_exclusive_group()
        call mutex_parser%add_argument("--a", env="A_ENV", group=group)
        call mutex_parser%add_argument("--b", action=store_true(), group=group)
        result = mutex_parser%parse_tokens([character(len=3) :: "--b"], &
            environment=[character(len=7) :: "A_ENV=1"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MUTEX_CONFLICT)
        if (allocated(error)) return
        call check(error, index(entry%message, &
            "(--a from environment variable A_ENV)") > 0)
    end subroutine test_conflict_source

    !> An invalid environment value is reported once: its option is not also
    !> missing, and other variables are still applied.
    subroutine test_invalid_required(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--n", data_type="integer", required=.true., &
            env="TOOL_N")
        call parser%add_argument("--m", data_type="integer", required=.true., &
            env="TOOL_M")

        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=8) :: "TOOL_N=x", "TOOL_M=2"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return

        ! Both invalid values are reported.
        result = parser%parse_tokens(no_tokens, &
            environment=[character(len=8) :: "TOOL_N=x", "TOOL_M=y"])
        call check(error, result%errors%count(), 2)
    end subroutine test_invalid_required

end module test_environment
