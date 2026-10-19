module test_rules
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, ParseResult, &
        PARSE_SUCCESS, PARSE_FAILURE, ERR_INVALID_RULE, &
        ERR_MISSING_DEPENDENCY, ERR_ARGUMENT_CONFLICT, ERR_MISSING_REQUIRED, &
        store_true
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_rules

contains

    subroutine collect_rules(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("requires", test_requires), &
            new_unittest("requires several arguments", test_requires_many), &
            new_unittest("conflicts", test_conflicts), &
            new_unittest("required unless", test_required_unless), &
            new_unittest("rules accept destinations and spellings", &
                test_references), &
            new_unittest("invalid rules are configuration errors", &
                test_invalid_rules), &
            new_unittest("rules are inherited and kept by subcommands", &
                test_rules_copied) &
        ]
    end subroutine collect_rules

    subroutine test_requires(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--restart", action=store_true())
        call parser%add_argument("--restart-file")
        call parser%add_requires("--restart-file", "--restart")
        call check(error, parser%rule_count(), 1)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=14) :: &
            "--restart-file", "state.h5"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_DEPENDENCY)
        if (allocated(error)) return
        call check(error, entry%arg_name, "restart_file")
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --restart-file: requires --restart")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=14) :: &
            "--restart-file", "state.h5", "--restart"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return

        ! The rule is one-directional and says nothing about absent subjects.
        result = parser%parse_tokens([character(len=14) :: "--restart"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=1) ::])
        call check(error, result%outcome, PARSE_SUCCESS)
    end subroutine test_requires

    subroutine test_requires_many(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--output")
        call parser%add_argument("-f", "--format", default="txt")
        call parser%add_argument("--level", data_type="integer")
        call parser%add_requires("output", [character(len=6) :: &
            "format", "level"])

        ! A default does not count: presence means given on the command line.
        result = parser%parse_tokens([character(len=8) :: "--output", "o"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_DEPENDENCY)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --output: requires -f/--format, --level")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: &
            "--output", "o", "-f", "csv"])
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --output: requires --level")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: &
            "--output", "o", "-f", "csv", "--level", "2"])
        call check(error, result%outcome, PARSE_SUCCESS)
    end subroutine test_requires_many

    subroutine test_conflicts(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--dry-run", action=store_true())
        call parser%add_argument("-s", "--submit", action=store_true())
        call parser%add_argument("--queue")
        call parser%add_conflict("--dry-run", [character(len=8) :: &
            "--submit", "--queue"])

        result = parser%parse_tokens([character(len=9) :: "--dry-run", "-s"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_ARGUMENT_CONFLICT)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --dry-run: not allowed with argument " // &
            "-s/--submit")
        if (allocated(error)) return

        ! Order on the command line does not matter, and every conflicting
        ! argument is reported.
        result = parser%parse_tokens([character(len=9) :: &
            "--queue", "short", "--submit", "--dry-run"])
        call check(error, result%errors%count(), 2)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "--submit", &
            "--queue", "long"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=9) :: "--dry-run"])
        call check(error, result%outcome, PARSE_SUCCESS)
    end subroutine test_conflicts

    subroutine test_required_unless(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--input")
        call parser%add_argument("--generate", action=store_true())
        call parser%add_argument("--demo", action=store_true())
        call parser%add_required_unless("input", [character(len=10) :: &
            "--generate", "--demo"])

        result = parser%parse_tokens([character(len=1) ::])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_REQUIRED)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --input: required unless --generate " // &
            "or --demo is given")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=7) :: "--input", "a.dat"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=7) :: "--demo"])
        call check(error, result%outcome, PARSE_SUCCESS)
    end subroutine test_required_unless

    subroutine test_references(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-v", "--verbose", action=store_true())
        call parser%add_argument("-q", "--quiet", action=store_true())
        call parser%add_argument("files", nargs="*")
        ! Short spelling, long spelling, and destination name the same rule
        ! participants; a positional can take part as well.
        call parser%add_conflict("-v", "quiet")
        call parser%add_requires("files", "--verbose")
        call check(error, parser%is_valid())
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "--verbose", "-q"])
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_ARGUMENT_CONFLICT)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=7) :: "a.dat"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=7) :: "-v", "a.dat"])
        call check(error, result%outcome, PARSE_SUCCESS)
    end subroutine test_references

    subroutine test_invalid_rules(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry
        integer :: index

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("input")
        call parser%add_argument("--mode", required=.true.)
        call parser%add_argument("--fast", action=store_true())

        call parser%add_requires("--missing", "--fast")
        call parser%add_requires("--fast", "--missing")
        call parser%add_requires("--fast", "fast")
        call parser%add_requires("--fast", [character(len=1) ::])
        call parser%add_required_unless("--mode", "--fast")
        call parser%add_required_unless("input", "--fast")
        call parser%add_conflict("--fast", "--mode")

        call check(error, parser%rule_count(), 0)
        if (allocated(error)) return
        call check(error, parser%is_valid(), .false.)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 7)
        if (allocated(error)) return
        do index = 1, errors%count()
            entry = errors%get(index)
            call check(error, entry%code, ERR_INVALID_RULE)
            if (allocated(error)) return
        end do

        entry = errors%get(2)
        call check(error, parser%format_diagnostic(entry), &
            "tool: invalid parser definition: argument --fast: the " // &
            "requires rule names an unknown argument '--missing'")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=6) :: &
            "in", "--mode", "x"])
        call check(error, result%outcome, PARSE_FAILURE)
    end subroutine test_invalid_rules

    subroutine test_rules_copied(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: common, parser, run_parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call common%init(add_help=.false.)
        call common%add_argument("--dry-run", action=store_true())
        call common%add_argument("--submit", action=store_true())
        call common%add_conflict("--dry-run", "--submit")

        call run_parser%init_with_parents([common], add_help=.false.)
        call run_parser%add_argument("--nodes", data_type="integer")
        call run_parser%add_requires("--nodes", "--submit")
        call check(error, run_parser%rule_count(), 2)
        if (allocated(error)) return

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_subparsers(dest="command")
        call parser%add_parser("run", run_parser)

        result = parser%parse_tokens([character(len=9) :: &
            "run", "--dry-run", "--submit"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_ARGUMENT_CONFLICT)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: &
            "run", "--nodes", "4"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_DEPENDENCY)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: &
            "run", "--nodes", "4", "--submit"])
        call check(error, result%outcome, PARSE_SUCCESS)
    end subroutine test_rules_copied

end module test_rules
