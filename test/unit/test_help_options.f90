module test_help_options
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, ParseResult, &
        StandardFormatter, PARSE_SUCCESS, PARSE_FAILURE, ERR_NO_ARGUMENTS, &
        ERR_INVALID_DEFAULT, store_true
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_help_options

    character(len=1), parameter :: nl = new_line('a')

contains

    subroutine collect_help_options(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("empty command line shows help", test_help_on_empty), &
            new_unittest("subcommand shows its own help when empty", &
                test_child_help_on_empty), &
            new_unittest("choices are described one per line", &
                test_choice_help), &
            new_unittest("choice help wraps under its description", &
                test_choice_help_wraps), &
            new_unittest("help on the label line respects the width", &
                test_first_line_width), &
            new_unittest("choice help needs matching choices", &
                test_invalid_choice_help) &
        ]
    end subroutine collect_help_options

    subroutine test_help_on_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", help_on_empty=.true.)
        call parser%add_argument("--verbose", action=store_true())

        result = parser%parse_tokens([character(len=1) ::])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_NO_ARGUMENTS)
        if (allocated(error)) return
        call check(error, allocated(result%text))
        if (allocated(error)) return
        call check(error, result%text, parser%format_help())
        if (allocated(error)) return

        ! Any token, even a flag, is a normal parse.
        result = parser%parse_tokens([character(len=9) :: "--verbose"])
        call check(error, result%outcome, PARSE_SUCCESS)
    end subroutine test_help_on_empty

    subroutine test_child_help_on_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, run_parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call run_parser%init(help_on_empty=.true.)
        call run_parser%add_argument("--input")
        call parser%init(prog="tool")
        call parser%add_subparsers()
        call parser%add_parser("run", run_parser)

        result = parser%parse_tokens([character(len=3) :: "run"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_NO_ARGUMENTS)
        if (allocated(error)) return
        call check(error, index(result%text, "usage: tool run") == 1, &
            "expected the child's help: " // result%text)
        if (allocated(error)) return

        ! The parent itself still accepts an empty command line.
        result = parser%parse_tokens([character(len=1) ::])
        call check(error, result%outcome, PARSE_SUCCESS)
    end subroutine test_child_help_on_empty

    subroutine test_choice_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--solver", choices=[character(len=8) :: &
            "cg", "gmres", "bicgstab"], help="linear solver", &
            choice_help=[character(len=31) :: "conjugate gradient", &
            "generalized minimal residual", "biconjugate gradient stabilized"])
        call parser%add_argument("--level", data_type="integer", &
            choices=[1, 2], choice_help=[character(len=6) :: "fast", "exact"])

        call check(error, parser%format_help(), &
            "usage: tool [--solver {cg,gmres,bicgstab}] [--level {1,2}]" // &
            nl // nl // &
            "options:" // nl // &
            "  --solver {cg,gmres,bicgstab}" // nl // &
            "                        linear solver" // nl // &
            "                          cg        conjugate gradient" // nl // &
            "                          gmres     generalized minimal residual" &
            // nl // &
            "                          bicgstab  biconjugate gradient " // &
            "stabilized" // nl // &
            "  --level {1,2}" // nl // &
            "                          1  fast" // nl // &
            "                          2  exact")
    end subroutine test_choice_help

    subroutine test_choice_help_wraps(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser

        call parser%init(prog="tool", add_help=.false., &
            formatter=StandardFormatter(help_width=50))
        call parser%add_argument("--solver", choices=[character(len=8) :: &
            "cg", "bicgstab"], choice_help=[character(len=31) :: &
            "conjugate gradient", "biconjugate gradient stabilized"])

        ! Continuation lines align with the choice descriptions.
        call check(error, parser%format_help(), &
            "usage: tool [--solver {cg,bicgstab}]" // nl // nl // &
            "options:" // nl // &
            "  --solver {cg,bicgstab}" // nl // &
            "                  cg        conjugate gradient" // nl // &
            "                  bicgstab  biconjugate gradient" // nl // &
            "                            stabilized")
    end subroutine test_choice_help_wraps

    subroutine test_first_line_width(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser

        call parser%init(prog="tool", add_help=.false., &
            formatter=StandardFormatter(help_width=40))
        call parser%add_argument("-m", action=store_true(), help="one two " // &
            "three four five six seven eight nine ten eleven twelve")

        call check(error, parser%format_help(), &
            "usage: tool [-m]" // nl // nl // &
            "options:" // nl // &
            "  -m         one two three four five six" // nl // &
            "             seven eight nine ten eleven" // nl // &
            "             twelve")
    end subroutine test_first_line_width

    subroutine test_invalid_choice_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--a", choice_help=[character(len=1) :: "x"])
        call parser%add_argument("--b", choices=[character(len=1) :: "x", "y"], &
            choice_help=[character(len=1) :: "x"])

        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 2)
        if (allocated(error)) return
        entry = errors%get(2)
        call check(error, entry%code, ERR_INVALID_DEFAULT)
    end subroutine test_invalid_choice_help

end module test_help_options
