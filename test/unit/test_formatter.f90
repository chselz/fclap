module test_formatter
    use fclap, only : ArgumentParser, FormatterType, HelpModel, ParseResult, &
        StandardFormatter, PARSE_SUCCESS, PARSE_HELP, PARSE_VERSION
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_formatter

    type, extends(FormatterType) :: ExternalFormatter
    contains
        procedure :: format_help => external_format_help
        procedure :: format_usage => external_format_usage
    end type ExternalFormatter

contains

    subroutine collect_formatter(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("standard help snapshot", test_standard_help), &
            new_unittest("custom usage and visibility", &
                test_custom_usage_and_visibility), &
            new_unittest("defaults choices and lifecycle", &
                test_annotations), &
            new_unittest("nargs usage forms", test_nargs_usage), &
            new_unittest("deterministic line wrapping", test_wrapping), &
            new_unittest("help model is a snapshot", test_help_model_snapshot), &
            new_unittest("external formatter extension", &
                test_external_formatter), &
            new_unittest("help version and process outcomes", &
                test_control_outcomes) &
        ]
    end subroutine collect_formatter

    subroutine test_standard_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: expected
        character(len=1) :: nl

        nl = new_line('a')
        call parser%init(prog="demo", description="Copy one input file.", &
            epilog="See the manual for details.")
        call parser%add_argument("input", help="input file")
        call parser%add_argument("-o", "--output", default="out.txt", &
            help="output file")

        expected = "usage: demo [-h] INPUT [-o OUTPUT]" // nl // nl // &
            "Copy one input file." // nl // nl // &
            "positional arguments:" // nl // &
            "  INPUT                 input file" // nl // nl // &
            "options:" // nl // &
            "  -h, --help            show this help message and exit" // nl // &
            "  -o OUTPUT, --output OUTPUT" // nl // &
            repeat(" ", 24) // "output file (default: 'out.txt')" // nl // nl // &
            "See the manual for details."

        call check(error, parser%format_usage(), &
            "usage: demo [-h] INPUT [-o OUTPUT]")
        if (allocated(error)) return
        call check(error, parser%format_help(), expected)
    end subroutine test_standard_help

    subroutine test_custom_usage_and_visibility(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text

        call parser%init(prog="demo", usage="demo SOURCE [EXTRA]", &
            add_help=.false.)
        call parser%add_argument("source", help="source file")
        call parser%add_argument("--secret", help="hidden value", &
            visible=.false.)

        call check(error, parser%format_usage(), &
            "usage: demo SOURCE [EXTRA]")
        if (allocated(error)) return
        help_text = parser%format_help()
        call check(error, help_text, &
            "usage: demo SOURCE [EXTRA]" // new_line('a') // new_line('a') // &
            "positional arguments:" // new_line('a') // &
            "  SOURCE                source file")
    end subroutine test_custom_usage_and_visibility

    subroutine test_annotations(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(StandardFormatter) :: formatter
        character(len=:), allocatable :: expected
        character(len=1) :: nl

        nl = new_line('a')
        formatter = StandardFormatter(help_width=120)
        call parser%init(prog="demo", formatter=formatter, add_help=.false.)
        call parser%add_argument("--mode", default="fast", &
            choices=[character(len=4) :: "fast", "safe"], &
            help="execution mode", print_choices=.true.)
        call parser%add_argument("--plain", default="safe", &
            choices=[character(len=4) :: "fast", "safe"])
        call parser%add_argument("--old", help="legacy mode", &
            deprecated_msg="use --mode")
        call parser%add_argument("--nodefault", default="hidden", &
            help="suppressed default", print_default=.false.)
        call parser%add_argument("--jobs", data_type="integer", default=2, &
            help="workers")
        call parser%add_argument("--hidden", default=7, visible=.false., &
            print_choices=.true.)

        expected = &
            "usage: demo [--mode MODE] [--plain PLAIN] [--old OLD] " // &
            "[--nodefault NODEFAULT] [--jobs JOBS]" // &
            nl // nl // "options:" // nl // &
            "  --mode MODE           execution mode (default: 'fast') " // &
            "(choices: ['fast', 'safe'])" // nl // &
            "  --plain PLAIN         (default: 'safe')" // nl // &
            "  --old OLD             legacy mode (deprecated: use --mode)" // &
            nl // "  --nodefault NODEFAULT suppressed default" // nl // &
            "  --jobs JOBS           workers (default: 2)"

        call check(error, parser%format_help(), expected)
    end subroutine test_annotations

    subroutine test_nargs_usage(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(StandardFormatter) :: formatter

        formatter = StandardFormatter(help_width=200)
        call parser%init(prog="tool", formatter=formatter, add_help=.false.)
        call parser%add_argument("head")
        call parser%add_argument("tail", nargs="*")
        call parser%add_argument("--pair", nargs=2)
        call parser%add_argument("--maybe", nargs="?", const="implicit")
        call parser%add_argument("--many", nargs="+")

        call check(error, parser%format_usage(), &
            "usage: tool HEAD [TAIL ...] [--pair PAIR PAIR] " // &
            "[--maybe [MAYBE]] [--many MANY [MANY ...]]")
    end subroutine test_nargs_usage

    subroutine test_wrapping(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(StandardFormatter) :: formatter
        character(len=:), allocatable :: expected
        character(len=1) :: nl

        nl = new_line('a')
        formatter = StandardFormatter(help_width=40)
        call parser%init(prog="wrap", formatter=formatter, add_help=.false., &
            description="Write generated files into the selected directory.")
        call parser%add_argument("--output", &
            help="destination for generated files and reports")

        expected = "usage: wrap [--output OUTPUT]" // nl // nl // &
            "Write generated files into the selected" // nl // &
            "directory." // nl // nl // "options:" // nl // &
            "  --output OUTPUT" // nl // &
            repeat(" ", 13) // "destination for generated" // nl // &
            repeat(" ", 13) // "files and reports"

        call check(error, parser%format_help(), expected)
    end subroutine test_wrapping

    subroutine test_help_model_snapshot(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(HelpModel) :: model

        call parser%init(prog="original", add_help=.false.)
        call parser%add_argument("input")
        model = parser%get_help_model()
        model%prog = "changed"
        model%arguments(1)%metavar = "CHANGED"

        call check(error, parser%format_usage(), "usage: original INPUT")
    end subroutine test_help_model_snapshot

    subroutine test_external_formatter(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ExternalFormatter) :: formatter

        call parser%init(prog="demo", formatter=formatter, add_help=.false.)
        call parser%add_argument("input")

        call check(error, parser%format_usage(), "external usage: demo")
        if (allocated(error)) return
        call check(error, parser%format_help(), "external help: demo (1 argument)")
    end subroutine test_external_formatter

    subroutine test_control_outcomes(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, empty_parser
        type(ParseResult) :: result

        call parser%init(prog="demo", version="demo 1.2")

        result = parser%parse_tokens([character(len=6) :: "--help"])
        call check(error, result%outcome, PARSE_HELP)
        if (allocated(error)) return
        call check(error, result%errors%count(), 0)
        if (allocated(error)) return
        call check(error, result%text, parser%format_help())
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "--version"])
        call check(error, result%outcome, PARSE_VERSION)
        if (allocated(error)) return
        call check(error, result%errors%count(), 0)
        if (allocated(error)) return
        call check(error, result%text, "demo 1.2")
        if (allocated(error)) return

        call empty_parser%init(prog="empty", add_help=.false.)
        result = empty_parser%try_parse_args()
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%errors%count(), 0)
    end subroutine test_control_outcomes

    function external_format_usage(self, model) result(text)
        class(ExternalFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: text

        text = "external usage: " // model%prog
    end function external_format_usage

    function external_format_help(self, model) result(text)
        class(ExternalFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: text
        integer :: count

        count = 0
        if (allocated(model%arguments)) count = size(model%arguments)
        text = "external help: " // model%prog // " (" // &
            integer_text(count) // " argument)"
    end function external_format_help

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

end module test_formatter
