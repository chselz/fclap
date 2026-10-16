module test_delimiters
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, ParseResult, &
        PARSE_SUCCESS, PARSE_FAILURE, ERR_MISSING_VALUE, ERR_INVALID_VALUE, &
        ERR_INVALID_NARGS, ERR_INCOMPATIBLE_ACTION, append
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_delimiters

contains

    subroutine collect_delimiters(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("exact count from one token", test_exact_count), &
            new_unittest("variable counts and append", test_variable_counts), &
            new_unittest("invalid items and counts", test_invalid_items), &
            new_unittest("environment values use the delimiter", &
                test_environment), &
            new_unittest("invalid delimiter definitions", &
                test_invalid_definitions), &
            new_unittest("usage shows the delimiter", test_usage), &
            new_unittest("explicit metavar of a delimited count", &
                test_explicit_metavar) &
        ]
    end subroutine collect_delimiters

    subroutine test_exact_count(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        integer, allocatable :: grid(:)
        character(len=:), allocatable :: input
        integer :: stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--grid", nargs=3, data_type="integer", &
            delimiter=",")
        call parser%add_argument("input")

        ! The following positional is not taken by the option.
        result = parser%parse_tokens([character(len=10) :: &
            "--grid", "64,64,128", "in.dat"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("grid", grid, stat)
        call check(error, all(grid == [64, 64, 128]))
        if (allocated(error)) return
        call result%namespace%get("input", input, stat)
        call check(error, input, "in.dat")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=14) :: &
            "in.dat", "--grid=1,2,3"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("grid", grid, stat)
        call check(error, all(grid == [1, 2, 3]))
        if (allocated(error)) return

        ! Negative items need the attached form: `-1,0,1` alone looks like
        ! an option.
        result = parser%parse_tokens([character(len=14) :: &
            "--grid=-1,0,1", "in.dat"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("grid", grid, stat)
        call check(error, all(grid == [-1, 0, 1]))
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=8) :: &
            "--grid", "-1,0,1", "in.dat"])
        call check(error, result%outcome, PARSE_FAILURE)
    end subroutine test_exact_count

    subroutine test_variable_counts(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: values(:)
        integer :: stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--species", nargs="+", delimiter=",")
        call parser%add_argument("--tags", nargs="*", delimiter=":")
        call parser%add_argument("--lib", nargs="+", action=append(), &
            delimiter=",")

        result = parser%parse_tokens([character(len=10) :: &
            "--species", "H,He,Li", "--tags", "a:b"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("species", values, stat)
        call check(error, size(values), 3)
        if (allocated(error)) return
        call check(error, trim(values(2)), "He")
        if (allocated(error)) return
        call result%namespace%get("tags", values, stat)
        call check(error, size(values), 2)
        if (allocated(error)) return

        ! `*` accepts no token at all, and an empty attached value.
        result = parser%parse_tokens([character(len=10) :: "--tags"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("tags", values, stat)
        call check(error, size(values), 0)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=10) :: "--tags="])
        call result%namespace%get("tags", values, stat)
        call check(error, size(values), 0)
        if (allocated(error)) return

        ! Each occurrence of an append option adds all of its items.
        result = parser%parse_tokens([character(len=12) :: &
            "--lib", "m,blas", "--lib=lapack"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("lib", values, stat)
        call check(error, size(values), 3)
        if (allocated(error)) return
        call check(error, trim(values(3)), "lapack")
    end subroutine test_variable_counts

    subroutine test_invalid_items(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--grid", nargs=3, data_type="integer", &
            delimiter=",")
        call parser%add_argument("--species", nargs="+", delimiter=",")

        result = parser%parse_tokens([character(len=8) :: "--grid", "1,2"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --grid: expected 3 values separated " // &
            "by ',' (got 2)")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: "--grid", "1,x,3"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --grid: invalid integer value (got 'x')")
        if (allocated(error)) return

        ! Empty items are reported, not dropped.
        result = parser%parse_tokens([character(len=8) :: "--grid", "1,,3"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "--species"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=10) :: "--species="])
        entry = result%errors%get(1)
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --species: expected at least one value " // &
            "separated by ',' (got 0)")
    end subroutine test_invalid_items

    subroutine test_environment(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        integer, allocatable :: grid(:)
        integer :: stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--grid", nargs=3, data_type="integer", &
            delimiter=",", env="TOOL_GRID")

        result = parser%parse_tokens([character(len=1) ::], &
            environment=[character(len=16) :: "TOOL_GRID=4,8,16"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("grid", grid, stat)
        call check(error, all(grid == [4, 8, 16]))
        if (allocated(error)) return

        ! Blanks are not separators once a delimiter is declared.
        result = parser%parse_tokens([character(len=1) ::], &
            environment=[character(len=16) :: "TOOL_GRID=4 8 16"])
        call check(error, result%outcome, PARSE_FAILURE)
    end subroutine test_environment

    subroutine test_invalid_definitions(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("files", nargs="+", delimiter=",")
        call parser%add_argument("--one", delimiter=",")
        call parser%add_argument("--a", nargs="+", delimiter="")
        call parser%add_argument("--b", nargs="+", delimiter=",;")
        call parser%add_argument("--c", nargs="+", delimiter="-")

        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 5)
        if (allocated(error)) return
        entry = errors%get(1)
        call check(error, entry%code, ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return
        entry = errors%get(2)
        call check(error, entry%code, ERR_INVALID_NARGS)
    end subroutine test_invalid_definitions

    subroutine test_usage(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--grid", nargs=3, delimiter=",")
        call parser%add_argument("--species", nargs="+", delimiter=",")
        call parser%add_argument("--tags", nargs="*", delimiter=":")

        call check(error, parser%format_usage(), &
            "usage: tool [--grid GRID,GRID,GRID] " // &
            "[--species SPECIES[,SPECIES...]]" // new_line('a') // &
            "       [--tags [TAGS[:TAGS...]]]")
    end subroutine test_usage

    subroutine test_explicit_metavar(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        character(len=1) :: nl

        nl = new_line('a')
        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--efield", data_type="real", nargs=3, &
            delimiter=",", metavar="EX,EY,EZ", help="electric field")
        call parser%add_argument("--field", data_type="real", nargs=3, &
            delimiter=",", metavar="E", help="field")
        ! A metavar derived from choices is repeated despite its comma.
        call parser%add_argument("--axes", nargs=2, delimiter=",", &
            choices=[character(len=1) :: "x", "y"], help="axes")

        call check(error, parser%format_usage(), &
            "usage: tool [--efield EX,EY,EZ] [--field E,E,E] " // &
            "[--axes {x,y},{x,y}]")
        if (allocated(error)) return
        call check(error, parser%format_help(), &
            "usage: tool [--efield EX,EY,EZ] [--field E,E,E] " // &
            "[--axes {x,y},{x,y}]" // nl // nl // "options:" // nl // &
            "  --efield EX,EY,EZ     electric field" // nl // &
            "  --field E,E,E         field" // nl // &
            "  --axes {x,y},{x,y}    axes")
    end subroutine test_explicit_metavar

end module test_delimiters
