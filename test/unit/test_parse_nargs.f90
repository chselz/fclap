module test_parse_nargs
    use fclap, only : ArgumentParser, ErrorEntry, ParseResult, ip, wp, &
        FCLAP_OK, PARSE_SUCCESS, PARSE_FAILURE, ERR_MISSING_VALUE
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_parse_nargs

contains

    subroutine collect_parse_nargs(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("exact option and positional counts", test_exact_counts), &
            new_unittest("optional star and plus options", &
                test_symbolic_options), &
            new_unittest("variable positional allocation", &
                test_variable_positionals), &
            new_unittest("remainder positional", test_remainder), &
            new_unittest("nargs consumption failures", test_nargs_failures) &
        ]
    end subroutine collect_parse_nargs

    subroutine test_exact_counts(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, positional_parser
        type(ParseResult) :: result
        real(wp), allocatable :: point(:)
        integer(ip), allocatable :: ids(:)
        character(len=16) :: label
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--point", nargs=3, data_type="real")
        result = parser%parse_tokens([character(len=8) :: &
            "--point", "1.0", "-2.5", "3.0"])

        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("point", point, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(point), 3)
        if (allocated(error)) return
        call check(error, all(point == [1.0_wp, -2.5_wp, 3.0_wp]), .true.)
        if (allocated(error)) return

        call positional_parser%init(add_help=.false.)
        call positional_parser%add_argument("ids", nargs=2, data_type="integer")
        call positional_parser%add_argument("label")
        result = positional_parser%parse_tokens([character(len=5) :: &
            "1", "2", "final"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("ids", ids, stat)
        call check(error, all(ids == [1_ip, 2_ip]), .true.)
        if (allocated(error)) return
        call result%namespace%get("label", label, stat)
        call check(error, trim(label), "final")
    end subroutine test_exact_counts

    subroutine test_symbolic_options(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: all_values(:), some_values(:)
        character(len=16) :: color
        integer :: stat

        call parser%init(add_help=.false.)
        allocate(character(len=1) :: all_values(0), some_values(0))
        call parser%add_argument("--color", nargs="?", const="auto")
        call parser%add_argument("--all", nargs="*")
        call parser%add_argument("--some", nargs="+")
        result = parser%parse_tokens([character(len=7) :: &
            "--color", "--all", "--some", "x", "y"])

        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("color", color, stat)
        call check(error, trim(color), "auto")
        if (allocated(error)) return
        call result%namespace%get("all", all_values, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(all_values), 0)
        if (allocated(error)) return
        call result%namespace%get("some", some_values, stat)
        call check(error, size(some_values), 2)
        if (allocated(error)) return
        call check(error, trim(some_values(2)), "y")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=7) :: "--color", "on"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("color", color, stat)
        call check(error, trim(color), "on")
    end subroutine test_symbolic_options

    subroutine test_variable_positionals(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, optional_parser, plus_parser
        type(ParseResult) :: result
        character(len=:), allocatable :: files(:)
        character(len=16) :: output
        integer :: stat

        call parser%init(add_help=.false.)
        allocate(character(len=1) :: files(0))
        call parser%add_argument("files", nargs="*")
        call parser%add_argument("output")
        result = parser%parse_tokens([character(len=5) :: "a.in", "b.out"])

        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("files", files, stat)
        call check(error, size(files), 1)
        if (allocated(error)) return
        call check(error, trim(files(1)), "a.in")
        if (allocated(error)) return
        call result%namespace%get("output", output, stat)
        call check(error, trim(output), "b.out")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=5) :: "b.out"])
        call result%namespace%get("files", files, stat)
        call check(error, size(files), 0)
        if (allocated(error)) return
        call result%namespace%get("output", output, stat)
        call check(error, trim(output), "b.out")
        if (allocated(error)) return

        call optional_parser%init(add_help=.false.)
        call optional_parser%add_argument("maybe", nargs="?")
        call optional_parser%add_argument("required")
        result = optional_parser%parse_tokens([character(len=5) :: "value"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%namespace%contains("maybe"), .false.)
        if (allocated(error)) return
        call result%namespace%get("required", output, stat)
        call check(error, trim(output), "value")
        if (allocated(error)) return

        result = optional_parser%parse_tokens([character(len=6) :: &
            "maybe", "needed"])
        call result%namespace%get("maybe", output, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(output), "maybe")
        if (allocated(error)) return
        call result%namespace%get("required", output, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(output), "needed")
        if (allocated(error)) return

        call plus_parser%init(add_help=.false.)
        call plus_parser%add_argument("inputs", nargs="+")
        call plus_parser%add_argument("output")
        result = plus_parser%parse_tokens([character(len=5) :: &
            "a.in", "b.in", "c.out"])
        call result%namespace%get("inputs", files, stat)
        call check(error, size(files), 2)
        if (allocated(error)) return
        call result%namespace%get("output", output, stat)
        call check(error, trim(output), "c.out")
    end subroutine test_variable_positionals

    subroutine test_remainder(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: rest(:)
        integer :: stat

        call parser%init(add_help=.false.)
        allocate(character(len=1) :: rest(0))
        call parser%add_argument("head")
        call parser%add_argument("rest", nargs="remainder")
        result = parser%parse_tokens([character(len=7) :: &
            "start", "first", "--flag", "--", "tail"])

        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("rest", rest, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(rest), 4)
        if (allocated(error)) return
        call check(error, trim(rest(2)), "--flag")
        if (allocated(error)) return
        call check(error, trim(rest(3)), "--")
        if (allocated(error)) return
        call check(error, trim(rest(4)), "tail")
    end subroutine test_remainder

    subroutine test_nargs_failures(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: exact_parser, plus_parser, positional_parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call exact_parser%init(add_help=.false.)
        call exact_parser%add_argument("--pair", nargs=2, data_type="integer")
        result = exact_parser%parse_tokens([character(len=6) :: "--pair", "1"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return
        call check(error, result%namespace%contains("pair"), .false.)
        if (allocated(error)) return

        call plus_parser%init(add_help=.false.)
        call plus_parser%add_argument("--items", nargs="+")
        result = plus_parser%parse_tokens([character(len=7) :: "--items"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return

        call positional_parser%init(add_help=.false.)
        call positional_parser%add_argument("coords", nargs=2, data_type="real")
        result = positional_parser%parse_tokens([character(len=3) :: "1.0"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 2)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return
        call check(error, result%namespace%contains("coords"), .false.)
    end subroutine test_nargs_failures

end module test_parse_nargs
