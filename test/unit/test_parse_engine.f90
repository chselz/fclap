module test_parse_engine
    use fclap, only : ArgumentParser, ErrorEntry, ParseResult, ip, wp, &
        FCLAP_OK, PARSE_SUCCESS, PARSE_FAILURE, ERR_DUPLICATE_OPTION, &
        ERR_UNKNOWN_ARGUMENT, ERR_MISSING_VALUE, ERR_EXTRA_POSITIONAL, &
        ERR_MISSING_REQUIRED
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_parse_engine

contains

    subroutine collect_parse_engine(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("parse positional option and alias", test_scalar_parse), &
            new_unittest("defaults and independent results", &
                test_defaults_and_independent_results), &
            new_unittest("refuse invalid parser", test_invalid_parser), &
            new_unittest("missing required arguments", test_missing_required), &
            new_unittest("unknown option", test_unknown_option), &
            new_unittest("missing option value", test_missing_option_value), &
            new_unittest("extra positional", test_extra_positional), &
            new_unittest("end of options marker", test_end_of_options), &
            new_unittest("negative numeric values", test_negative_values) &
        ]
    end subroutine collect_parse_engine

    subroutine test_scalar_parse(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=32) :: input, output
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("input")
        call parser%add_argument("-o", "--output")

        result = parser%parse_tokens([character(len=12) :: &
            "input.dat", "-o", "custom.dat"])

        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%succeeded(), .true.)
        if (allocated(error)) return
        call check(error, result%errors%count(), 0)
        if (allocated(error)) return
        call result%namespace%get("input", input, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(input), "input.dat")
        if (allocated(error)) return
        call result%namespace%get("output", output, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(output), "custom.dat")
    end subroutine test_scalar_parse

    subroutine test_defaults_and_independent_results(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: first, second
        character(len=32) :: value
        character(len=1), allocatable :: no_tokens(:)
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--output", default="result.dat")
        allocate(no_tokens(0))

        first = parser%parse_tokens(no_tokens)
        second = parser%parse_tokens([character(len=11) :: &
            "--output", "changed.dat"])

        call check(error, first%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call first%namespace%get("output", value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(value), "result.dat")
        if (allocated(error)) return
        call second%namespace%get("output", value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(value), "changed.dat")
        if (allocated(error)) return

        ! A later parse must not modify an earlier self-contained result.
        call first%namespace%get("output", value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(value), "result.dat")
    end subroutine test_defaults_and_independent_results

    subroutine test_invalid_parser(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(add_help=.false.)
        call parser%add_argument("--value", default="before")
        call parser%add_argument("--value")

        result = parser%parse_tokens([character(len=9) :: "--value", "after"])

        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_DUPLICATE_OPTION)
        if (allocated(error)) return
        call check(error, result%namespace%size(), 0)
    end subroutine test_invalid_parser

    subroutine test_missing_required(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        character(len=16) :: value
        character(len=1), allocatable :: no_tokens(:)
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("input")
        call parser%add_argument("--mode", required=.true., default="safe")
        allocate(no_tokens(0))

        result = parser%parse_tokens(no_tokens)

        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%failed(), .true.)
        if (allocated(error)) return
        call check(error, result%errors%count(), 2)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_REQUIRED)
        if (allocated(error)) return
        call check(error, entry%arg_name, "input")
        if (allocated(error)) return
        entry = result%errors%get(2)
        call check(error, entry%code, ERR_MISSING_REQUIRED)
        if (allocated(error)) return
        call check(error, entry%arg_name, "mode")
        if (allocated(error)) return

        ! A default is inserted but does not satisfy explicit required state.
        call result%namespace%get("mode", value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(value), "safe")
    end subroutine test_missing_required

    subroutine test_unknown_option(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(add_help=.false.)
        result = parser%parse_tokens([character(len=9) :: "--unknown"])

        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_ARGUMENT)
        if (allocated(error)) return
        call check(error, entry%arg_name, "--unknown")
        if (allocated(error)) return
        call check(error, entry%flag, "--unknown")
        if (allocated(error)) return
        call check(error, entry%flag_index, 1)
    end subroutine test_unknown_option

    subroutine test_missing_option_value(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(add_help=.false.)
        call parser%add_argument("-o", "--output")
        result = parser%parse_tokens([character(len=2) :: "-o"])

        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return
        call check(error, entry%arg_name, "output")
        if (allocated(error)) return
        call check(error, entry%flag, "-o")
        if (allocated(error)) return
        call check(error, entry%flag_index, 1)
        if (allocated(error)) return
        call check(error, result%namespace%contains("output"), .false.)
    end subroutine test_missing_option_value

    subroutine test_extra_positional(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        character(len=16) :: input
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("input")
        result = parser%parse_tokens([character(len=7) :: "one.dat", "two.dat"])

        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_EXTRA_POSITIONAL)
        if (allocated(error)) return
        call check(error, entry%flag, "two.dat")
        if (allocated(error)) return
        call check(error, entry%flag_index, 2)
        if (allocated(error)) return

        ! Values accepted before a later fatal token remain available.
        call result%namespace%get("input", input, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(input), "one.dat")
    end subroutine test_extra_positional

    subroutine test_end_of_options(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=16) :: value
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("value")
        result = parser%parse_tokens([character(len=9) :: "--", "--literal"])

        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("value", value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(value), "--literal")
    end subroutine test_end_of_options

    subroutine test_negative_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: option_parser, positional_parser
        type(ParseResult) :: result
        integer(ip) :: level
        real(wp) :: offset
        integer :: stat

        call option_parser%init(add_help=.false.)
        call option_parser%add_argument("--offset", data_type="real")
        result = option_parser%parse_tokens([character(len=8) :: &
            "--offset", "-2.5"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("offset", offset, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, offset, -2.5_wp)
        if (allocated(error)) return

        call positional_parser%init(add_help=.false.)
        call positional_parser%add_argument("level", data_type="integer")
        result = positional_parser%parse_tokens([character(len=2) :: "-7"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("level", level, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, level, -7_ip)
    end subroutine test_negative_values

end module test_parse_engine
