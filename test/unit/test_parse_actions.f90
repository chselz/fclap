module test_parse_actions
    use fclap, only : ArgumentParser, ErrorEntry, ParseResult, ip, wp, &
        append, count, store_const, store_true, store_false, FCLAP_OK, &
        PARSE_SUCCESS, PARSE_FAILURE, ERR_INVALID_VALUE
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_parse_actions

contains

    subroutine collect_parse_actions(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("typed scalar conversion", test_scalar_conversion), &
            new_unittest("repeated built-in actions", test_repeated_actions), &
            new_unittest("conversion failures are atomic", &
                test_conversion_failures) &
        ]
    end subroutine collect_parse_actions

    subroutine test_scalar_conversion(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        integer(ip) :: integer_value
        real(wp) :: real_value
        logical :: logical_value
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--integer", data_type="integer")
        call parser%add_argument("--real", data_type="real")
        call parser%add_argument("--logical", data_type="logical")
        result = parser%parse_tokens([character(len=9) :: &
            "--integer", "01", "--real", "1.25", "--logical", "YES"])

        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("integer", integer_value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, integer_value, 1_ip)
        if (allocated(error)) return
        call result%namespace%get("real", real_value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, real_value, 1.25_wp)
        if (allocated(error)) return
        call result%namespace%get("logical", logical_value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, logical_value, .true.)
    end subroutine test_scalar_conversion

    subroutine test_repeated_actions(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=16) :: value, mode
        character(len=:), allocatable :: tags(:), empty(:)
        integer(ip) :: occurrences
        logical :: verbose, quiet
        integer :: stat

        call parser%init(add_help=.false.)
        allocate(character(len=1) :: tags(0), empty(0))
        call parser%add_argument("--value")
        call parser%add_argument("--mode", action=store_const("fast"))
        call parser%add_argument("--verbose", action=store_true())
        call parser%add_argument("--quiet", action=store_false())
        call parser%add_argument("--count", action=count())
        call parser%add_argument("--tag", action=append(), nargs="+")
        call parser%add_argument("--empty", action=append(), nargs="*")
        result = parser%parse_tokens([character(len=9) :: &
            "--value", "first", "--value", "second", "--mode", &
            "--verbose", "--quiet", "--count", "--count", &
            "--tag", "a", "b", "--tag", "c", "--empty"])

        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("value", value, stat)
        call check(error, trim(value), "second")
        if (allocated(error)) return
        call result%namespace%get("mode", mode, stat)
        call check(error, trim(mode), "fast")
        if (allocated(error)) return
        call result%namespace%get("verbose", verbose, stat)
        call check(error, verbose, .true.)
        if (allocated(error)) return
        call result%namespace%get("quiet", quiet, stat)
        call check(error, quiet, .false.)
        if (allocated(error)) return
        call result%namespace%get("count", occurrences, stat)
        call check(error, occurrences, 2_ip)
        if (allocated(error)) return
        call result%namespace%get("tag", tags, stat)
        call check(error, size(tags), 3)
        if (allocated(error)) return
        call check(error, trim(tags(2)), "b")
        if (allocated(error)) return
        call check(error, trim(tags(3)), "c")
        if (allocated(error)) return
        call result%namespace%get("empty", empty, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(empty), 0)
    end subroutine test_repeated_actions

    subroutine test_conversion_failures(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(add_help=.false.)
        call parser%add_argument("--value", data_type="integer")
        result = parser%parse_tokens([character(len=7) :: "--value", "wrong"])
        call check_invalid_conversion(error, result, "value", "wrong", 2)
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--value", data_type="real")
        result = parser%parse_tokens([character(len=7) :: "--value", "wrong"])
        call check_invalid_conversion(error, result, "value", "wrong", 2)
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--value", data_type="logical")
        result = parser%parse_tokens([character(len=7) :: "--value", "maybe"])
        call check_invalid_conversion(error, result, "value", "maybe", 2)
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--values", nargs=2, data_type="integer")
        result = parser%parse_tokens([character(len=8) :: &
            "--values", "1", "wrong"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, entry%flag, "wrong")
        if (allocated(error)) return
        call check(error, entry%flag_index, 3)
        if (allocated(error)) return
        call check(error, result%namespace%contains("values"), .false.)
    end subroutine test_conversion_failures

    subroutine check_invalid_conversion(error, result, dest, token, token_index)
        type(error_type), allocatable, intent(out) :: error
        type(ParseResult), intent(in) :: result
        character(len=*), intent(in) :: dest, token
        integer, intent(in) :: token_index
        type(ErrorEntry) :: entry

        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, entry%arg_name, dest)
        if (allocated(error)) return
        call check(error, entry%flag, token)
        if (allocated(error)) return
        call check(error, entry%flag_index, token_index)
        if (allocated(error)) return
        call check(error, result%namespace%contains(dest), .false.)
    end subroutine check_invalid_conversion

end module test_parse_actions
