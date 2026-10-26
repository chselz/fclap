module test_conversion
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_positive_inf, &
        ieee_is_finite, ieee_is_nan
    use, intrinsic :: iso_fortran_env, only : int64, real32, wp => real64
    use fclap, only : ArgumentParser, ErrorEntry, Namespace, ParseResult, &
        FCLAP_OK, PARSE_SUCCESS, PARSE_FAILURE, ERR_INVALID_VALUE, &
        ERR_NAMESPACE_OUT_OF_RANGE, ERR_NAMESPACE_TYPE_MISMATCH, &
        ERR_VALIDATION_FAILED, store_const, not_less_than, not_bigger_than, &
        in_range
    use testdrive, only : new_unittest, unittest_type, error_type, check, &
        test_failed
    implicit none
    private

    public :: collect_conversion

contains

    subroutine collect_conversion(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("strict integer tokens", test_strict_integers), &
            new_unittest("strict real tokens", test_strict_reals), &
            new_unittest("accepted numeric spellings", test_numeric_spellings), &
            new_unittest("64-bit integer values", test_integer_i8), &
            new_unittest("narrowing retrieval is range checked", &
                test_narrowing_retrieval), &
            new_unittest("string retrieval keeps the exact length", &
                test_string_exact_length), &
            new_unittest("kind variant round trips", test_kind_round_trips), &
            new_unittest("kind-generic constants and validators", &
                test_kind_generic_factories), &
            new_unittest("every getter reports a type mismatch", &
                test_getter_type_mismatch), &
            new_unittest("real tokens beyond the range are rejected", &
                test_real_overflow_tokens), &
            new_unittest("narrowing to real32 rejects underflow", &
                test_narrowing_underflow), &
            new_unittest("real32 defaults keep their decimal value", &
                test_single_precision_defaults) &
        ]
    end subroutine collect_conversion

    subroutine test_strict_integers(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=8), parameter :: rejected(*) = [character(len=8) :: &
            ",", "2*7", "4 foo", "3,9", "5/", "", "1.0", "+", "0x10", "1e3"]
        integer :: index

        do index = 1, size(rejected)
            call check_rejected(error, "integer", rejected(index))
            if (allocated(error)) return
        end do
    end subroutine test_strict_integers

    subroutine test_strict_reals(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=8), parameter :: rejected(*) = [character(len=8) :: &
            ",", "1.0,2", "2*1.5", "1..2", "e5", "1e", "1.5q3", ".", "-", &
            "1.0/", "nanx"]
        integer :: index

        do index = 1, size(rejected)
            call check_rejected(error, "real", rejected(index))
            if (allocated(error)) return
        end do
    end subroutine test_strict_reals

    subroutine test_numeric_spellings(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        real(wp) :: values(5)
        integer :: count
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--a", data_type="real")
        call parser%add_argument("--b", data_type="real")
        call parser%add_argument("--c", data_type="real")
        call parser%add_argument("--d", data_type="real")
        call parser%add_argument("--e", data_type="real")
        call parser%add_argument("--n", data_type="integer")
        result = parser%parse_tokens([character(len=8) :: &
            "--a", "1.", "--b", ".5", "--c", "-1.5e-3", "--d", "2D3", &
            "--e", "+inf", "--n", "+42"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return

        call result%namespace%get("a", values(1), stat)
        call result%namespace%get("b", values(2), stat)
        call result%namespace%get("c", values(3), stat)
        call result%namespace%get("d", values(4), stat)
        call result%namespace%get("e", values(5), stat)
        call check(error, values(1), 1.0_wp)
        if (allocated(error)) return
        call check(error, values(2), 0.5_wp)
        if (allocated(error)) return
        call check(error, values(3), -1.5e-3_wp)
        if (allocated(error)) return
        call check(error, values(4), 2000.0_wp)
        if (allocated(error)) return
        call check(error, values(5) > huge(values(5)), .true.)
        if (allocated(error)) return
        call result%namespace%get("n", count, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, count, 42)
    end subroutine test_numeric_spellings

    subroutine test_integer_i8(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        integer(int64) :: seed
        integer(int64), allocatable :: sizes(:)
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--seed", data_type="integer", &
            default=1_int64)
        call parser%add_argument("--sizes", data_type="integer", nargs="+")
        result = parser%parse_tokens([character(len=20) :: &
            "--seed", "9000000000", "--sizes", "3", "-9223372036854775807"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("seed", seed, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, seed, 9000000000_int64)
        if (allocated(error)) return
        call result%namespace%get("sizes", sizes, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(sizes), 2)
        if (allocated(error)) return
        call check(error, sizes(2), -9223372036854775807_int64)
        if (allocated(error)) return

        ! Values beyond the 64-bit range are conversion failures, not wraps.
        result = parser%parse_tokens([character(len=20) :: &
            "--seed", "99999999999999999999"])
        call check(error, result%outcome, PARSE_FAILURE)
    end subroutine test_integer_i8

    subroutine test_narrowing_retrieval(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        integer :: narrow
        integer, allocatable :: narrow_list(:)
        real(real32) :: single
        integer :: stat

        call args%set("big", 9000000000_int64)
        call args%set("bigs", [1_int64, 9000000000_int64])
        call args%set("small", 7_int64)
        call args%set("huge", 1.0e300_wp)
        call args%set("ratio", 0.25_wp)

        narrow = -1
        call args%get("big", narrow, stat)
        call check(error, stat, ERR_NAMESPACE_OUT_OF_RANGE)
        if (allocated(error)) return
        call check(error, narrow, -1)
        if (allocated(error)) return
        call args%get("bigs", narrow_list, stat)
        call check(error, stat, ERR_NAMESPACE_OUT_OF_RANGE)
        if (allocated(error)) return
        call args%get("small", narrow, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, narrow, 7)
        if (allocated(error)) return

        call args%get("huge", single, stat)
        call check(error, stat, ERR_NAMESPACE_OUT_OF_RANGE)
        if (allocated(error)) return
        call args%get("ratio", single, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, single, 0.25_real32)
    end subroutine test_narrowing_retrieval

    subroutine test_string_exact_length(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        character(len=:), allocatable :: path
        integer :: stat

        call args%set("path", "/very/long/path/file ")
        call args%get("path", path, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, len(path), 21)
        if (allocated(error)) return

        ! A second retrieval reallocates to the new length.
        call args%set("path", "/tmp")
        call args%get("path", path, stat)
        call check(error, path, "/tmp")
    end subroutine test_string_exact_length

    subroutine test_kind_round_trips(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        real(real32) :: single
        real(real32), allocatable :: singles(:)
        real(wp), allocatable :: doubles(:)
        integer(int64), allocatable :: longs(:)
        integer, allocatable :: ints(:)
        integer :: stat

        call args%set("ratio", 0.5_real32)
        call args%set("ratios", [0.25_real32, 0.75_real32])
        call args%append("ratios", 1.0_real32, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call args%get("ratio", single, stat)
        call check(error, single, 0.5_real32)
        if (allocated(error)) return
        call args%get("ratios", singles, stat)
        call check(error, size(singles), 3)
        if (allocated(error)) return
        call check(error, singles(3), 1.0_real32)
        if (allocated(error)) return
        ! Single-precision input is stored at working precision.
        call args%get("ratios", doubles, stat)
        call check(error, doubles(1), 0.25_wp)
        if (allocated(error)) return

        call args%set("big", [1.0e300_wp])
        call args%get("big", singles, stat)
        call check(error, stat, ERR_NAMESPACE_OUT_OF_RANGE)
        if (allocated(error)) return

        call args%set("sizes", [1_int64, 2_int64])
        call args%append("sizes", 3_int64, stat)
        call args%get("sizes", longs, stat)
        call check(error, size(longs), 3)
        if (allocated(error)) return
        call check(error, longs(3), 3_int64)
        if (allocated(error)) return
        call args%get("sizes", ints, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, ints(2), 2)
    end subroutine test_kind_round_trips

    subroutine test_kind_generic_factories(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        integer(int64) :: level
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--max", action=store_const(9000000000_int64), &
            dest="level")
        call parser%add_argument("--seed", data_type="integer", &
            validator=in_range(0_int64, 5000000000_int64))
        call parser%add_argument("--floor", data_type="integer", &
            validator=not_less_than(-1_int64))
        call parser%add_argument("--ceiling", data_type="integer", &
            validator=not_bigger_than(10_int64))
        call parser%add_argument("--ratio", data_type="real", &
            validator=in_range(0.0_real32, 1.0_real32))
        call parser%add_argument("--scale", data_type="real", &
            validator=not_bigger_than(2.0_real32))
        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=10) :: "--max", &
            "--seed", "4000000000", "--floor", "-1", "--ceiling", "10", &
            "--ratio", "0.5", "--scale", "2"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("level", level, stat)
        call check(error, level, 9000000000_int64)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=10) :: &
            "--ratio", "1.5"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_VALIDATION_FAILED)
        if (allocated(error)) return
        call check(error, entry%message, "value must be between 0.0 and 1.0")
    end subroutine test_kind_generic_factories

    subroutine test_getter_type_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: text
        character(len=:), allocatable :: texts(:)
        integer :: number
        integer(int64) :: long
        integer, allocatable :: numbers(:)
        integer(int64), allocatable :: longs(:)
        real(wp) :: real_value
        real(real32) :: single
        real(wp), allocatable :: reals(:)
        real(real32), allocatable :: singles(:)
        logical :: flag
        logical, allocatable :: flags(:)
        integer :: stat(12)

        call args%set("text", "abc")
        call args%set("numbers", [1, 2])
        call args%set("texts", [character(len=1) :: "a", "b"])
        call args%append("reals", 1.0_wp)
        call args%append("flags", .true.)

        call args%get("numbers", text, stat(1))
        call args%get("text", number, stat(2))
        call args%get("text", long, stat(3))
        call args%get("text", real_value, stat(4))
        call args%get("text", single, stat(5))
        call args%get("text", flag, stat(6))
        call args%get("numbers", texts, stat(7))
        call args%get("texts", numbers, stat(8))
        call args%get("texts", longs, stat(9))
        call args%get("numbers", reals, stat(10))
        call args%get("texts", singles, stat(11))
        call args%get("numbers", flags, stat(12))
        call check(error, all(stat == ERR_NAMESPACE_TYPE_MISMATCH), .true.)
        if (allocated(error)) then
            print "(12(i0,1x))", stat
            return
        end if

        ! Non-finite values narrow to single precision.
        call args%set("inf", ieee_value(1.0_wp, ieee_positive_inf))
        call args%get("inf", single, stat(1))
        call check(error, stat(1), FCLAP_OK)
        if (allocated(error)) return
        call check(error, single > huge(single), .true.)
        if (allocated(error)) return

        ! Remaining bound factories accept and reject consistently.
        call parser%init(add_help=.false.)
        call parser%add_argument("--a", data_type="real", &
            validator=not_less_than(1.0_real32))
        call parser%add_argument("--b", data_type="integer", &
            validator=not_bigger_than(3))
        call parser%add_argument("--c", data_type="real", &
            validator=not_bigger_than(3.0_wp))
        result = parser%parse_tokens([character(len=3) :: &
            "--a", "1", "--b", "3", "--c", "3"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=3) :: "--b", "4"])
        call check(error, result%outcome, PARSE_FAILURE)
    end subroutine test_getter_type_mismatch

    subroutine test_real_overflow_tokens(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=8), parameter :: rejected(*) = [character(len=8) :: &
            "1e400", "1d+400", "+1E400", "1e999999"]
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        real(wp) :: values(4)
        integer :: index, stat

        do index = 1, size(rejected)
            call check_rejected(error, "real", rejected(index))
            if (allocated(error)) return
        end do
        call check_rejected(error, "real", "1" // repeat("9", 400))
        if (allocated(error)) return
        call check_rejected(error, "real", "1.79769313486232e308")
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--a", data_type="real")
        ! The attached form keeps `-1e400` a value rather than an option.
        result = parser%parse_tokens([character(len=10) :: "--a=-1e400"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return

        ! Spelled special values and underflow to zero stay accepted.
        call parser%add_argument("--b", data_type="real")
        call parser%add_argument("--c", data_type="real")
        call parser%add_argument("--d", data_type="real")
        result = parser%parse_tokens([character(len=9) :: "--a", "-INF", &
            "--b", "+Infinity", "--c", "NaN", "--d", "1e-400"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("a", values(1), stat)
        call result%namespace%get("b", values(2), stat)
        call result%namespace%get("c", values(3), stat)
        call result%namespace%get("d", values(4), stat)
        call check(error, .not. ieee_is_finite(values(1)) .and. &
            values(1) < 0.0_wp, .true.)
        if (allocated(error)) return
        call check(error, .not. ieee_is_finite(values(2)) .and. &
            values(2) > 0.0_wp, .true.)
        if (allocated(error)) return
        call check(error, ieee_is_nan(values(3)), .true.)
        if (allocated(error)) return
        call check(error, values(4) == 0.0_wp, .true.)
    end subroutine test_real_overflow_tokens

    subroutine test_narrowing_underflow(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        real(real32) :: single
        real(real32), allocatable :: singles(:)
        integer :: stat

        call args%set("tiny", 1.0e-50_wp)
        call args%set("negative", -1.0e-50_wp)
        call args%set("tinies", [1.0_wp, 1.0e-50_wp])
        call args%set("subnormal", 1.0e-40_wp)
        call args%set("zero", -0.0_wp)

        single = -1.0_real32
        call args%get("tiny", single, stat)
        call check(error, stat, ERR_NAMESPACE_OUT_OF_RANGE)
        if (allocated(error)) return
        call check(error, single, -1.0_real32)
        if (allocated(error)) return
        call args%get("negative", single, stat)
        call check(error, stat, ERR_NAMESPACE_OUT_OF_RANGE)
        if (allocated(error)) return
        call args%get("tinies", singles, stat)
        call check(error, stat, ERR_NAMESPACE_OUT_OF_RANGE)
        if (allocated(error)) return
        call check(error, allocated(singles), .false.)
        if (allocated(error)) return

        ! A value that stays nonzero as a subnormal real32 still fits.
        call args%get("subnormal", single, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, single > 0.0_real32, .true.)
        if (allocated(error)) return
        call args%get("zero", single, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, single == 0.0_real32, .true.)
    end subroutine test_narrowing_underflow

    subroutine test_single_precision_defaults(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: help
        real(wp) :: ratio, scale
        integer :: stat

        call parser%init(prog="t", add_help=.false.)
        call parser%add_argument("--ratio", data_type="real", default=0.1_real32)
        call parser%add_argument("--scale", data_type="real", nargs="?", &
            const=0.3_real32, choices=[0.2_real32, 0.3_real32])
        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        help = parser%format_help()
        call check(error, index(help, "(default: 0.1)") > 0, .true.)
        if (allocated(error)) then
            print "(a)", help
            return
        end if

        ! The decimal token must match the real32 choices.
        result = parser%parse_tokens([character(len=7) :: "--scale", "0.2"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("ratio", ratio, stat)
        call check(error, ratio == 0.1_wp, .true.)
        if (allocated(error)) return
        call result%namespace%get("scale", scale, stat)
        call check(error, scale == 0.2_wp, .true.)
        if (allocated(error)) return
        result = parser%parse_tokens([character(len=7) :: "--scale"])
        call result%namespace%get("scale", scale, stat)
        call check(error, scale == 0.3_wp, .true.)
    end subroutine test_single_precision_defaults

    subroutine check_rejected(error, type_name, token)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: type_name, token
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        character(len=:), allocatable :: tokens(:)

        allocate(character(len=max(8, len(token))) :: tokens(2))
        tokens(1) = "--value"
        tokens(2) = token
        call parser%init(add_help=.false.)
        call parser%add_argument("--value", data_type=type_name)
        result = parser%parse_tokens(tokens)
        if (result%outcome /= PARSE_FAILURE) then
            call test_failed(error, type_name // " token '" // trim(token) // &
                "' was accepted")
            return
        end if
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, result%namespace%contains("value"), .false.)
    end subroutine check_rejected

end module test_conversion
