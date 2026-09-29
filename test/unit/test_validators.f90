module test_validators
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, ParseResult, &
        ip, wp, store_true, not_less_than, not_bigger_than, PARSE_SUCCESS, &
        PARSE_FAILURE, ERR_INVALID_DEFAULT, ERR_INVALID_CHOICE, &
        ERR_VALIDATION_FAILED, ERR_DEPRECATED_ARGUMENT, ERR_REMOVED_ARGUMENT, &
        ERROR_WARNING
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_validators

contains

    subroutine collect_validators(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("numeric bound validators", test_bound_validators), &
            new_unittest("runtime choices by type", test_runtime_choices), &
            new_unittest("deprecated and removed arguments", test_lifecycle) &
        ]
    end subroutine collect_validators

    subroutine test_bound_validators(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        type(ErrorStack) :: errors

        call parser%init(add_help=.false.)
        call parser%add_argument("--threads", data_type="integer", &
            validator=not_less_than(1))
        call parser%add_argument("--ratio", data_type="real", &
            validator=not_bigger_than(1.0))
        result = parser%parse_tokens([character(len=9) :: &
            "--threads", "2", "--ratio", "0.5"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: "--threads", "0"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_VALIDATION_FAILED)
        if (allocated(error)) return
        call check(error, entry%arg_name, "threads")
        if (allocated(error)) return
        call check(error, entry%flag, "0")
        if (allocated(error)) return
        call check(error, result%namespace%contains("threads"), .false.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=7) :: "--ratio", "1.5"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_VALIDATION_FAILED)
        if (allocated(error)) return
        call check(error, result%namespace%contains("ratio"), .false.)
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--bad", data_type="integer", default=0_ip, &
            validator=not_less_than(1_ip))
        errors = parser%get_config_errors()
        call check(error, parser%is_valid(), .false.)
        if (allocated(error)) return
        entry = errors%get(1)
        call check(error, entry%code, ERR_INVALID_DEFAULT)
    end subroutine test_bound_validators

    subroutine test_runtime_choices(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result

        call parser%init(add_help=.false.)
        call parser%add_argument("--value", choices=[character(len=4) :: &
            "fast", "slow"])
        result = parser%parse_tokens([character(len=7) :: "--value", "other"])
        call check_choice_failure(error, result, "value", "other", 2)
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--value", data_type="integer", nargs=2, &
            choices=[1_ip, 2_ip])
        result = parser%parse_tokens([character(len=7) :: &
            "--value", "1", "3"])
        call check_choice_failure(error, result, "value", "3", 3)
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--value", data_type="real", &
            choices=[1.0_wp, 2.0_wp])
        result = parser%parse_tokens([character(len=7) :: "--value", "3.0"])
        call check_choice_failure(error, result, "value", "3.0", 2)
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--value", data_type="logical", &
            choices=[.true.])
        result = parser%parse_tokens([character(len=7) :: "--value", "false"])
        call check_choice_failure(error, result, "value", "false", 2)
    end subroutine test_runtime_choices

    subroutine test_lifecycle(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        logical :: old
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--old", action=store_true(), &
            deprecated_msg="use --new instead")
        call parser%add_argument("--gone", removed_msg="option was removed")

        result = parser%parse_tokens([character(len=5) :: "--old"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_DEPRECATED_ARGUMENT)
        if (allocated(error)) return
        call check(error, entry%severity, ERROR_WARNING)
        if (allocated(error)) return
        call result%namespace%get("old", old, stat)
        call check(error, old, .true.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=7) :: "--gone", "value"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_REMOVED_ARGUMENT)
        if (allocated(error)) return
        call check(error, entry%flag, "--gone")
        if (allocated(error)) return
        call check(error, result%namespace%contains("gone"), .false.)
    end subroutine test_lifecycle

    subroutine check_choice_failure(error, result, dest, token, token_index)
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
        call check(error, entry%code, ERR_INVALID_CHOICE)
        if (allocated(error)) return
        call check(error, entry%arg_name, dest)
        if (allocated(error)) return
        call check(error, entry%flag, token)
        if (allocated(error)) return
        call check(error, entry%flag_index, token_index)
        if (allocated(error)) return
        call check(error, result%namespace%contains(dest), .false.)
    end subroutine check_choice_failure

end module test_validators
