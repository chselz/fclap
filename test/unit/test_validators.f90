module test_validators
    use, intrinsic :: iso_fortran_env, only : wp => real64
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, ParseResult, &
        store_true, not_less_than, not_bigger_than, PARSE_SUCCESS, &
        PARSE_FAILURE, ERR_INVALID_DEFAULT, ERR_INVALID_CHOICE, &
        ERR_VALIDATION_FAILED, ERR_DEPRECATED_ARGUMENT, ERR_REMOVED_ARGUMENT, &
        ERROR_WARNING, ERR_INCOMPATIBLE_ACTION, FCLAP_OK
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
            new_unittest("case-insensitive choices", &
                test_case_insensitive_choices), &
            new_unittest("case-insensitive choices configuration", &
                test_case_insensitive_config), &
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
        call check(error, entry%token, "0")
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
        call parser%add_argument("--bad", data_type="integer", default=0, &
            validator=not_less_than(1))
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
            choices=[1, 2])
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

    subroutine test_case_insensitive_choices(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        character(len=:), allocatable :: closure
        character(len=:), allocatable :: models(:)
        integer :: stat

        call parser%init(add_help=.false.)
        call parser%add_argument("--closure", choices=[character(len=3) :: &
            "HNC", "KH", "PY"], default="hnc", case_insensitive_choices=.true.)
        call parser%add_argument("--models", nargs="+", &
            choices=[character(len=3) :: "HNC", "KH", "PY"], &
            case_insensitive_choices=.true.)
        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return

        ! A default is stored with the declared spelling.
        result = parser%parse_tokens([character(len=1) ::])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("closure", closure, stat)
        call check(error, closure, "HNC")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: &
            "--closure", "pY", "--models", "hnc", "Kh"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("closure", closure, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, closure, "PY")
        if (allocated(error)) return
        call result%namespace%get("models", models, stat)
        call check(error, size(models), 2)
        if (allocated(error)) return
        call check(error, trim(models(1)), "HNC")
        if (allocated(error)) return
        call check(error, trim(models(2)), "KH")
        if (allocated(error)) return

        ! A rejected token leaves the default in place.
        result = parser%parse_tokens([character(len=9) :: "--closure", "msa"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_CHOICE)
        if (allocated(error)) return
        call check(error, entry%token, "msa")
        if (allocated(error)) return
        call result%namespace%get("closure", closure, stat)
        call check(error, closure, "HNC")
        if (allocated(error)) return

        ! Without the option, matching stays exact.
        call parser%init(add_help=.false.)
        call parser%add_argument("--closure", choices=[character(len=3) :: &
            "HNC", "KH", "PY"])
        result = parser%parse_tokens([character(len=9) :: "--closure", "hnc"])
        call check_choice_failure(error, result, "closure", "hnc", 2)
    end subroutine test_case_insensitive_choices

    subroutine test_case_insensitive_config(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry

        call parser%init(add_help=.false.)
        call parser%add_argument("--level", data_type="integer", &
            choices=[1, 2], case_insensitive_choices=.true.)
        call parser%add_argument("--mode", case_insensitive_choices=.true.)
        call parser%add_argument("--unit", choices=[character(len=2) :: &
            "mm", "MM"], case_insensitive_choices=.true.)
        call parser%add_argument("--shape", choices=[character(len=4) :: &
            "cube", "ball"], default="cone", case_insensitive_choices=.true.)
        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return

        errors = parser%get_config_errors()
        call check(error, errors%count(), 4)
        if (allocated(error)) return
        entry = errors%get(1)
        call check(error, entry%code, ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return
        call check(error, entry%arg_name, "level")
        if (allocated(error)) return
        entry = errors%get(2)
        call check(error, entry%code, ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return
        call check(error, entry%arg_name, "mode")
        if (allocated(error)) return
        entry = errors%get(3)
        call check(error, entry%code, ERR_INVALID_DEFAULT)
        if (allocated(error)) return
        call check(error, entry%message, "choices differ only by letter case")
        if (allocated(error)) return
        entry = errors%get(4)
        call check(error, entry%code, ERR_INVALID_DEFAULT)
        if (allocated(error)) return
        call check(error, entry%arg_name, "shape")
    end subroutine test_case_insensitive_config

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
        call check(error, entry%token, "--gone")
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
        call check(error, entry%token, token)
        if (allocated(error)) return
        call check(error, entry%token_index, token_index)
        if (allocated(error)) return
        call check(error, result%namespace%contains(dest), .false.)
    end subroutine check_choice_failure

end module test_validators
