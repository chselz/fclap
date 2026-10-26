module test_registration
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, ValidatorType, ValueBox, &
        IntegerValue, LogicalValue, ListValue, new_value, store, store_true, &
        store_false, store_const, append, count, boolean_optional, in_range, &
        ERR_INVALID_ARGUMENT_NAME, &
        ERR_DUPLICATE_OPTION, &
        ERR_DUPLICATE_DEST, ERR_INVALID_NARGS, ERR_INCOMPATIBLE_ACTION, &
        ERR_INVALID_DEFAULT, ERR_INVALID_TYPE_NAME, ERR_INVALID_LIFECYCLE
    use fclap_actions_builtin, only : HelpAction, VersionAction, &
        StoreAction, StoreTrueAction, StoreFalseAction, CountAction, &
        AppendAction, StoreConstAction
    use fclap_argument, only : Argument
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_registration

    type, extends(ValidatorType) :: NonnegativeValidator
    contains
        procedure :: validate => validate_nonnegative
    end type NonnegativeValidator

    !> Rejects every value without allocating a message
    type, extends(ValidatorType) :: SilentValidator
    contains
        procedure :: validate => validate_silently
    end type SilentValidator

contains

    subroutine collect_registration(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("automatic actions and reinit", test_automatic_actions), &
            new_unittest("normalized metadata", test_normalized_metadata), &
            new_unittest("positional and optional arity", test_arity_metadata), &
            new_unittest("built-in action metadata", test_builtin_actions), &
            new_unittest("dynamic argument growth", test_dynamic_growth), &
            new_unittest("reject names transactionally", test_reject_names), &
            new_unittest("reject incompatible definitions", &
                test_reject_incompatible_definitions), &
            new_unittest("defaults choices and validators", &
                test_defaults_choices_validators), &
            new_unittest("silent validator rejects default", &
                test_silent_validator), &
            new_unittest("reject unmatchable names", test_reject_name_syntax), &
            new_unittest("reject zero-value positionals", &
                test_reject_zero_value_positionals), &
            new_unittest("reject optional positional needing a value", &
                test_reject_unrequired_positional), &
            new_unittest("reject empty, repeated, and invalid choices", &
                test_reject_bad_choices), &
            new_unittest("collisions with built-in arguments", &
                test_builtin_collisions) &
        ]
    end subroutine collect_registration

    subroutine test_automatic_actions(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(Argument) :: definition

        call parser%init(prog="demo", version="demo 1.0")
        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        call check(error, parser%argument_count(), 2)
        if (allocated(error)) return
        call check(error, parser%has_option("-h"), .true.)
        if (allocated(error)) return
        call check(error, parser%has_option("--help"), .true.)
        if (allocated(error)) return
        call check(error, parser%has_option("--version"), .true.)
        if (allocated(error)) return

        definition = parser%get_argument(1)
        select type (action => definition%action)
        type is (HelpAction)
            call check(error, .true.)
        class default
            call check(error, .false., "expected HelpAction")
        end select
        if (allocated(error)) return
        definition = parser%get_argument(2)
        select type (action => definition%action)
        type is (VersionAction)
            call check(error, .true.)
        class default
            call check(error, .false., "expected VersionAction")
        end select
        if (allocated(error)) return

        call parser%init(prog="demo", add_help=.false.)
        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        call check(error, parser%is_valid(), .true.)
    end subroutine test_automatic_actions

    subroutine test_normalized_metadata(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(Argument) :: definition

        call parser%init(add_help=.false.)
        call parser%add_argument("-q", "--dry-run", "--dry", &
            data_type="BOOL", default="yes", metavar="MODE", &
            help="Do not write output", visible=.false.)

        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        call check(error, parser%argument_count(), 1)
        if (allocated(error)) return

        definition = parser%get_argument(1)
        call check(error, definition%name_count(), 3)
        if (allocated(error)) return
        call check(error, definition%primary_name(), "--dry-run")
        if (allocated(error)) return
        call check(error, definition%dest, "dry_run")
        if (allocated(error)) return
        call check(error, definition%data_type, "logical")
        if (allocated(error)) return
        call check(error, definition%effective_metavar(), "MODE")
        if (allocated(error)) return
        call check(error, definition%required, .false.)
        if (allocated(error)) return
        call check(error, definition%visible, .false.)
        if (allocated(error)) return
        call check(error, definition%has_default, .true.)
        if (allocated(error)) return
        select type (stored => definition%default_value%item)
        type is (LogicalValue)
            call check(error, stored%value, .true.)
        class default
            call check(error, .false., "expected logical default")
        end select
    end subroutine test_normalized_metadata

    subroutine test_arity_metadata(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, positional_parser
        type(Argument) :: definition

        call parser%init(add_help=.false.)
        call parser%add_argument("files", nargs="+", &
            default=[character(len=5) :: "a.dat", "b.dat"], &
            choices=[character(len=5) :: "a.dat", "b.dat", "c.dat"])
        call parser%add_argument("--color", nargs="?", const="auto", &
            choices=[character(len=4) :: "auto", "on", "off"])

        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        definition = parser%get_argument(1)
        call check(error, definition%required, .true.)
        if (allocated(error)) return
        call check(error, definition%produces_list(), .true.)
        if (allocated(error)) return
        call check(error, definition%nargs_display(), "+")
        if (allocated(error)) return
        select type (stored => definition%default_value%item)
        type is (ListValue)
            call check(error, stored%size(), 2)
        class default
            call check(error, .false., "expected list default")
        end select
        if (allocated(error)) return

        definition = parser%get_argument(2)
        call check(error, definition%has_const, .true.)
        if (allocated(error)) return
        call check(error, definition%produces_list(), .false.)
        if (allocated(error)) return

        call positional_parser%init(add_help=.false.)
        call positional_parser%add_argument("maybe", nargs="?")
        call check(error, positional_parser%is_valid(), .true.)
        if (allocated(error)) return
        definition = positional_parser%get_argument(1)
        call check(error, definition%required, .false.)
        if (allocated(error)) return
        call check(error, definition%has_const, .false.)
    end subroutine test_arity_metadata

    subroutine test_builtin_actions(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(Argument) :: definition
        type(StoreConstAction) :: caller_action

        call parser%init(add_help=.false.)
        call parser%add_argument("--verbose", action=store_true())
        call parser%add_argument("--quiet", action=count())
        call parser%add_argument("--tag", action=append())
        call parser%add_argument("--mode", action=store_const("fast"))
        call parser%add_argument("--output", action=store())
        call parser%add_argument("--no-cache", action=store_false())
        caller_action = store_const("before")
        call parser%add_argument("--owned", action=caller_action)
        caller_action%constant = new_value("after")

        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        call check(error, parser%argument_count(), 7)
        if (allocated(error)) return

        definition = parser%get_argument(1)
        call check(error, definition%nargs_value(), 0)
        if (allocated(error)) return
        call check(error, definition%data_type, "logical")
        if (allocated(error)) return
        call check(error, definition%has_default, .true.)
        if (allocated(error)) return
        select type (action => definition%action)
        type is (StoreTrueAction)
            call check(error, .true.)
        class default
            call check(error, .false., "expected StoreTrueAction")
        end select
        if (allocated(error)) return

        definition = parser%get_argument(2)
        select type (action => definition%action)
        type is (CountAction)
            call check(error, definition%data_type, "integer")
        class default
            call check(error, .false., "expected CountAction")
        end select
        if (allocated(error)) return

        definition = parser%get_argument(3)
        select type (action => definition%action)
        type is (AppendAction)
            call check(error, definition%produces_list(), .true.)
        class default
            call check(error, .false., "expected AppendAction")
        end select
        if (allocated(error)) return

        definition = parser%get_argument(4)
        select type (action => definition%action)
        type is (StoreConstAction)
            call check(error, definition%has_const, .true.)
        class default
            call check(error, .false., "expected StoreConstAction")
        end select
        if (allocated(error)) return

        definition = parser%get_argument(5)
        select type (action => definition%action)
        type is (StoreAction)
            call check(error, definition%nargs_value(), 1)
        class default
            call check(error, .false., "expected StoreAction")
        end select
        if (allocated(error)) return

        definition = parser%get_argument(6)
        select type (action => definition%action)
        type is (StoreFalseAction)
            call check(error, definition%has_default, .true.)
        class default
            call check(error, .false., "expected StoreFalseAction")
        end select
        if (allocated(error)) return

        definition = parser%get_argument(7)
        select type (action => definition%action)
        type is (StoreConstAction)
            call check(error, action%constant%to_string(), "'before'")
        class default
            call check(error, .false., "expected owned StoreConstAction")
        end select
    end subroutine test_builtin_actions

    subroutine test_dynamic_growth(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        character(len=16) :: name
        integer :: index

        call parser%init(add_help=.false.)
        do index = 1, 64
            write(name, '("--option-", i0)') index
            call parser%add_argument(trim(name))
        end do

        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        call check(error, parser%argument_count(), 64)
        if (allocated(error)) return
        call check(error, parser%has_option("--option-64"), .true.)
        if (allocated(error)) return
        call check(error, parser%has_dest("option_64"), .true.)
    end subroutine test_dynamic_growth

    subroutine test_reject_names(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors

        call parser%init(add_help=.false.)
        call parser%add_argument("--good", dest="value")
        call parser%add_argument("")
        call parser%add_argument("input", "--mixed")
        call parser%add_argument("--good")
        call parser%add_argument("--other", dest="value")

        call check(error, parser%argument_count(), 1)
        if (allocated(error)) return
        call check(error, parser%is_valid(), .false.)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 4)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 1), ERR_INVALID_ARGUMENT_NAME)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 2), ERR_INVALID_ARGUMENT_NAME)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 3), ERR_DUPLICATE_OPTION)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 4), ERR_DUPLICATE_DEST)
        if (allocated(error)) return

        ! A later valid definition is committed but does not erase earlier
        ! configuration failures.
        call parser%add_argument("--later")
        call check(error, parser%argument_count(), 2)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 4)
        if (allocated(error)) return
        call check(error, parser%is_valid(), .false.)
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        call check(error, parser%is_valid(), .true.)
    end subroutine test_reject_names

    subroutine test_reject_incompatible_definitions(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors

        call parser%init(add_help=.false.)
        call parser%add_argument("--negative", nargs=-1)
        call parser%add_argument("--truth", action=store_true(), nargs=1)
        call parser%add_argument("--mystery", data_type="complex")
        call parser%add_argument("maybe", nargs="?", const="x", required=.true.)
        call parser%add_argument("--life", deprecated_msg="old", removed_msg="gone")
        call parser%add_argument("--rest", nargs="remainder")
        call parser%add_argument("--flag", action=store_true(), &
            choices=[character(len=3) :: "on", "off"])

        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 7)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 1), ERR_INVALID_NARGS)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 2), ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 3), ERR_INVALID_TYPE_NAME)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 4), ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 5), ERR_INVALID_LIFECYCLE)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 6), ERR_INVALID_NARGS)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 7), ERR_INCOMPATIBLE_ACTION)
    end subroutine test_reject_incompatible_definitions

    subroutine test_defaults_choices_validators(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        type(Argument) :: definition
        type(NonnegativeValidator) :: nonnegative

        call parser%init(add_help=.false.)
        call parser%add_argument("--threads", data_type="int", default="2", &
            choices=[1, 2, 4], validator=nonnegative)
        call check(error, parser%argument_count(), 1)
        if (allocated(error)) return
        definition = parser%get_argument(1)
        select type (stored => definition%default_value%item)
        type is (IntegerValue)
            call check(error, int(stored%value), 2)
        class default
            call check(error, .false., "expected integer default")
        end select
        if (allocated(error)) return
        call check(error, allocated(definition%validator), .true.)
        if (allocated(error)) return

        call parser%add_argument("--wrong-type", data_type="string", default=1)
        call parser%add_argument("--wrong-choice", data_type="integer", &
            default=3, choices=[1, 2])
        call parser%add_argument("--wrong-shape", nargs=2, default="scalar")
        call parser%add_argument("--missing-const", nargs="?")
        call parser%add_argument("--extra-const", const="x")
        call parser%add_argument("--bad-const", nargs="?", const="x", &
            choices=[character(len=1) :: "a", "b"])
        call parser%add_argument("--negative-default", data_type="integer", &
            default=-1, validator=nonnegative)

        call check(error, parser%argument_count(), 1)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 7)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 1), ERR_INVALID_DEFAULT)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 2), ERR_INVALID_DEFAULT)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 3), ERR_INVALID_DEFAULT)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 4), ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 5), ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 6), ERR_INVALID_DEFAULT)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 7), ERR_INVALID_DEFAULT)
    end subroutine test_defaults_choices_validators

    subroutine validate_nonnegative(self, value, valid, message)
        class(NonnegativeValidator), intent(in) :: self
        type(ValueBox), intent(in) :: value
        logical, intent(out) :: valid
        character(len=:), allocatable, intent(out) :: message

        select type (stored => value%item)
        type is (IntegerValue)
            valid = stored%value >= 0
        class default
            valid = .false.
        end select
        if (valid) then
            message = ""
        else
            message = "value must be nonnegative"
        end if
    end subroutine validate_nonnegative

    subroutine validate_silently(self, value, valid, message)
        class(SilentValidator), intent(in) :: self
        type(ValueBox), intent(in) :: value
        logical, intent(out) :: valid
        character(len=:), allocatable, intent(out) :: message

        valid = .false.
    end subroutine validate_silently

    !> A validator may reject without a message; registration must not
    !> dereference it.
    subroutine test_silent_validator(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        type(SilentValidator) :: silent

        call parser%init(add_help=.false.)
        call parser%add_argument("--n", data_type="integer", default=3, &
            validator=silent)
        call parser%add_argument("--c", data_type="integer", nargs="?", &
            const=3, validator=silent)
        call parser%add_argument("--l", data_type="integer", nargs=2, &
            default=[1, 2], validator=silent)

        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 3)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 1), ERR_INVALID_DEFAULT)
        if (allocated(error)) return
        call check(error, error_message_at(errors, 1), &
            "default value failed validation")
        if (allocated(error)) return
        call check(error, error_message_at(errors, 2), &
            "const value failed validation")
        if (allocated(error)) return
        call check(error, error_message_at(errors, 3), &
            "default value failed validation")
    end subroutine test_silent_validator

    !> Names that can never match a token, or that shadow negative numbers.
    subroutine test_reject_name_syntax(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        integer :: index

        call parser%init(add_help=.false.)
        call parser%add_argument("---x")
        call parser%add_argument("--a b")
        call parser%add_argument("--a=b")
        call parser%add_argument("-x", "--y=z")
        call parser%add_argument("-1")
        call parser%add_argument("-.5")
        call parser%add_argument("in put")
        call parser%add_argument(" x")
        call parser%add_argument("--x", dest="a b")
        call parser%add_argument("--x", dest="a=b")

        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 10)
        if (allocated(error)) return
        do index = 1, errors%count()
            call check(error, error_code_at(errors, index), &
                ERR_INVALID_ARGUMENT_NAME)
            if (allocated(error)) return
        end do

        ! Multi-letter single-dash options are supported (exact spelling
        ! wins over a cluster), and digits after a letter are not a number.
        call parser%init(add_help=.false.)
        call parser%add_argument("-ab", action=store_true())
        call parser%add_argument("-np")
        call parser%add_argument("-x1")
        call parser%add_argument("--2d", action=store_true())
        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        call check(error, parser%argument_count(), 4)
    end subroutine test_reject_name_syntax

    !> A positional is set by consuming a value, so a zero-value action
    !> could never set it.
    subroutine test_reject_zero_value_positionals(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        integer :: index

        call parser%init(add_help=.false.)
        call parser%add_argument("a", action=store_true())
        call parser%add_argument("b", action=store_false())
        call parser%add_argument("c", action=count())
        call parser%add_argument("d", action=boolean_optional())
        call parser%add_argument("e", action=store_const(3))
        call parser%add_argument("f", action=store_true(), nargs=0)
        call parser%add_argument("g", nargs=0)

        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 7)
        if (allocated(error)) return
        do index = 1, errors%count()
            call check(error, error_code_at(errors, index), &
                ERR_INCOMPATIBLE_ACTION)
            if (allocated(error)) return
        end do
    end subroutine test_reject_zero_value_positionals

    subroutine test_reject_unrequired_positional(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors

        call parser%init(add_help=.false.)
        call parser%add_argument("input", required=.false.)
        call parser%add_argument("files", nargs="+", required=.false.)
        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 2)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 1), ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 2), ERR_INCOMPATIBLE_ACTION)
        if (allocated(error)) return

        ! Statements that agree with the nargs are accepted.
        call parser%init(add_help=.false.)
        call parser%add_argument("input", required=.true.)
        call parser%add_argument("extra", nargs="?", required=.false.)
        call check(error, parser%is_valid(), .true.)
    end subroutine test_reject_unrequired_positional

    subroutine test_reject_bad_choices(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors
        integer :: index

        call parser%init(add_help=.false.)
        call parser%add_argument("--empty", data_type="integer", &
            choices=[integer ::])
        call parser%add_argument("--ints", data_type="integer", choices=[1, 1])
        call parser%add_argument("--words", choices=[character(len=1) :: &
            "a", "b", "a"])
        call parser%add_argument("--range", data_type="integer", &
            choices=[1, 5, 10], validator=in_range(2, 8))

        call check(error, parser%argument_count(), 0)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        call check(error, errors%count(), 4)
        if (allocated(error)) return
        do index = 1, errors%count()
            call check(error, error_code_at(errors, index), ERR_INVALID_DEFAULT)
            if (allocated(error)) return
        end do
        call check(error, error_message_at(errors, 4), "choice 1 failed " // &
            "validation: value must be between 2 and 8")
        if (allocated(error)) return

        call parser%init(add_help=.false.)
        call parser%add_argument("--range", data_type="integer", &
            choices=[2, 5, 8], validator=in_range(2, 8))
        call check(error, parser%is_valid(), .true.)
    end subroutine test_reject_bad_choices

    !> The automatic help and version arguments are named in collisions.
    subroutine test_builtin_collisions(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ErrorStack) :: errors

        call parser%init(prog="demo", version="demo 1.0")
        call parser%add_argument("help")
        call parser%add_argument("--version", dest="shown")
        call parser%add_argument("--other", dest="version")

        errors = parser%get_config_errors()
        call check(error, errors%count(), 3)
        if (allocated(error)) return
        call check(error, error_code_at(errors, 1), ERR_DUPLICATE_DEST)
        if (allocated(error)) return
        call check(error, error_message_at(errors, 1), &
            "the destination collides with the built-in help argument")
        if (allocated(error)) return
        call check(error, error_code_at(errors, 2), ERR_DUPLICATE_OPTION)
        if (allocated(error)) return
        call check(error, error_message_at(errors, 2), &
            "an option name collides with the built-in version argument")
        if (allocated(error)) return
        call check(error, error_code_at(errors, 3), ERR_DUPLICATE_DEST)
        if (allocated(error)) return
        call check(error, error_message_at(errors, 3), &
            "the destination collides with the built-in version argument")
    end subroutine test_builtin_collisions

    function error_message_at(errors, index) result(message)
        type(ErrorStack), intent(in) :: errors
        integer, intent(in) :: index
        character(len=:), allocatable :: message
        type(ErrorEntry) :: entry

        entry = errors%get(index)
        message = entry%message
    end function error_message_at

    integer function error_code_at(errors, index) result(code)
        type(ErrorStack), intent(in) :: errors
        integer, intent(in) :: index
        type(ErrorEntry) :: entry

        entry = errors%get(index)
        code = entry%code
    end function error_code_at

end module test_registration
