!> Actions module for fclap
!>
!> This module defines the Action type which stores all data about
!> an argument action, and provides methods for matching, validating,
!> and executing actions.

module fclap_actions
    use fclap_constants, only: MAX_ARG_LEN, MAX_OPTION_STRINGS, MAX_CHOICES, &
        TYPE_STRING, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL, &
        ACT_STORE, ACT_STORE_TRUE, ACT_STORE_FALSE, ACT_COUNT, ACT_APPEND, &
        ACT_HELP, ACT_VERSION, ARG_SINGLE, &
        STATUS_ACTIVE, STATUS_DEPRECATED, STATUS_REMOVED
    use fclap_namespace, only: ValueContainer, Namespace
    use fclap_errors, only: fclap_error
    implicit none
    private

    ! ============================================================================
    ! ACTION TYPE
    ! ============================================================================

    !> Action type - stores all data about an argument action
    !> Represents a single argument definition with all its properties
    type, public :: Action
        !> Destination name where the argument value will be stored
        character(len=:), allocatable :: dest
        !> Array of option strings (e.g., "-v", "--verbose")
        character(len=MAX_ARG_LEN) :: option_strings(MAX_OPTION_STRINGS)
        !> Number of option strings defined for this argument
        integer :: num_option_strings = 0
        !> Number of arguments consumed (ARG_* constants or positive integer)
        integer :: nargs = ARG_SINGLE
        !> Help text description displayed in usage/help output
        character(len=:), allocatable :: help_text
        !> Whether this argument is required (positional args are always required)
        logical :: required = .false.
        !> Default value if argument is not provided on command line
        type(ValueContainer) :: default_value
        !> Flag indicating whether a default value has been set
        logical :: has_default = .false.
        !> Metavar for display in usage/help (e.g., "FILE" instead of "filename")
        character(len=:), allocatable :: metavar
        !> Array of valid choices that constrain acceptable values
        character(len=MAX_ARG_LEN) :: choices(MAX_CHOICES)
        !> Number of choices defined for this argument
        integer :: num_choices = 0
        !> Expected value type (TYPE_STRING, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL)
        integer :: value_type = TYPE_STRING
        !> Whether this is a positional argument (vs optional/flag)
        logical :: is_positional = .false.
        !> The action type (ACT_STORE, ACT_STORE_TRUE, ACT_COUNT, etc.)
        integer :: action_type = ACT_STORE
        !> Version string for ACT_VERSION action
        character(len=:), allocatable :: version_string
        !> Argument status (STATUS_ACTIVE, STATUS_DEPRECATED, STATUS_REMOVED)
        integer :: status = STATUS_ACTIVE
        !> Whether argument is visible in help output (hidden if .false.)
        logical :: visible = .true.
        !> Deprecation message shown when deprecated argument is used
        character(len=:), allocatable :: deprecated_message
        !> Removal message shown when removed argument is used
        character(len=:), allocatable :: removed_message
    contains
        procedure :: matches_option => action_matches_option
        procedure :: get_display_name => action_get_display_name
        procedure :: is_valid_choice => action_is_valid_choice
        procedure :: execute => action_execute
        procedure :: check_status => action_check_status
    end type Action

contains

    !> Check if action matches given option string
    function action_matches_option(self, opt) result(matches)
        class(Action), intent(in) :: self
        character(len=*), intent(in) :: opt
        logical :: matches
        integer :: i

        matches = .false.
        do i = 1, self%num_option_strings
            if (trim(self%option_strings(i)) == trim(opt)) then
                matches = .true.
                return
            end if
        end do
    end function action_matches_option

    !> Get display name for action (longest option string or dest)
    function action_get_display_name(self) result(name)
        class(Action), intent(in) :: self
        character(len=:), allocatable :: name
        integer :: i, max_len, idx

        if (self%num_option_strings > 0) then
            max_len = 0
            idx = 1
            do i = 1, self%num_option_strings
                if (len_trim(self%option_strings(i)) > max_len) then
                    max_len = len_trim(self%option_strings(i))
                    idx = i
                end if
            end do
            name = trim(self%option_strings(idx))
        else
            name = self%dest
        end if
    end function action_get_display_name

    !> Check if value is in choices (if choices defined)
    function action_is_valid_choice(self, value) result(valid)
        class(Action), intent(in) :: self
        character(len=*), intent(in) :: value
        logical :: valid
        integer :: i

        if (self%num_choices == 0) then
            valid = .true.
            return
        end if

        valid = .false.
        do i = 1, self%num_choices
            if (trim(self%choices(i)) == trim(value)) then
                valid = .true.
                return
            end if
        end do
    end function action_is_valid_choice

    !> Check argument status and handle deprecated/removed arguments
    !> Returns .true. if argument can proceed, .false. if removed
    function action_check_status(self, error) result(can_proceed)
        class(Action), intent(in) :: self
        type(fclap_error), intent(inout) :: error
        logical :: can_proceed
        character(len=:), allocatable :: display_name, msg

        can_proceed = .true.
        display_name = self%get_display_name()

        select case(self%status)
        case(STATUS_ACTIVE)
            can_proceed = .true.

        case(STATUS_DEPRECATED)
            if (allocated(self%deprecated_message)) then
                msg = self%deprecated_message
            else
                msg = "argument '" // trim(display_name) // "' is deprecated"
            end if
            write(*, '(A,A)') "warning: ", trim(msg)
            can_proceed = .true.

        case(STATUS_REMOVED)
            if (allocated(self%removed_message)) then
                msg = self%removed_message
            else
                msg = "argument '" // trim(display_name) // "' has been removed"
            end if
            call error%init(msg, display_name)
            can_proceed = .false.

        case default
            can_proceed = .true.
        end select
    end function action_check_status

    !> Execute action based on action type
    subroutine action_execute(self, args, values, num_values, error)
        class(Action), intent(in) :: self
        type(Namespace), intent(inout) :: args
        character(len=*), intent(in) :: values(:)
        integer, intent(in) :: num_values
        type(fclap_error), intent(inout) :: error
        integer :: int_val, ios, i
        real :: real_val

        select case(self%action_type)
        case(ACT_STORE)
            if (num_values < 1) then
                call error%init("expected one argument", self%dest)
                return
            end if

            if (.not. self%is_valid_choice(values(1))) then
                call error%init("invalid choice", values(1))
                return
            end if

            select case(self%value_type)
            case(TYPE_INTEGER)
                read(values(1), *, iostat=ios) int_val
                if (ios /= 0) then
                    call error%init("invalid integer value", values(1))
                    return
                end if
                call args%set_integer(self%dest, int_val)

            case(TYPE_REAL)
                read(values(1), *, iostat=ios) real_val
                if (ios /= 0) then
                    call error%init("invalid real value", values(1))
                    return
                end if
                call args%set_real(self%dest, real_val)

            case default
                call args%set_string(self%dest, values(1))
            end select

        case(ACT_STORE_TRUE)
            call args%set_logical(self%dest, .true.)

        case(ACT_STORE_FALSE)
            call args%set_logical(self%dest, .false.)

        case(ACT_COUNT)
            call args%increment(self%dest)

        case(ACT_APPEND)
            if (num_values < 1) then
                call error%init("expected at least one argument", self%dest)
                return
            end if

            do i = 1, num_values
                if (.not. self%is_valid_choice(values(i))) then
                    call error%init("invalid choice", values(i))
                    return
                end if

                select case(self%value_type)
                case(TYPE_INTEGER)
                    read(values(i), *, iostat=ios) int_val
                    if (ios /= 0) then
                        call error%init("invalid integer value", values(i))
                        return
                    end if
                    call args%append_integer(self%dest, int_val)

                case default
                    call args%append_string(self%dest, values(i))
                end select
            end do

        case(ACT_HELP)
            call args%set_logical("__help__", .true.)

        case(ACT_VERSION)
            if (allocated(self%version_string)) then
                write(*, '(A)') trim(self%version_string)
            end if
            stop

        end select
    end subroutine action_execute

end module fclap_actions
