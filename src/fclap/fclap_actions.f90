!> @file fclap_actions.f90
!> @brief Actions module for fclap - defines argument action types.
!>
!> @details This module defines the Action type which stores all data about
!> an argument action, and provides methods for matching, validating,
!> and executing actions.
!>
!> An Action represents a single argument definition and encapsulates:
!> - Option strings (e.g., "-v", "--verbose")
!> - Argument properties (nargs, type, default, choices)
!> - Action behavior (store, store_true, count, append, etc.)
!> - Help text and visibility settings

module fclap_actions
    use fclap_constants, only: MAX_ARG_LEN, MAX_OPTION_STRINGS, MAX_CHOICES, &
        TYPE_STRING, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL, &
        ACT_STORE, ACT_STORE_TRUE, ACT_STORE_FALSE, ACT_COUNT, ACT_APPEND, &
        ACT_HELP, ACT_VERSION, ACT_NOT_LESS_THAN, ACT_NOT_BIGGER_THAN, &
        ARG_SINGLE, &
        ARG_OPTIONAL, ARG_ONE_OR_MORE, ARG_ZERO_OR_MORE, ARG_REMAINDER, &
        STATUS_ACTIVE, STATUS_DEPRECATED, STATUS_REMOVED
    use fclap_namespace, only: ValueContainer, Namespace
    use fclap_errors, only: fclap_error
    use fclap_utils_accuracy, only: wp
    implicit none
    private

    ! ============================================================================
    ! GENERIC INTERFACES FOR BOUND-CHECKING ACTION FACTORIES
    ! ============================================================================

    !> @brief Factory function: returns an action string for not-less-than validation.
    !>
    !> @details Generic interface supporting integer, real(wp), and string types.
    !> Returns an encoded action string (e.g. "not_less_than:5") that the parser
    !> recognizes. Usage: action=not_less_than(0.0_wp)
    !> For strings, the bound is passed as a character and will be compared
    !> against the trimmed string length during validation.
    interface not_less_than
        module procedure not_less_than_int
        module procedure not_less_than_real
        module procedure not_less_than_string
    end interface not_less_than

    !> @brief Factory function: returns an action string for not-bigger-than validation.
    !>
    !> @details Generic interface supporting integer, real(wp), and string types.
    !> Returns an encoded action string (e.g. "not_bigger_than:100") that the parser
    !> recognizes. Usage: action=not_bigger_than(100)
    !> For strings, the bound is passed as a character and will be compared
    !> against the trimmed string length during validation.
    interface not_bigger_than
        module procedure not_bigger_than_int
        module procedure not_bigger_than_real
        module procedure not_bigger_than_string
    end interface not_bigger_than

    public :: not_less_than, not_bigger_than

    ! ============================================================================
    ! ACTION TYPE
    ! ============================================================================

    !> @brief Represents a single argument action with all its properties.
    !>
    !> @details The Action type encapsulates everything about an argument:
    !> how it appears on the command line, what type of value it expects,
    !> how many values it consumes, and what happens when it's encountered.
    !>
    !> Actions are created by ArgumentParser%add_argument() and stored
    !> internally for use during parsing.
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
        !> @brief Deprecation message shown when deprecated argument is used
        character(len=:), allocatable :: deprecated_message
        !> @brief Removal message shown when removed argument is used
        character(len=:), allocatable :: removed_message
        !> @brief Bound value string for not_less_than / not_bigger_than actions
        character(len=:), allocatable :: bound_str
    contains
        !> @brief Check if action matches a given option string.
        !> @param opt The option string to check (e.g., "-v" or "--verbose")
        !> @return .true. if this action handles the given option
        procedure :: matches_option => action_matches_option
        !> @brief Get the display name for this action.
        !> @return The longest option string or destination name
        procedure :: get_display_name => action_get_display_name
        !> @brief Check if a value is in the allowed choices.
        !> @param value The value to validate
        !> @return .true. if valid (or no choices defined)
        procedure :: is_valid_choice => action_is_valid_choice
        !> @brief Execute the action with given values.
        !> @param args The Namespace to store results in
        !> @param values Array of consumed argument values
        !> @param num_values Number of values consumed
        !> @param error Error object for reporting issues
        procedure :: execute => action_execute
        !> @brief Check argument status and handle deprecation.
        !> @param error Error object for removed arguments
        !> @return .true. if argument can proceed
        procedure :: check_status => action_check_status
    end type Action

contains

    !> @brief Check if action matches given option string.
    !>
    !> @details Compares the given option string against all option strings
    !> registered for this action.
    !>
    !> @param self The Action instance
    !> @param opt The option string to check
    !> @return .true. if this action matches the option
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

    !> @brief Get display name for action (longest option string or dest).
    !>
    !> @details Returns the longest option string for display in help/error
    !> messages. For positional arguments, returns the destination name.
    !>
    !> @param self The Action instance
    !> @return The display name string
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

    !> @brief Check if value is in choices (if choices defined).
    !>
    !> @details Validates the given value against the list of allowed choices.
    !> If no choices are defined, any value is considered valid.
    !>
    !> @param self The Action instance
    !> @param value The value to validate
    !> @return .true. if value is valid
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

    !> @brief Check argument status and handle deprecated/removed arguments.
    !>
    !> @details Checks the argument's status flag and takes appropriate action:
    !> - STATUS_ACTIVE: Proceeds normally
    !> - STATUS_DEPRECATED: Prints warning but continues
    !> - STATUS_REMOVED: Sets error and returns .false.
    !>
    !> @param self The Action instance
    !> @param error Error object for removed arguments
    !> @return .true. if argument can proceed, .false. if removed
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

    !> @brief Execute action based on action type.
    !>
    !> @details Performs the action's behavior based on its action_type:
    !> - ACT_STORE: Store a single value
    !> - ACT_STORE_TRUE/FALSE: Store .true./.false.
    !> - ACT_COUNT: Increment counter
    !> - ACT_APPEND: Append to list
    !> - ACT_HELP: Set help flag
    !> - ACT_VERSION: Print version and exit
    !>
    !> @param self The Action instance
    !> @param args The Namespace to store results in
    !> @param values Array of consumed argument values
    !> @param num_values Number of values consumed
    !> @param error Error object for reporting issues
    subroutine action_execute(self, args, values, num_values, error)
        class(Action), intent(in) :: self
        type(Namespace), intent(inout) :: args
        character(len=*), intent(in) :: values(:)
        integer, intent(in) :: num_values
        type(fclap_error), intent(inout) :: error
        integer :: int_val, ios, i, int_bound, str_len
        real(wp) :: real_val, real_bound
        real :: store_real_val

        select case(self%action_type)
        case(ACT_STORE)
            ! Check if this is a multi-value nargs (list storage)
            if (self%nargs == ARG_ONE_OR_MORE .or. self%nargs == ARG_ZERO_OR_MORE .or. &
                self%nargs == ARG_OPTIONAL .or. &
                self%nargs == ARG_REMAINDER .or. self%nargs > 1) then
                ! Store all values as a list
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
            else
                ! Single-value storage
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
                    store_real_val = real(real_val)
                    call args%set_real(self%dest, store_real_val)

                case default
                    call args%set_string(self%dest, values(1))
                end select
            end if

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

        case(ACT_NOT_LESS_THAN)
            ! Store value and validate it is >= bound
            if (num_values < 1) then
                call error%init("expected one argument", self%dest)
                return
            end if

            select case(self%value_type)
            case(TYPE_INTEGER)
                read(values(1), *, iostat=ios) int_val
                if (ios /= 0) then
                    call error%init("invalid integer value", values(1))
                    return
                end if
                read(self%bound_str, *, iostat=ios) int_bound
                if (ios /= 0) then
                    call error%init("invalid bound value", self%bound_str)
                    return
                end if
                if (int_val < int_bound) then
                            call error%init( &
                                "value must not be less than " // trim(self%bound_str), &
                                values(1))
                    return
                end if
                call args%set_integer(self%dest, int_val)

            case(TYPE_REAL)
                read(values(1), *, iostat=ios) real_val
                if (ios /= 0) then
                    call error%init("invalid real value", values(1))
                    return
                end if
                read(self%bound_str, *, iostat=ios) real_bound
                if (ios /= 0) then
                    call error%init("invalid bound value", self%bound_str)
                    return
                end if
                if (real_val < real_bound) then
                            call error%init( &
                                "value must not be less than " // trim(self%bound_str), &
                                values(1))
                    return
                end if
                store_real_val = real(real_val)
                call args%set_real(self%dest, store_real_val)

            case default
                ! String type: compare len(trim(value)) against integer bound
                str_len = len(trim(values(1)))
                read(self%bound_str, *, iostat=ios) int_bound
                if (ios /= 0) then
                    call error%init("invalid bound value", self%bound_str)
                    return
                end if
                if (str_len < int_bound) then
                            call error%init( &
                                "string length must not be less than " // trim(self%bound_str), &
                                values(1))
                    return
                end if
                call args%set_string(self%dest, values(1))
            end select

        case(ACT_NOT_BIGGER_THAN)
            ! Store value and validate it is <= bound
            if (num_values < 1) then
                call error%init("expected one argument", self%dest)
                return
            end if

            select case(self%value_type)
            case(TYPE_INTEGER)
                read(values(1), *, iostat=ios) int_val
                if (ios /= 0) then
                    call error%init("invalid integer value", values(1))
                    return
                end if
                read(self%bound_str, *, iostat=ios) int_bound
                if (ios /= 0) then
                    call error%init("invalid bound value", self%bound_str)
                    return
                end if
                if (int_val > int_bound) then
                            call error%init( &
                                "value must not be bigger than " // trim(self%bound_str), &
                                values(1))
                    return
                end if
                call args%set_integer(self%dest, int_val)

            case(TYPE_REAL)
                read(values(1), *, iostat=ios) real_val
                if (ios /= 0) then
                    call error%init("invalid real value", values(1))
                    return
                end if
                read(self%bound_str, *, iostat=ios) real_bound
                if (ios /= 0) then
                    call error%init("invalid bound value", self%bound_str)
                    return
                end if
                if (real_val > real_bound) then
                            call error%init( &
                                "value must not be bigger than " // trim(self%bound_str), &
                                values(1))
                    return
                end if
                store_real_val = real(real_val)
                call args%set_real(self%dest, store_real_val)

            case default
                ! String type: compare len(trim(value)) against integer bound
                str_len = len(trim(values(1)))
                read(self%bound_str, *, iostat=ios) int_bound
                if (ios /= 0) then
                    call error%init("invalid bound value", self%bound_str)
                    return
                end if
                if (str_len > int_bound) then
                            call error%init( &
                                "string length must not be bigger than " // trim(self%bound_str), &
                                values(1))
                    return
                end if
                call args%set_string(self%dest, values(1))
            end select

        end select
    end subroutine action_execute

    ! ============================================================================
    ! NOT_LESS_THAN FACTORY PROCEDURES
    ! ============================================================================

    !> @brief Create an action string for integer not-less-than validation.
    !>
    !> @param bound The minimum allowed value
    !> @return Encoded action string "not_less_than:<bound>"
    function not_less_than_int(bound) result(action_string)
        integer, intent(in) :: bound
        character(len=:), allocatable :: action_string
        character(len=32) :: buf

        write(buf, '(I0)') bound
        action_string = "not_less_than:" // trim(buf)
    end function not_less_than_int

    !> @brief Create an action string for real(wp) not-less-than validation.
    !>
    !> @param bound The minimum allowed value (working precision)
    !> @return Encoded action string "not_less_than:<bound>"
    function not_less_than_real(bound) result(action_string)
        real(wp), intent(in) :: bound
        character(len=:), allocatable :: action_string
        character(len=64) :: buf

        write(buf, '(ES23.16)') bound
        action_string = "not_less_than:" // trim(adjustl(buf))
    end function not_less_than_real

    !> @brief Create an action string for string not-less-than validation.
    !>
    !> @details The bound is passed as a character string representing the
    !> minimum trimmed length. During validation, `len(trim(value))` is
    !> compared against this bound.
    !>
    !> @param bound The minimum allowed trimmed length (as string, e.g. "5")
    !> @return Encoded action string "not_less_than:<bound>"
    function not_less_than_string(bound) result(action_string)
        character(len=*), intent(in) :: bound
        character(len=:), allocatable :: action_string

        action_string = "not_less_than:" // trim(bound)
    end function not_less_than_string

    ! ============================================================================
    ! NOT_BIGGER_THAN FACTORY PROCEDURES
    ! ============================================================================

    !> @brief Create an action string for integer not-bigger-than validation.
    !>
    !> @param bound The maximum allowed value
    !> @return Encoded action string "not_bigger_than:<bound>"
    function not_bigger_than_int(bound) result(action_string)
        integer, intent(in) :: bound
        character(len=:), allocatable :: action_string
        character(len=32) :: buf

        write(buf, '(I0)') bound
        action_string = "not_bigger_than:" // trim(buf)
    end function not_bigger_than_int

    !> @brief Create an action string for real(wp) not-bigger-than validation.
    !>
    !> @param bound The maximum allowed value (working precision)
    !> @return Encoded action string "not_bigger_than:<bound>"
    function not_bigger_than_real(bound) result(action_string)
        real(wp), intent(in) :: bound
        character(len=:), allocatable :: action_string
        character(len=64) :: buf

        write(buf, '(ES23.16)') bound
        action_string = "not_bigger_than:" // trim(adjustl(buf))
    end function not_bigger_than_real

    !> @brief Create an action string for string not-bigger-than validation.
    !>
    !> @details The bound is passed as a character string representing the
    !> maximum trimmed length. During validation, `len(trim(value))` is
    !> compared against this bound.
    !>
    !> @param bound The maximum allowed trimmed length (as string, e.g. "100")
    !> @return Encoded action string "not_bigger_than:<bound>"
    function not_bigger_than_string(bound) result(action_string)
        character(len=*), intent(in) :: bound
        character(len=:), allocatable :: action_string

        action_string = "not_bigger_than:" // trim(bound)
    end function not_bigger_than_string

end module fclap_actions
