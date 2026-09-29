!> Normalized representations of argparse-style nargs specifications.
!> nargs means number of arguments and with this is meant how many arguments, i.e. values are passed to a single flag provided via the cli
!> for example nargs = 1 -> --threads 5
!> nargs = 2 -> --solvents water octanol
module fclap_nargs
    implicit none
    private

    integer, parameter, public :: NARGS_INVALID      = -huge(0)
    integer, parameter, public :: NARGS_OPTIONAL     = -1
    integer, parameter, public :: NARGS_ZERO_OR_MORE = -2
    integer, parameter, public :: NARGS_ONE_OR_MORE  = -3
    integer, parameter, public :: NARGS_REMAINDER    = -4
    integer, parameter, public :: NARGS_ZERO         = 0
    integer, parameter, public :: NARGS_ONE          = 1
    integer, parameter, public :: NARGS_UNBOUNDED    = huge(0)

    integer, parameter, public :: NARGS_SUCCESS       = 0
    integer, parameter, public :: NARGS_INVALID_VALUE = 1

    type, public :: NargsSpec
        private
        integer :: code = NARGS_ONE
    contains
        procedure :: value         => nargs_spec_value
        procedure :: is_valid      => nargs_spec_is_valid
        procedure :: min_count     => nargs_spec_min_count
        procedure :: max_count     => nargs_spec_max_count
        procedure :: is_variable   => nargs_spec_is_variable
        procedure :: produces_list => nargs_spec_produces_list
        procedure :: to_string     => nargs_spec_to_string
    end type NargsSpec

    interface new_nargs
        module procedure :: new_nargs_integer
        module procedure :: new_nargs_character
    end interface new_nargs

    interface normalize_nargs
        module procedure :: normalize_nargs_integer
        module procedure :: normalize_nargs_character
    end interface normalize_nargs

    public :: new_nargs
    public :: normalize_nargs
    public :: nargs_is_valid
    public :: nargs_min_count
    public :: nargs_max_count
    public :: nargs_is_variable
    public :: nargs_produces_list
    public :: nargs_to_string

contains

    pure function new_nargs_integer(value) result(spec)
        integer, intent(in) :: value
        type(NargsSpec) :: spec

        if (is_valid_public_integer(value)) then
            spec%code = value
        else
            spec%code = NARGS_INVALID
        end if
    end function new_nargs_integer

    pure function new_nargs_character(value) result(spec)
        character(len=*), intent(in) :: value
        type(NargsSpec) :: spec
        integer :: stat

        call normalize_nargs_character(value, spec, stat)
    end function new_nargs_character

    pure subroutine normalize_nargs_integer(value, spec, stat)
        integer, intent(in) :: value
        type(NargsSpec), intent(out) :: spec
        integer, intent(out) :: stat

        if (is_valid_public_integer(value)) then
            spec%code = value
            stat = NARGS_SUCCESS
        else
            spec%code = NARGS_INVALID
            stat = NARGS_INVALID_VALUE
        end if
    end subroutine normalize_nargs_integer

    pure subroutine normalize_nargs_character(value, spec, stat)
        character(len=*), intent(in) :: value
        type(NargsSpec), intent(out) :: spec
        integer, intent(out) :: stat
        character(len=:), allocatable :: normalized

        normalized = trim(adjustl(value))
        select case (normalized)
        case ("?")
            spec%code = NARGS_OPTIONAL
            stat = NARGS_SUCCESS
        case ("*")
            spec%code = NARGS_ZERO_OR_MORE
            stat = NARGS_SUCCESS
        case ("+")
            spec%code = NARGS_ONE_OR_MORE
            stat = NARGS_SUCCESS
        case ("remainder", "REMAINDER")
            spec%code = NARGS_REMAINDER
            stat = NARGS_SUCCESS
        case default
            spec%code = NARGS_INVALID
            stat = NARGS_INVALID_VALUE
        end select
    end subroutine normalize_nargs_character

    pure integer function nargs_spec_value(self) result(value)
        class(NargsSpec), intent(in) :: self

        value = self%code
    end function nargs_spec_value

    pure logical function nargs_spec_is_valid(self) result(valid)
        class(NargsSpec), intent(in) :: self

        valid = nargs_is_valid(self%code)
    end function nargs_spec_is_valid

    pure integer function nargs_spec_min_count(self) result(count)
        class(NargsSpec), intent(in) :: self

        count = nargs_min_count(self%code)
    end function nargs_spec_min_count

    pure integer function nargs_spec_max_count(self) result(count)
        class(NargsSpec), intent(in) :: self

        count = nargs_max_count(self%code)
    end function nargs_spec_max_count

    pure logical function nargs_spec_is_variable(self) result(variable)
        class(NargsSpec), intent(in) :: self

        variable = nargs_is_variable(self%code)
    end function nargs_spec_is_variable

    pure logical function nargs_spec_produces_list(self) result(produces_list)
        class(NargsSpec), intent(in) :: self

        produces_list = nargs_produces_list(self%code)
    end function nargs_spec_produces_list

    pure function nargs_spec_to_string(self) result(string)
        class(NargsSpec), intent(in) :: self
        character(len=:), allocatable :: string

        string = nargs_to_string(self%code)
    end function nargs_spec_to_string

    pure logical function nargs_is_valid(value) result(valid)
        integer, intent(in) :: value

        valid = value >= NARGS_ZERO .or. value == NARGS_OPTIONAL .or. &
            value == NARGS_ZERO_OR_MORE .or. value == NARGS_ONE_OR_MORE .or. &
            value == NARGS_REMAINDER
    end function nargs_is_valid

    pure logical function is_valid_public_integer(value) result(valid)
        integer, intent(in) :: value

        ! Symbolic modes have negative internal codes, but only the explicitly
        ! named remainder sentinel is also a public integer spelling.  Other
        ! negative integer input must not accidentally alias "?", "*", or "+".
        valid = value >= NARGS_ZERO .or. value == NARGS_REMAINDER
    end function is_valid_public_integer

    pure integer function nargs_min_count(value) result(count)
        integer, intent(in) :: value

        select case (value)
        case (NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, NARGS_REMAINDER)
            count = 0
        case (NARGS_ONE_OR_MORE)
            count = 1
        case (NARGS_ZERO:)
            count = value
        case default
            count = -1
        end select
    end function nargs_min_count

    pure integer function nargs_max_count(value) result(count)
        integer, intent(in) :: value

        select case (value)
        case (NARGS_OPTIONAL)
            count = 1
        case (NARGS_ZERO_OR_MORE, NARGS_ONE_OR_MORE, NARGS_REMAINDER)
            count = NARGS_UNBOUNDED
        case (NARGS_ZERO:)
            count = value
        case default
            count = -1
        end select
    end function nargs_max_count

    pure logical function nargs_is_variable(value) result(variable)
        integer, intent(in) :: value

        variable = value == NARGS_OPTIONAL .or. value == NARGS_ZERO_OR_MORE .or. &
            value == NARGS_ONE_OR_MORE .or. value == NARGS_REMAINDER
    end function nargs_is_variable

    pure logical function nargs_produces_list(value) result(produces_list)
        integer, intent(in) :: value

        produces_list = value > NARGS_ONE .or. value == NARGS_ZERO_OR_MORE .or. &
            value == NARGS_ONE_OR_MORE .or. value == NARGS_REMAINDER
    end function nargs_produces_list

    pure function nargs_to_string(value) result(string)
        integer, intent(in) :: value
        character(len=:), allocatable :: string
        character(len=32) :: buffer

        select case (value)
        case (NARGS_OPTIONAL)
            string = "?"
        case (NARGS_ZERO_OR_MORE)
            string = "*"
        case (NARGS_ONE_OR_MORE)
            string = "+"
        case (NARGS_REMAINDER)
            string = "remainder"
        case (NARGS_ZERO:)
            write(buffer, '(i0)') value
            string = trim(buffer)
        case default
            string = "invalid"
        end select
    end function nargs_to_string

end module fclap_nargs
