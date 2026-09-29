!> Normalized, owned definition of one registered argument.
module fclap_argument
    use fclap_actions_abstract, only : ActionType
    use fclap_nargs, only : NargsSpec
    use fclap_validators_abstract, only : ValidatorType
    use fclap_value_abstract, only : ValueBox
    implicit none
    private

    public :: Argument
    public :: derive_dest_from_names

    type :: Argument
        character(len=:), allocatable :: names(:)
        character(len=:), allocatable :: dest
        logical :: is_optional = .false.

        type(NargsSpec) :: nargs
        character(len=:), allocatable :: data_type
        class(ActionType), allocatable :: action
        class(ValidatorType), allocatable :: validator

        type(ValueBox) :: default_value
        type(ValueBox) :: const_value
        type(ValueBox) :: choices
        logical :: has_default = .false.
        logical :: has_const = .false.
        logical :: has_choices = .false.

        character(len=:), allocatable :: help
        character(len=:), allocatable :: metavar
        logical :: required = .false.
        logical :: visible = .true.
        logical :: print_default = .true.
        logical :: print_choices = .false.
        character(len=:), allocatable :: deprecated_msg
        character(len=:), allocatable :: removed_msg
    contains
        procedure :: init => argument_init
        procedure :: name_count => argument_name_count
        procedure :: matches_name => argument_matches_name
        procedure :: is_positional => argument_is_positional
        procedure :: primary_name => argument_primary_name
        procedure :: derive_dest => argument_derive_dest
        procedure :: effective_metavar => argument_effective_metavar
        procedure :: nargs_display => argument_nargs_display
        procedure :: nargs_value => argument_nargs_value
        procedure :: produces_list => argument_produces_list
    end type Argument

contains

    subroutine argument_init(self, names, nargs, data_type, action, dest, &
        default_value, const_value, choices, has_default, has_const, &
        has_choices, validator, required, help, &
        metavar, visible, deprecated_msg, removed_msg, print_default, &
        print_choices)
        class(Argument), intent(out) :: self
        character(len=*), intent(in) :: names(:)
        type(NargsSpec), intent(in) :: nargs
        character(len=*), intent(in) :: data_type
        class(ActionType), intent(in) :: action
        character(len=*), intent(in), optional :: dest
        type(ValueBox), intent(in) :: default_value, const_value, choices
        logical, intent(in) :: has_default, has_const, has_choices
        class(ValidatorType), intent(in), optional :: validator
        logical, intent(in) :: required
        character(len=*), intent(in), optional :: help, metavar
        logical, intent(in), optional :: visible
        character(len=*), intent(in), optional :: deprecated_msg, removed_msg
        logical, intent(in), optional :: print_default, print_choices
        integer :: index, name_length

        name_length = maxval(len_trim(names))
        allocate(character(len=name_length) :: self%names(size(names)))
        do index = 1, size(names)
            self%names(index) = trim(names(index))
        end do

        self%is_optional = names_are_optional(names)
        self%nargs = nargs
        self%data_type = trim(data_type)
        allocate(self%action, source=action)
        if (present(validator)) allocate(self%validator, source=validator)

        if (present(dest)) then
            self%dest = trim(dest)
        else
            self%dest = derive_dest_from_names(names)
        end if

        if (has_default) then
            self%default_value = default_value%clone()
            self%has_default = .true.
        end if
        if (has_const) then
            self%const_value = const_value%clone()
            self%has_const = .true.
        end if
        if (has_choices) then
            self%choices = choices%clone()
            self%has_choices = .true.
        end if

        self%required = required
        if (present(help)) self%help = trim(help)
        if (present(metavar)) self%metavar = trim(metavar)
        if (present(visible)) self%visible = visible
        if (present(deprecated_msg)) self%deprecated_msg = trim(deprecated_msg)
        if (present(removed_msg)) self%removed_msg = trim(removed_msg)
        if (present(print_default)) self%print_default = print_default
        if (present(print_choices)) self%print_choices = print_choices
    end subroutine argument_init

    pure integer function argument_name_count(self) result(number)
        class(Argument), intent(in) :: self

        if (allocated(self%names)) then
            number = size(self%names)
        else
            number = 0
        end if
    end function argument_name_count

    pure logical function argument_matches_name(self, name) result(matches)
        class(Argument), intent(in) :: self
        character(len=*), intent(in) :: name
        integer :: index

        matches = .false.
        do index = 1, self%name_count()
            if (trim(self%names(index)) == trim(name)) then
                matches = .true.
                return
            end if
        end do
    end function argument_matches_name

    pure logical function argument_is_positional(self) result(is_positional)
        class(Argument), intent(in) :: self

        is_positional = .not. self%is_optional
    end function argument_is_positional

    function argument_primary_name(self) result(name)
        class(Argument), intent(in) :: self
        character(len=:), allocatable :: name

        if (self%name_count() == 0) then
            name = ""
        else
            name = primary_name_from_names(self%names)
        end if
    end function argument_primary_name

    function argument_derive_dest(self) result(dest)
        class(Argument), intent(in) :: self
        character(len=:), allocatable :: dest

        if (self%name_count() == 0) then
            dest = ""
        else
            dest = derive_dest_from_names(self%names)
        end if
    end function argument_derive_dest

    function argument_effective_metavar(self) result(metavar)
        class(Argument), intent(in) :: self
        character(len=:), allocatable :: metavar
        integer :: index, code

        if (allocated(self%metavar)) then
            metavar = self%metavar
            return
        end if

        if (.not. allocated(self%dest)) then
            metavar = "VALUE"
            return
        end if

        metavar = self%dest
        do index = 1, len(metavar)
            code = iachar(metavar(index:index))
            if (code >= iachar('a') .and. code <= iachar('z')) then
                metavar(index:index) = achar(code + iachar('A') - iachar('a'))
            end if
        end do
    end function argument_effective_metavar

    function argument_nargs_display(self) result(display)
        class(Argument), intent(in) :: self
        character(len=:), allocatable :: display

        display = self%nargs%to_string()
    end function argument_nargs_display

    pure integer function argument_nargs_value(self) result(value)
        class(Argument), intent(in) :: self

        value = self%nargs%value()
    end function argument_nargs_value

    pure logical function argument_produces_list(self) result(produces_list)
        class(Argument), intent(in) :: self

        if (allocated(self%action)) then
            produces_list = self%action%produces_list(self%nargs)
        else
            produces_list = self%nargs%produces_list()
        end if
    end function argument_produces_list

    function derive_dest_from_names(names) result(dest)
        character(len=*), intent(in) :: names(:)
        character(len=:), allocatable :: dest
        character(len=:), allocatable :: primary
        integer :: index, first

        primary = primary_name_from_names(names)
        first = 1
        do while (first <= len_trim(primary) .and. primary(first:first) == '-')
            first = first + 1
        end do
        if (first > len_trim(primary)) then
            dest = ""
            return
        end if

        dest = primary(first:len_trim(primary))
        do index = 1, len(dest)
            if (dest(index:index) == '-') dest(index:index) = '_'
        end do
    end function derive_dest_from_names

    function primary_name_from_names(names) result(name)
        character(len=*), intent(in) :: names(:)
        character(len=:), allocatable :: name
        integer :: index, best, best_length
        logical :: has_long_option

        if (size(names) == 0) then
            name = ""
            return
        end if
        if (.not. names_are_optional(names)) then
            name = trim(names(1))
            return
        end if

        has_long_option = .false.
        do index = 1, size(names)
            if (len_trim(names(index)) > 2 .and. names(index)(1:2) == '--') then
                has_long_option = .true.
                exit
            end if
        end do

        best = 1
        best_length = -1
        do index = 1, size(names)
            if (has_long_option) then
                if (len_trim(names(index)) <= 2) cycle
                if (names(index)(1:2) /= '--') cycle
            end if
            if (len_trim(names(index)) > best_length) then
                best = index
                best_length = len_trim(names(index))
            end if
        end do
        name = trim(names(best))
    end function primary_name_from_names

    pure logical function names_are_optional(names) result(optional_names)
        character(len=*), intent(in) :: names(:)

        optional_names = size(names) > 0 .and. len_trim(names(1)) > 0
        if (optional_names) optional_names = names(1)(1:1) == '-'
    end function names_are_optional

end module fclap_argument
