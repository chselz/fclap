!> Internal validator used by the dedicated choices= argument metadata.
module fclap_validators_choices
    use fclap_validators_abstract, only : ValidatorType
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : ListValue
    use fclap_value_convert, only : value_in_choices
    implicit none
    private

    public :: ChoicesValidator
    public :: choices_validator

    type, extends(ValidatorType) :: ChoicesValidator
        type(ValueBox) :: allowed
        !> Compare strings without regard to ASCII letter case
        logical :: ignore_case = .false.
    contains
        procedure :: validate => choices_validate
    end type ChoicesValidator

contains

    function choices_validator(allowed, ignore_case) result(validator)
        type(ValueBox), intent(in) :: allowed
        logical, intent(in), optional :: ignore_case
        type(ChoicesValidator) :: validator

        validator%allowed = allowed%clone()
        if (present(ignore_case)) validator%ignore_case = ignore_case
    end function choices_validator

    subroutine choices_validate(self, value, valid, message)
        class(ChoicesValidator), intent(in) :: self
        type(ValueBox), intent(in) :: value
        logical, intent(out) :: valid
        character(len=:), allocatable, intent(out) :: message

        valid = value_in_choices(value, self%allowed, self%ignore_case)
        if (valid) then
            message = ""
        else
            message = "invalid choice: choose from " // &
                choice_list(self%allowed)
        end if
    end subroutine choices_validate

    !> List the allowed values as help shows them, e.g. `'a', 'b'`.
    !>
    !> A long list is cut after the value that reaches `max_length`
    !> characters and ends with `...`, so one diagnostic stays one line.
    function choice_list(allowed) result(text)
        type(ValueBox), intent(in) :: allowed
        character(len=:), allocatable :: text
        integer, parameter :: max_length = 60
        integer :: index

        text = ""
        if (.not. allocated(allowed%item)) return
        select type (list => allowed%item)
        type is (ListValue)
            do index = 1, list%size()
                if (index > 1) text = text // ", "
                if (index > 1 .and. len(text) >= max_length) then
                    text = text // "..."
                    return
                end if
                text = text // list%items(index)%to_string()
            end do
        class default
            text = allowed%to_string()
        end select
    end function choice_list

end module fclap_validators_choices
