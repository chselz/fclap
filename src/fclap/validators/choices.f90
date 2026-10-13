!> Internal validator used by the dedicated choices= argument metadata.
module fclap_validators_choices
    use fclap_validators_abstract, only : ValidatorType
    use fclap_value_abstract, only : ValueBox
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
            message = "value is not one of the allowed choices"
        end if
    end subroutine choices_validate

end module fclap_validators_choices
