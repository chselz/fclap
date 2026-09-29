!> Abstract extension contract for validators operating on converted values.
module fclap_validators_abstract
    use fclap_value_abstract, only : ValueBox
    implicit none
    private

    public :: ValidatorType

    type, abstract :: ValidatorType
    contains
        procedure(validator_validate_interface), deferred :: validate
    end type ValidatorType

    abstract interface
        subroutine validator_validate_interface(self, value, valid, message)
            import :: ValidatorType, ValueBox
            class(ValidatorType), intent(in) :: self
            type(ValueBox), intent(in) :: value
            logical, intent(out) :: valid
            character(len=:), allocatable, intent(out) :: message
        end subroutine validator_validate_interface
    end interface

end module fclap_validators_abstract
