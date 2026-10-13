!> Abstract extension contract for validators operating on converted values.
module fclap_validators_abstract
    use fclap_value_abstract, only : ValueBox
    implicit none
    private

    public :: ValidatorType

    type, abstract :: ValidatorType
    contains
        procedure(validator_validate_interface), deferred :: validate
        !> Report whether values of a canonical argument type can be validated
        procedure :: accepts_type => validator_accepts_type
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

contains

    !> Accept every argument type unless a concrete validator narrows it.
    !>
    !> Registration calls this with the canonical type name (`string`,
    !> `integer`, `real`, or `logical`) and rejects the definition when the
    !> result is false, so a mismatch is reported once as a configuration
    !> error instead of rejecting every parsed value.
    logical function validator_accepts_type(self, type_name) result(accepted)
        class(ValidatorType), intent(in) :: self
        character(len=*), intent(in) :: type_name

        accepted = .true.
    end function validator_accepts_type

end module fclap_validators_abstract
