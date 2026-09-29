!> Inclusive lower- and upper-bound validators for numeric values.
module fclap_validators_bounds
    use fclap_utils_accuracy, only : ip, sp, wp
    use fclap_validators_abstract, only : ValidatorType
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : IntegerValue, RealValue, new_value
    implicit none
    private

    public :: LowerBoundValidator, UpperBoundValidator
    public :: not_less_than, not_bigger_than

    type, extends(ValidatorType) :: LowerBoundValidator
        type(ValueBox) :: bound
    contains
        procedure :: validate => lower_bound_validate
    end type LowerBoundValidator

    type, extends(ValidatorType) :: UpperBoundValidator
        type(ValueBox) :: bound
    contains
        procedure :: validate => upper_bound_validate
    end type UpperBoundValidator

    interface not_less_than
        module procedure :: not_less_than_integer
        module procedure :: not_less_than_real_sp
        module procedure :: not_less_than_real
    end interface not_less_than

    interface not_bigger_than
        module procedure :: not_bigger_than_integer
        module procedure :: not_bigger_than_real_sp
        module procedure :: not_bigger_than_real
    end interface not_bigger_than

contains

    function not_less_than_integer(bound) result(validator)
        integer(ip), intent(in) :: bound
        type(LowerBoundValidator) :: validator

        validator%bound = new_value(bound)
    end function not_less_than_integer

    function not_less_than_real(bound) result(validator)
        real(wp), intent(in) :: bound
        type(LowerBoundValidator) :: validator

        validator%bound = new_value(bound)
    end function not_less_than_real

    function not_less_than_real_sp(bound) result(validator)
        real(sp), intent(in) :: bound
        type(LowerBoundValidator) :: validator

        validator%bound = new_value(real(bound, kind=wp))
    end function not_less_than_real_sp

    function not_bigger_than_integer(bound) result(validator)
        integer(ip), intent(in) :: bound
        type(UpperBoundValidator) :: validator

        validator%bound = new_value(bound)
    end function not_bigger_than_integer

    function not_bigger_than_real(bound) result(validator)
        real(wp), intent(in) :: bound
        type(UpperBoundValidator) :: validator

        validator%bound = new_value(bound)
    end function not_bigger_than_real

    function not_bigger_than_real_sp(bound) result(validator)
        real(sp), intent(in) :: bound
        type(UpperBoundValidator) :: validator

        validator%bound = new_value(real(bound, kind=wp))
    end function not_bigger_than_real_sp

    subroutine lower_bound_validate(self, value, valid, message)
        class(LowerBoundValidator), intent(in) :: self
        type(ValueBox), intent(in) :: value
        logical, intent(out) :: valid
        character(len=:), allocatable, intent(out) :: message

        valid = numeric_at_least(value, self%bound)
        if (valid) then
            message = ""
        else
            message = "value must not be less than " // self%bound%to_string()
        end if
    end subroutine lower_bound_validate

    subroutine upper_bound_validate(self, value, valid, message)
        class(UpperBoundValidator), intent(in) :: self
        type(ValueBox), intent(in) :: value
        logical, intent(out) :: valid
        character(len=:), allocatable, intent(out) :: message

        valid = numeric_at_most(value, self%bound)
        if (valid) then
            message = ""
        else
            message = "value must not be bigger than " // self%bound%to_string()
        end if
    end subroutine upper_bound_validate

    logical function numeric_at_least(value, bound) result(valid)
        type(ValueBox), intent(in) :: value, bound

        valid = .false.
        select type (candidate => value%item)
        type is (IntegerValue)
            select type (limit => bound%item)
            type is (IntegerValue)
                valid = candidate%value >= limit%value
            end select
        type is (RealValue)
            select type (limit => bound%item)
            type is (RealValue)
                valid = candidate%value >= limit%value
            end select
        end select
    end function numeric_at_least

    logical function numeric_at_most(value, bound) result(valid)
        type(ValueBox), intent(in) :: value, bound

        valid = .false.
        select type (candidate => value%item)
        type is (IntegerValue)
            select type (limit => bound%item)
            type is (IntegerValue)
                valid = candidate%value <= limit%value
            end select
        type is (RealValue)
            select type (limit => bound%item)
            type is (RealValue)
                valid = candidate%value <= limit%value
            end select
        end select
    end function numeric_at_most

end module fclap_validators_bounds
