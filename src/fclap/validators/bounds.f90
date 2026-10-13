!> Inclusive lower-bound, upper-bound, and range validators for numbers.
module fclap_validators_bounds
    use fclap_utils_accuracy, only : i8, ip, sp, wp
    use fclap_validators_abstract, only : ValidatorType
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : IntegerValue, RealValue, new_value
    implicit none
    private

    public :: LowerBoundValidator, UpperBoundValidator, RangeValidator
    public :: not_less_than, not_bigger_than, in_range

    type, extends(ValidatorType) :: LowerBoundValidator
        type(ValueBox) :: bound
    contains
        procedure :: validate => lower_bound_validate
        procedure :: accepts_type => lower_bound_accepts_type
    end type LowerBoundValidator

    type, extends(ValidatorType) :: UpperBoundValidator
        type(ValueBox) :: bound
    contains
        procedure :: validate => upper_bound_validate
        procedure :: accepts_type => upper_bound_accepts_type
    end type UpperBoundValidator

    !> Inclusive closed interval `[lower, upper]` of one numeric type
    type, extends(ValidatorType) :: RangeValidator
        type(ValueBox) :: lower
        type(ValueBox) :: upper
    contains
        procedure :: validate => range_validate
        procedure :: accepts_type => range_accepts_type
    end type RangeValidator

    interface not_less_than
        module procedure :: not_less_than_integer
        module procedure :: not_less_than_integer_i8
        module procedure :: not_less_than_real_sp
        module procedure :: not_less_than_real
    end interface not_less_than

    interface not_bigger_than
        module procedure :: not_bigger_than_integer
        module procedure :: not_bigger_than_integer_i8
        module procedure :: not_bigger_than_real_sp
        module procedure :: not_bigger_than_real
    end interface not_bigger_than

    interface in_range
        module procedure :: in_range_integer
        module procedure :: in_range_integer_i8
        module procedure :: in_range_real_sp
        module procedure :: in_range_real
    end interface in_range

contains

    function not_less_than_integer(bound) result(validator)
        integer(ip), intent(in) :: bound
        type(LowerBoundValidator) :: validator

        validator%bound = new_value(bound)
    end function not_less_than_integer

    function not_less_than_integer_i8(bound) result(validator)
        integer(i8), intent(in) :: bound
        type(LowerBoundValidator) :: validator

        validator%bound = new_value(bound)
    end function not_less_than_integer_i8

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

    function not_bigger_than_integer_i8(bound) result(validator)
        integer(i8), intent(in) :: bound
        type(UpperBoundValidator) :: validator

        validator%bound = new_value(bound)
    end function not_bigger_than_integer_i8

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

    function in_range_integer(lower, upper) result(validator)
        integer(ip), intent(in) :: lower, upper
        type(RangeValidator) :: validator

        validator%lower = new_value(lower)
        validator%upper = new_value(upper)
    end function in_range_integer

    function in_range_integer_i8(lower, upper) result(validator)
        integer(i8), intent(in) :: lower, upper
        type(RangeValidator) :: validator

        validator%lower = new_value(lower)
        validator%upper = new_value(upper)
    end function in_range_integer_i8

    function in_range_real(lower, upper) result(validator)
        real(wp), intent(in) :: lower, upper
        type(RangeValidator) :: validator

        validator%lower = new_value(lower)
        validator%upper = new_value(upper)
    end function in_range_real

    function in_range_real_sp(lower, upper) result(validator)
        real(sp), intent(in) :: lower, upper
        type(RangeValidator) :: validator

        validator%lower = new_value(real(lower, kind=wp))
        validator%upper = new_value(real(upper, kind=wp))
    end function in_range_real_sp

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

    subroutine range_validate(self, value, valid, message)
        class(RangeValidator), intent(in) :: self
        type(ValueBox), intent(in) :: value
        logical, intent(out) :: valid
        character(len=:), allocatable, intent(out) :: message

        valid = numeric_at_least(value, self%lower) .and. &
            numeric_at_most(value, self%upper)
        if (valid) then
            message = ""
        else
            message = "value must be between " // self%lower%to_string() // &
                " and " // self%upper%to_string()
        end if
    end subroutine range_validate

    logical function lower_bound_accepts_type(self, type_name) result(accepted)
        class(LowerBoundValidator), intent(in) :: self
        character(len=*), intent(in) :: type_name

        accepted = self%bound%type_name() == type_name
    end function lower_bound_accepts_type

    logical function upper_bound_accepts_type(self, type_name) result(accepted)
        class(UpperBoundValidator), intent(in) :: self
        character(len=*), intent(in) :: type_name

        accepted = self%bound%type_name() == type_name
    end function upper_bound_accepts_type

    !> Both limits must match the argument type and describe a nonempty range.
    logical function range_accepts_type(self, type_name) result(accepted)
        class(RangeValidator), intent(in) :: self
        character(len=*), intent(in) :: type_name

        accepted = self%lower%type_name() == type_name .and. &
            self%upper%type_name() == type_name
        if (accepted) accepted = numeric_at_most(self%lower, self%upper)
    end function range_accepts_type

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
