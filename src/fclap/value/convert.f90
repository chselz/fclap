!> Conversion and comparison helpers shared by registration and parsing.
module fclap_value_convert
    use fclap_utils_accuracy, only : i1, i2, i4, i8, ip, sp, wp
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : StringValue, IntegerValue, RealValue, &
        LogicalValue, ListValue, new_value
    implicit none
    private

    integer, parameter, public :: CONVERT_SUCCESS = 0
    integer, parameter, public :: CONVERT_TYPE_ERROR = 1
    integer, parameter, public :: CONVERT_VALUE_ERROR = 2
    integer, parameter, public :: CONVERT_RANK_ERROR = 3

    public :: normalize_type_name
    public :: convert_value
    public :: value_is_list
    public :: value_matches_type
    public :: value_in_choices
    public :: values_equal

contains

    subroutine normalize_type_name(input, normalized, stat)
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: normalized
        integer, intent(out) :: stat
        character(len=:), allocatable :: lowered

        lowered = lowercase(trim(adjustl(input)))
        select case (lowered)
        case ("string", "character", "char")
            normalized = "string"
            stat = CONVERT_SUCCESS
        case ("integer", "int")
            normalized = "integer"
            stat = CONVERT_SUCCESS
        case ("real", "float", "double")
            normalized = "real"
            stat = CONVERT_SUCCESS
        case ("logical", "bool", "boolean")
            normalized = "logical"
            stat = CONVERT_SUCCESS
        case default
            normalized = ""
            stat = CONVERT_TYPE_ERROR
        end select
    end subroutine normalize_type_name

    subroutine convert_value(input, type_name, value, stat)
        class(*), intent(in) :: input(..)
        character(len=*), intent(in) :: type_name
        type(ValueBox), intent(out) :: value
        integer, intent(out) :: stat
        type(ListValue) :: list
        type(ValueBox) :: item
        integer :: index

        select rank (input)
        rank (0)
            call convert_scalar(input, type_name, value, stat)
        rank (1)
            call list%initialize(type_name)
            do index = 1, size(input)
                call convert_scalar(input(index), type_name, item, stat)
                if (stat /= CONVERT_SUCCESS) return
                call list%append(item)
            end do
            call value%set(list)
            stat = CONVERT_SUCCESS
        rank default
            stat = CONVERT_RANK_ERROR
        end select
    end subroutine convert_value

    subroutine convert_scalar(input, type_name, value, stat)
        class(*), intent(in) :: input
        character(len=*), intent(in) :: type_name
        type(ValueBox), intent(out) :: value
        integer, intent(out) :: stat
        integer(ip) :: integer_value
        real(wp) :: real_value
        logical :: logical_value
        integer :: io_status

        stat = CONVERT_TYPE_ERROR
        select type (input)
        type is (character(len=*))
            select case (type_name)
            case ("string")
                value = new_value(trim(input))
                stat = CONVERT_SUCCESS
            case ("integer")
                read(input, *, iostat=io_status) integer_value
                if (io_status == 0) then
                    value = new_value(integer_value)
                    stat = CONVERT_SUCCESS
                else
                    stat = CONVERT_VALUE_ERROR
                end if
            case ("real")
                read(input, *, iostat=io_status) real_value
                if (io_status == 0) then
                    value = new_value(real_value)
                    stat = CONVERT_SUCCESS
                else
                    stat = CONVERT_VALUE_ERROR
                end if
            case ("logical")
                call parse_logical(input, logical_value, stat)
                if (stat == CONVERT_SUCCESS) value = new_value(logical_value)
            end select
        type is (integer(kind=i1))
            if (type_name == "integer") then
                value = new_value(int(input, kind=ip))
                stat = CONVERT_SUCCESS
            end if
        type is (integer(kind=i2))
            if (type_name == "integer") then
                value = new_value(int(input, kind=ip))
                stat = CONVERT_SUCCESS
            end if
        type is (integer(kind=i4))
            if (type_name == "integer") then
                value = new_value(int(input, kind=ip))
                stat = CONVERT_SUCCESS
            end if
        type is (integer(kind=i8))
            if (type_name == "integer") then
                if (input <= int(huge(integer_value), kind=i8) .and. &
                    input >= -int(huge(integer_value), kind=i8)) then
                    value = new_value(int(input, kind=ip))
                    stat = CONVERT_SUCCESS
                else
                    stat = CONVERT_VALUE_ERROR
                end if
            end if
        type is (real(kind=sp))
            if (type_name == "real") then
                value = new_value(real(input, kind=wp))
                stat = CONVERT_SUCCESS
            end if
        type is (real(kind=wp))
            if (type_name == "real") then
                value = new_value(input)
                stat = CONVERT_SUCCESS
            end if
        type is (logical)
            if (type_name == "logical") then
                value = new_value(input)
                stat = CONVERT_SUCCESS
            end if
        class default
            stat = CONVERT_TYPE_ERROR
        end select
    end subroutine convert_scalar

    subroutine parse_logical(input, value, stat)
        character(len=*), intent(in) :: input
        logical, intent(out) :: value
        integer, intent(out) :: stat

        select case (lowercase(trim(adjustl(input))))
        case ("true", "t", "1", "yes", "on", ".true.")
            value = .true.
            stat = CONVERT_SUCCESS
        case ("false", "f", "0", "no", "off", ".false.")
            value = .false.
            stat = CONVERT_SUCCESS
        case default
            value = .false.
            stat = CONVERT_VALUE_ERROR
        end select
    end subroutine parse_logical

    pure logical function value_is_list(value) result(is_list)
        type(ValueBox), intent(in) :: value

        is_list = .false.
        if (.not. allocated(value%item)) return
        select type (stored => value%item)
        type is (ListValue)
            is_list = .true.
        end select
    end function value_is_list

    logical function value_matches_type(value, type_name) result(matches)
        type(ValueBox), intent(in) :: value
        character(len=*), intent(in) :: type_name

        matches = .false.
        if (.not. allocated(value%item)) return
        select type (stored => value%item)
        type is (ListValue)
            matches = stored%item_type() == type_name
        class default
            matches = value%type_name() == type_name
        end select
    end function value_matches_type

    recursive logical function values_equal(left, right) result(equal)
        type(ValueBox), intent(in) :: left, right
        integer :: index

        equal = .false.
        if (.not. allocated(left%item) .or. .not. allocated(right%item)) return
        if (left%type_name() /= right%type_name()) return

        select type (left_value => left%item)
        type is (StringValue)
            select type (right_value => right%item)
            type is (StringValue)
                equal = left_value%value == right_value%value
            end select
        type is (IntegerValue)
            select type (right_value => right%item)
            type is (IntegerValue)
                equal = left_value%value == right_value%value
            end select
        type is (RealValue)
            select type (right_value => right%item)
            type is (RealValue)
                equal = left_value%value == right_value%value
            end select
        type is (LogicalValue)
            select type (right_value => right%item)
            type is (LogicalValue)
                equal = left_value%value .eqv. right_value%value
            end select
        type is (ListValue)
            select type (right_value => right%item)
            type is (ListValue)
                if (left_value%item_type() /= right_value%item_type()) return
                if (left_value%size() /= right_value%size()) return
                equal = .true.
                do index = 1, left_value%size()
                    if (.not. values_equal(left_value%items(index), &
                        right_value%items(index))) then
                        equal = .false.
                        return
                    end if
                end do
            end select
        end select
    end function values_equal

    logical function value_in_choices(value, choices) result(accepted)
        type(ValueBox), intent(in) :: value, choices
        integer :: value_index, choice_index

        accepted = .false.
        if (.not. allocated(choices%item)) return
        select type (allowed => choices%item)
        type is (ListValue)
            select type (candidate_list => value%item)
            type is (ListValue)
                accepted = .true.
                do value_index = 1, candidate_list%size()
                    if (.not. item_is_allowed(candidate_list%items(value_index), allowed)) then
                        accepted = .false.
                        return
                    end if
                end do
            class default
                do choice_index = 1, allowed%size()
                    if (values_equal(value, allowed%items(choice_index))) then
                        accepted = .true.
                        return
                    end if
                end do
            end select
        end select
    end function value_in_choices

    logical function item_is_allowed(value, choices) result(accepted)
        type(ValueBox), intent(in) :: value
        type(ListValue), intent(in) :: choices
        integer :: index

        accepted = .false.
        do index = 1, choices%size()
            if (values_equal(value, choices%items(index))) then
                accepted = .true.
                return
            end if
        end do
    end function item_is_allowed

    pure function lowercase(input) result(output)
        character(len=*), intent(in) :: input
        character(len=len(input)) :: output
        integer :: index, code

        output = input
        do index = 1, len(input)
            code = iachar(input(index:index))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                output(index:index) = achar(code + iachar('a') - iachar('A'))
            end if
        end do
    end function lowercase

end module fclap_value_convert
