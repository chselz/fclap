!> Conversion and comparison helpers shared by registration and parsing.
module fclap_value_convert
    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
    use fclap_utils_accuracy, only : i1, i2, i4, i8, sp, wp
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
    public :: canonicalize_choice
    public :: has_ambiguous_choices
    public :: lowercase

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

    subroutine convert_value(input, type_name, value, stat, verbatim)
        class(*), intent(in) :: input(..)
        character(len=*), intent(in) :: type_name
        type(ValueBox), intent(out) :: value
        integer, intent(out) :: stat
        !> Keep trailing blanks of string input, which is exact-length text
        !> such as a command-line token rather than blank-padded storage
        logical, intent(in), optional :: verbatim
        type(ListValue) :: list
        type(ValueBox) :: item
        integer :: index

        select rank (input)
        rank (0)
            call convert_scalar(input, type_name, value, stat, verbatim)
        rank (1)
            call list%initialize(type_name)
            do index = 1, size(input)
                call convert_scalar(input(index), type_name, item, stat, &
                    verbatim)
                if (stat /= CONVERT_SUCCESS) return
                call list%append(item)
            end do
            call value%set(list)
            stat = CONVERT_SUCCESS
        rank default
            stat = CONVERT_RANK_ERROR
        end select
    end subroutine convert_value

    subroutine convert_scalar(input, type_name, value, stat, verbatim)
        class(*), intent(in) :: input
        character(len=*), intent(in) :: type_name
        type(ValueBox), intent(out) :: value
        integer, intent(out) :: stat
        logical, intent(in), optional :: verbatim
        integer(i8) :: integer_value
        real(wp) :: real_value
        logical :: logical_value

        stat = CONVERT_TYPE_ERROR
        select type (input)
        type is (character(len=*))
            select case (type_name)
            case ("string")
                value = new_value(trim(input))
                if (present(verbatim)) then
                    if (verbatim) value = new_value(input)
                end if
                stat = CONVERT_SUCCESS
            case ("integer")
                call parse_integer(input, integer_value, stat)
                if (stat == CONVERT_SUCCESS) value = new_value(integer_value)
            case ("real")
                call parse_real(input, real_value, stat)
                if (stat == CONVERT_SUCCESS) value = new_value(real_value)
            case ("logical")
                call parse_logical(input, logical_value, stat)
                if (stat == CONVERT_SUCCESS) value = new_value(logical_value)
            case default
                ! Unknown type names keep CONVERT_TYPE_ERROR.
            end select
        type is (integer(kind=i1))
            if (type_name == "integer") then
                value = new_value(int(input, kind=i8))
                stat = CONVERT_SUCCESS
            end if
        type is (integer(kind=i2))
            if (type_name == "integer") then
                value = new_value(int(input, kind=i8))
                stat = CONVERT_SUCCESS
            end if
        type is (integer(kind=i4))
            if (type_name == "integer") then
                value = new_value(int(input, kind=i8))
                stat = CONVERT_SUCCESS
            end if
        type is (integer(kind=i8))
            if (type_name == "integer") then
                value = new_value(input)
                stat = CONVERT_SUCCESS
            end if
        type is (real(kind=sp))
            if (type_name == "real") then
                value = new_value(input)
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

    !> Convert a strict decimal integer literal: an optional sign and digits.
    !>
    !> List-directed input is deliberately avoided because it accepts value
    !> separators, repeat counts, and null values that are not numbers.
    subroutine parse_integer(input, value, stat)
        character(len=*), intent(in) :: input
        integer(i8), intent(out) :: value
        integer, intent(out) :: stat
        character(len=:), allocatable :: text
        integer :: io_status

        value = 0_i8
        stat = CONVERT_VALUE_ERROR
        text = trim(adjustl(input))
        if (.not. is_integer_literal(text)) return
        read(text, *, iostat=io_status) value
        if (io_status /= 0) then
            value = 0_i8
            return
        end if
        stat = CONVERT_SUCCESS
    end subroutine parse_integer

    !> Convert a strict real literal with optional fraction and exponent.
    !>
    !> Accepted forms are decimal literals using an optional `e`, `E`, `d`, or
    !> `D` exponent, and the case-insensitive special values `inf`,
    !> `infinity`, and `nan`. A decimal literal beyond the real range, such
    !> as `1e400`, is rejected rather than read as an infinity; one that
    !> underflows reads as zero.
    subroutine parse_real(input, value, stat)
        character(len=*), intent(in) :: input
        real(wp), intent(out) :: value
        integer, intent(out) :: stat
        character(len=:), allocatable :: text
        integer :: io_status

        value = 0.0_wp
        stat = CONVERT_VALUE_ERROR
        text = trim(adjustl(input))
        if (.not. is_real_literal(text)) return
        read(text, *, iostat=io_status) value
        if (io_status == 0 .and. .not. ieee_is_finite(value)) then
            if (.not. is_special_real(text)) io_status = 1
        end if
        if (io_status /= 0) then
            value = 0.0_wp
            return
        end if
        stat = CONVERT_SUCCESS
    end subroutine parse_real

    pure logical function is_integer_literal(text) result(valid)
        character(len=*), intent(in) :: text
        integer :: first

        valid = .false.
        if (len(text) == 0) return
        first = 1
        if (text(1:1) == "+" .or. text(1:1) == "-") first = 2
        if (first > len(text)) return
        valid = verify(text(first:), "0123456789") == 0
    end function is_integer_literal

    !> Report whether the text spells `inf`, `infinity`, or `nan`, in any
    !> letter case and with an optional sign.
    pure logical function is_special_real(text) result(special)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: body
        integer :: first

        special = .false.
        if (len(text) == 0) return
        first = 1
        if (text(1:1) == "+" .or. text(1:1) == "-") first = 2
        if (first > len(text)) return
        body = lowercase(text(first:))
        special = body == "inf" .or. body == "infinity" .or. body == "nan"
    end function is_special_real

    pure logical function is_real_literal(text) result(valid)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: body
        integer :: first, position, digits, fraction_digits, exponent_digits

        valid = .false.
        if (len(text) == 0) return
        if (is_special_real(text)) then
            valid = .true.
            return
        end if
        first = 1
        if (text(1:1) == "+" .or. text(1:1) == "-") first = 2
        if (first > len(text)) return
        body = lowercase(text(first:))

        position = 1
        call skip_digits(body, position, digits)
        if (position <= len(body)) then
            if (body(position:position) == ".") then
                position = position + 1
                call skip_digits(body, position, fraction_digits)
                digits = digits + fraction_digits
            end if
        end if
        if (digits == 0) return

        if (position <= len(body)) then
            if (body(position:position) /= "e" .and. &
                body(position:position) /= "d") return
            position = position + 1
            if (position <= len(body)) then
                if (body(position:position) == "+" .or. &
                    body(position:position) == "-") position = position + 1
            end if
            call skip_digits(body, position, exponent_digits)
            if (exponent_digits == 0) return
        end if
        valid = position > len(body)
    end function is_real_literal

    !> Advance past consecutive decimal digits and report how many there were.
    pure subroutine skip_digits(text, position, number)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: position
        integer, intent(out) :: number

        number = 0
        do while (position <= len(text))
            if (verify(text(position:position), "0123456789") /= 0) exit
            number = number + 1
            position = position + 1
        end do
    end subroutine skip_digits

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

    !> Compare two values; `ignore_case` makes string comparison ASCII
    !> case-insensitive and has no effect on other types.
    recursive logical function values_equal(left, right, ignore_case) &
        result(equal)
        type(ValueBox), intent(in) :: left, right
        logical, intent(in), optional :: ignore_case
        logical :: fold
        integer :: index

        equal = .false.
        fold = .false.
        if (present(ignore_case)) fold = ignore_case
        if (.not. allocated(left%item) .or. .not. allocated(right%item)) return
        if (left%type_name() /= right%type_name()) return

        select type (left_value => left%item)
        type is (StringValue)
            select type (right_value => right%item)
            type is (StringValue)
                if (fold) then
                    equal = lowercase(left_value%value) == &
                        lowercase(right_value%value)
                else
                    equal = left_value%value == right_value%value
                end if
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
                        right_value%items(index), fold)) then
                        equal = .false.
                        return
                    end if
                end do
            end select
        end select
    end function values_equal

    logical function value_in_choices(value, choices, ignore_case) &
        result(accepted)
        type(ValueBox), intent(in) :: value, choices
        logical, intent(in), optional :: ignore_case
        integer :: value_index

        accepted = .false.
        if (.not. allocated(choices%item)) return
        if (.not. allocated(value%item)) return
        select type (allowed => choices%item)
        type is (ListValue)
            select type (candidate_list => value%item)
            type is (ListValue)
                accepted = .true.
                do value_index = 1, candidate_list%size()
                    if (choice_index(candidate_list%items(value_index), &
                        allowed, ignore_case) == 0) then
                        accepted = .false.
                        return
                    end if
                end do
            class default
                accepted = choice_index(value, allowed, ignore_case) > 0
            end select
        end select
    end function value_in_choices

    !> Replace an accepted value, or every item of an accepted list, by the
    !> declared choice it matches, so case-insensitive input is stored with
    !> the spelling the program registered.
    subroutine canonicalize_choice(value, choices, ignore_case)
        type(ValueBox), intent(inout) :: value
        type(ValueBox), intent(in) :: choices
        logical, intent(in), optional :: ignore_case
        type(ListValue) :: canonical
        integer :: index, match

        if (.not. allocated(choices%item)) return
        if (.not. allocated(value%item)) return
        select type (allowed => choices%item)
        type is (ListValue)
            select type (candidate_list => value%item)
            type is (ListValue)
                call canonical%initialize(candidate_list%item_type())
                do index = 1, candidate_list%size()
                    match = choice_index(candidate_list%items(index), &
                        allowed, ignore_case)
                    if (match > 0) then
                        call canonical%append(allowed%items(match))
                    else
                        call canonical%append(candidate_list%items(index))
                    end if
                end do
                call value%set(canonical)
            class default
                match = choice_index(value, allowed, ignore_case)
                if (match > 0) value = allowed%items(match)%clone()
            end select
        end select
    end subroutine canonicalize_choice

    !> Report whether two declared choices would match the same input.
    logical function has_ambiguous_choices(choices, ignore_case) &
        result(ambiguous)
        type(ValueBox), intent(in) :: choices
        logical, intent(in), optional :: ignore_case
        integer :: left, right

        ambiguous = .false.
        if (.not. allocated(choices%item)) return
        select type (allowed => choices%item)
        type is (ListValue)
            do left = 1, allowed%size() - 1
                do right = left + 1, allowed%size()
                    if (values_equal(allowed%items(left), &
                        allowed%items(right), ignore_case)) then
                        ambiguous = .true.
                        return
                    end if
                end do
            end do
        end select
    end function has_ambiguous_choices

    integer function choice_index(value, choices, ignore_case) result(match)
        type(ValueBox), intent(in) :: value
        type(ListValue), intent(in) :: choices
        logical, intent(in), optional :: ignore_case
        integer :: index

        match = 0
        do index = 1, choices%size()
            if (values_equal(value, choices%items(index), ignore_case)) then
                match = index
                return
            end if
        end do
    end function choice_index

    elemental function lowercase(input) result(output)
        character(len=*), intent(in) :: input
        character(len=len(input)) :: output
        integer :: index, code

        output = input
        do index = 1, len(input)
            code = iachar(input(index:index))
            if (code >= iachar("A") .and. code <= iachar("Z")) then
                output(index:index) = achar(code + iachar("a") - iachar("A"))
            end if
        end do
    end function lowercase

end module fclap_value_convert
