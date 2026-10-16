!> Built-in scalar and list value implementations.
module fclap_value_builtin
    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_is_nan
    use fclap_utils_accuracy, only : i8, ip, wp
    use fclap_value_abstract, only : ValueType, ValueBox
    implicit none
    private

    public :: StringValue
    public :: IntegerValue
    public :: RealValue
    public :: LogicalValue
    public :: ListValue
    public :: new_value

    type, extends(ValueType) :: StringValue
        character(len=:), allocatable :: value
    contains
        procedure :: clone     => string_value_clone
        procedure :: to_string => string_value_to_string
        procedure :: type_name => string_value_type_name
    end type StringValue

    !> Integer value, stored at 64-bit range for every public integer kind
    type, extends(ValueType) :: IntegerValue
        integer(i8) :: value = 0_i8
    contains
        procedure :: clone     => integer_value_clone
        procedure :: to_string => integer_value_to_string
        procedure :: type_name => integer_value_type_name
    end type IntegerValue

    type, extends(ValueType) :: RealValue
        real(wp) :: value = 0.0_wp
    contains
        procedure :: clone     => real_value_clone
        procedure :: to_string => real_value_to_string
        procedure :: type_name => real_value_type_name
    end type RealValue

    type, extends(ValueType) :: LogicalValue
        logical :: value = .false.
    contains
        procedure :: clone     => logical_value_clone
        procedure :: to_string => logical_value_to_string
        procedure :: type_name => logical_value_type_name
    end type LogicalValue

    type, extends(ValueType) :: ListValue
        type(ValueBox), allocatable :: items(:)
        character(len=:), allocatable, private :: element_type
    contains
        procedure :: clone     => list_value_clone
        procedure :: to_string => list_value_to_string
        procedure :: type_name => list_value_type_name
        procedure :: append    => list_value_append
        procedure :: size      => list_value_size
        procedure :: get       => list_value_get
        procedure :: item_type => list_value_item_type
        procedure :: initialize => list_value_initialize
    end type ListValue

    interface new_value
        module procedure :: new_string_value
        module procedure :: new_integer_value
        module procedure :: new_integer_i8_value
        module procedure :: new_real_value
        module procedure :: new_logical_value
        module procedure :: new_string_list_value
        module procedure :: new_integer_list_value
        module procedure :: new_integer_i8_list_value
        module procedure :: new_real_list_value
        module procedure :: new_logical_list_value
    end interface new_value

contains

    function string_value_clone(self) result(cloned)
        class(StringValue), intent(in) :: self
        class(ValueType), allocatable :: cloned

        allocate(cloned, source=self)
    end function string_value_clone

    function string_value_to_string(self) result(text)
        class(StringValue), intent(in) :: self
        character(len=:), allocatable :: text

        if (allocated(self%value)) then
            text = "'" // self%value // "'"
        else
            text = "''"
        end if
    end function string_value_to_string

    function string_value_type_name(self) result(name)
        class(StringValue), intent(in) :: self
        character(len=:), allocatable :: name

        name = "string"
    end function string_value_type_name

    function integer_value_clone(self) result(cloned)
        class(IntegerValue), intent(in) :: self
        class(ValueType), allocatable :: cloned

        allocate(cloned, source=self)
    end function integer_value_clone

    function integer_value_to_string(self) result(text)
        class(IntegerValue), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=64) :: buffer

        write(buffer, '(i0)') self%value
        text = trim(buffer)
    end function integer_value_to_string

    function integer_value_type_name(self) result(name)
        class(IntegerValue), intent(in) :: self
        character(len=:), allocatable :: name

        name = "integer"
    end function integer_value_type_name

    function real_value_clone(self) result(cloned)
        class(RealValue), intent(in) :: self
        class(ValueType), allocatable :: cloned

        allocate(cloned, source=self)
    end function real_value_clone

    !> Shortest decimal text that reads back to the same value.
    !>
    !> Magnitudes from 1e-3 up to 1e6 use fixed notation, e.g. `0.1` or
    !> `2.5`; others use scientific notation with one leading digit, e.g.
    !> `1e-6` or `2.5e10`. Help and diagnostics then show `0.1` rather than
    !> `0.10000000000000001` and `1e-6` rather than `0.1E-5`.
    function real_value_to_string(self) result(text)
        class(RealValue), intent(in) :: self
        character(len=:), allocatable :: text
        real(wp) :: magnitude

        if (ieee_is_nan(self%value)) then
            text = "nan"
        else if (.not. ieee_is_finite(self%value)) then
            text = "inf"
            if (self%value < 0.0_wp) text = "-inf"
        else
            magnitude = abs(self%value)
            if (magnitude == 0.0_wp .or. (magnitude >= 1.0e-3_wp .and. &
                magnitude < 1.0e6_wp)) then
                text = shortest_fixed(self%value)
            else
                text = shortest_scientific(self%value)
            end if
        end if
    end function real_value_to_string

    !> Fixed notation with the fewest fraction digits that round-trip.
    function shortest_fixed(value) result(text)
        real(wp), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=64) :: buffer
        character(len=16) :: edit
        real(wp) :: back
        integer :: digits, io_status

        do digits = 1, precision(value) + 6
            write(edit, '(a,i0,a)') '(f0.', digits, ')'
            write(buffer, edit) value
            read(buffer, *, iostat=io_status) back
            if (io_status == 0 .and. back == value) exit
        end do
        text = trim(adjustl(buffer))
        ! F0.d may omit the zero before the decimal point, e.g. `.5`.
        if (text(1:1) == ".") then
            text = "0" // text
        else if (text(1:2) == "-.") then
            text = "-0" // text(2:)
        end if
    end function shortest_fixed

    !> Scientific notation with the fewest mantissa digits that round-trip,
    !> written without a `+` sign or leading zeros in the exponent.
    function shortest_scientific(value) result(text)
        real(wp), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=:), allocatable :: mantissa, exponent, sign
        character(len=64) :: buffer
        character(len=16) :: edit
        real(wp) :: back
        integer :: digits, io_status, position

        do digits = 0, precision(value) + 2
            write(edit, '(a,i0,a)') '(es40.', digits, 'e3)'
            write(buffer, edit) value
            read(buffer, *, iostat=io_status) back
            if (io_status == 0 .and. back == value) exit
        end do
        text = trim(adjustl(buffer))
        position = scan(text, "Ee")
        mantissa = text(:position - 1)
        exponent = text(position + 1:)
        if (mantissa(len(mantissa):) == ".") &
            mantissa = mantissa(:len(mantissa) - 1)
        sign = ""
        if (exponent(1:1) == "-") sign = "-"
        if (scan(exponent(1:1), "+-") > 0) exponent = exponent(2:)
        position = verify(exponent, "0")
        if (position == 0) then
            exponent = "0"
        else
            exponent = exponent(position:)
        end if
        text = mantissa // "e" // sign // exponent
    end function shortest_scientific

    function real_value_type_name(self) result(name)
        class(RealValue), intent(in) :: self
        character(len=:), allocatable :: name

        name = "real"
    end function real_value_type_name

    function logical_value_clone(self) result(cloned)
        class(LogicalValue), intent(in) :: self
        class(ValueType), allocatable :: cloned

        allocate(cloned, source=self)
    end function logical_value_clone

    function logical_value_to_string(self) result(text)
        class(LogicalValue), intent(in) :: self
        character(len=:), allocatable :: text

        if (self%value) then
            text = ".true."
        else
            text = ".false."
        end if
    end function logical_value_to_string

    function logical_value_type_name(self) result(name)
        class(LogicalValue), intent(in) :: self
        character(len=:), allocatable :: name

        name = "logical"
    end function logical_value_type_name

    function list_value_clone(self) result(cloned)
        class(ListValue), intent(in) :: self
        class(ValueType), allocatable :: cloned

        allocate(cloned, source=self)
    end function list_value_clone

    function list_value_to_string(self) result(text)
        class(ListValue), intent(in) :: self
        character(len=:), allocatable :: text
        integer :: index

        text = "["
        do index = 1, self%size()
            if (index > 1) text = text // ", "
            text = text // self%items(index)%to_string()
        end do
        text = text // "]"
    end function list_value_to_string

    function list_value_type_name(self) result(name)
        class(ListValue), intent(in) :: self
        character(len=:), allocatable :: name

        name = "list"
    end function list_value_type_name

    function list_value_item_type(self) result(name)
        class(ListValue), intent(in) :: self
        character(len=:), allocatable :: name

        if (allocated(self%element_type)) then
            name = self%element_type
        else
            name = "untyped"
        end if
    end function list_value_item_type

    subroutine list_value_initialize(self, element_type)
        class(ListValue), intent(inout) :: self
        character(len=*), intent(in) :: element_type

        if (self%size() == 0) self%element_type = trim(element_type)
    end subroutine list_value_initialize

    subroutine list_value_append(self, value)
        class(ListValue), intent(inout) :: self
        type(ValueBox), intent(in) :: value
        type(ValueBox), allocatable :: temporary(:)
        integer :: old_size

        if (.not. allocated(self%element_type)) self%element_type = value%type_name()
        if (self%element_type /= value%type_name()) return

        old_size = self%size()
        allocate(temporary(old_size + 1))
        if (old_size > 0) temporary(:old_size) = self%items
        temporary(old_size + 1) = value%clone()
        call move_alloc(temporary, self%items)
    end subroutine list_value_append

    pure integer function list_value_size(self) result(number)
        class(ListValue), intent(in) :: self

        if (allocated(self%items)) then
            number = size(self%items)
        else
            number = 0
        end if
    end function list_value_size

    function list_value_get(self, index) result(value)
        class(ListValue), intent(in) :: self
        integer, intent(in) :: index
        type(ValueBox) :: value

        if (index >= 1 .and. index <= self%size()) value = self%items(index)%clone()
    end function list_value_get

    function new_string_value(value) result(box)
        character(len=*), intent(in) :: value
        type(ValueBox) :: box
        type(StringValue) :: concrete

        concrete%value = value
        call box%set(concrete)
    end function new_string_value

    function new_integer_value(value) result(box)
        integer(ip), intent(in) :: value
        type(ValueBox) :: box

        box = new_integer_i8_value(int(value, kind=i8))
    end function new_integer_value

    function new_integer_i8_value(value) result(box)
        integer(i8), intent(in) :: value
        type(ValueBox) :: box
        type(IntegerValue) :: concrete

        concrete%value = value
        call box%set(concrete)
    end function new_integer_i8_value

    function new_real_value(value) result(box)
        real(wp), intent(in) :: value
        type(ValueBox) :: box
        type(RealValue) :: concrete

        concrete%value = value
        call box%set(concrete)
    end function new_real_value

    function new_logical_value(value) result(box)
        logical, intent(in) :: value
        type(ValueBox) :: box
        type(LogicalValue) :: concrete

        concrete%value = value
        call box%set(concrete)
    end function new_logical_value

    function new_string_list_value(values) result(box)
        character(len=*), intent(in) :: values(:)
        type(ValueBox) :: box, item
        type(ListValue) :: list
        integer :: index

        list%element_type = "string"
        do index = 1, size(values)
            item = new_string_value(values(index))
            call list%append(item)
        end do
        call box%set(list)
    end function new_string_list_value

    function new_integer_list_value(values) result(box)
        integer(ip), intent(in) :: values(:)
        type(ValueBox) :: box

        box = new_integer_i8_list_value(int(values, kind=i8))
    end function new_integer_list_value

    function new_integer_i8_list_value(values) result(box)
        integer(i8), intent(in) :: values(:)
        type(ValueBox) :: box, item
        type(ListValue) :: list
        integer :: index

        list%element_type = "integer"
        do index = 1, size(values)
            item = new_integer_i8_value(values(index))
            call list%append(item)
        end do
        call box%set(list)
    end function new_integer_i8_list_value

    function new_real_list_value(values) result(box)
        real(wp), intent(in) :: values(:)
        type(ValueBox) :: box, item
        type(ListValue) :: list
        integer :: index

        list%element_type = "real"
        do index = 1, size(values)
            item = new_real_value(values(index))
            call list%append(item)
        end do
        call box%set(list)
    end function new_real_list_value

    function new_logical_list_value(values) result(box)
        logical, intent(in) :: values(:)
        type(ValueBox) :: box, item
        type(ListValue) :: list
        integer :: index

        list%element_type = "logical"
        do index = 1, size(values)
            item = new_logical_value(values(index))
            call list%append(item)
        end do
        call box%set(list)
    end function new_logical_list_value

end module fclap_value_builtin
