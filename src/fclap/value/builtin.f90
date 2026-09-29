!> Built-in scalar and list value implementations.
module fclap_value_builtin
    use fclap_utils_accuracy, only : ip, wp
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

    type, extends(ValueType) :: IntegerValue
        integer(ip) :: value = 0_ip
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
        module procedure :: new_real_value
        module procedure :: new_logical_value
        module procedure :: new_string_list_value
        module procedure :: new_integer_list_value
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

    function real_value_to_string(self) result(text)
        class(RealValue), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=128) :: buffer

        write(buffer, '(g0)') self%value
        text = trim(buffer)
    end function real_value_to_string

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
        type(IntegerValue) :: concrete

        concrete%value = value
        call box%set(concrete)
    end function new_integer_value

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
        type(ValueBox) :: box, item
        type(ListValue) :: list
        integer :: index

        list%element_type = "integer"
        do index = 1, size(values)
            item = new_integer_value(values(index))
            call list%append(item)
        end do
        call box%set(list)
    end function new_integer_list_value

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
