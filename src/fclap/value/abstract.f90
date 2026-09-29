!> Abstract value contract and polymorphic owning box.
module fclap_value_abstract
    implicit none
    private

    public :: ValueType
    public :: ValueBox

    type, abstract :: ValueType
    contains
        procedure(value_clone_interface), deferred :: clone
        procedure(value_to_string_interface), deferred :: to_string
        procedure(value_type_name_interface), deferred :: type_name
    end type ValueType

    type :: ValueBox
        class(ValueType), allocatable :: item
    contains
        procedure :: set          => value_box_set
        procedure :: clear        => value_box_clear
        procedure :: is_allocated => value_box_is_allocated
        procedure :: clone        => value_box_clone
        procedure :: to_string    => value_box_to_string
        procedure :: type_name    => value_box_type_name
    end type ValueBox

    abstract interface
        function value_clone_interface(self) result(cloned)
            import :: ValueType
            class(ValueType), intent(in) :: self
            class(ValueType), allocatable :: cloned
        end function value_clone_interface

        function value_to_string_interface(self) result(text)
            import :: ValueType
            class(ValueType), intent(in) :: self
            character(len=:), allocatable :: text
        end function value_to_string_interface

        function value_type_name_interface(self) result(name)
            import :: ValueType
            class(ValueType), intent(in) :: self
            character(len=:), allocatable :: name
        end function value_type_name_interface
    end interface

contains

    subroutine value_box_set(self, value)
        class(ValueBox), intent(inout) :: self
        class(ValueType), intent(in) :: value

        if (allocated(self%item)) deallocate(self%item)
        allocate(self%item, source=value)
    end subroutine value_box_set

    subroutine value_box_clear(self)
        class(ValueBox), intent(inout) :: self

        if (allocated(self%item)) deallocate(self%item)
    end subroutine value_box_clear

    pure logical function value_box_is_allocated(self) result(is_allocated)
        class(ValueBox), intent(in) :: self

        is_allocated = allocated(self%item)
    end function value_box_is_allocated

    function value_box_clone(self) result(cloned)
        class(ValueBox), intent(in) :: self
        type(ValueBox) :: cloned

        if (allocated(self%item)) cloned%item = self%item%clone()
    end function value_box_clone

    recursive function value_box_to_string(self) result(text)
        class(ValueBox), intent(in) :: self
        character(len=:), allocatable :: text

        if (allocated(self%item)) then
            text = self%item%to_string()
        else
            text = ""
        end if
    end function value_box_to_string

    function value_box_type_name(self) result(name)
        class(ValueBox), intent(in) :: self
        character(len=:), allocatable :: name

        if (allocated(self%item)) then
            name = self%item%type_name()
        else
            name = "unallocated"
        end if
    end function value_box_type_name

end module fclap_value_abstract
