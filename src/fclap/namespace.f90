!> Typed key/value storage for parsed command-line arguments.
module fclap_namespace
    use fclap_error_codes, only : FCLAP_OK, ERR_NAMESPACE_MISSING_KEY, &
        ERR_NAMESPACE_TYPE_MISMATCH
    use fclap_utils_accuracy, only : ip, wp
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : StringValue, IntegerValue, RealValue, &
        LogicalValue, ListValue, new_value
    implicit none
    private

    public :: Namespace

    type :: NamespaceEntry
        character(len=:), allocatable :: key
        type(ValueBox) :: value
    end type NamespaceEntry

    type :: Namespace
        private
        type(NamespaceEntry), allocatable :: entries(:)
    contains
        procedure :: init  => namespace_clear
        procedure :: clear => namespace_clear
        procedure :: size  => namespace_size
        procedure :: contains => namespace_contains
        procedure :: has_key  => namespace_contains
        procedure :: merge => namespace_merge

        procedure :: set_string       => namespace_set_string
        procedure :: set_integer      => namespace_set_integer
        procedure :: set_real         => namespace_set_real
        procedure :: set_logical      => namespace_set_logical
        procedure :: set_string_list  => namespace_set_string_list
        procedure :: set_integer_list => namespace_set_integer_list
        procedure :: set_real_list    => namespace_set_real_list
        procedure :: set_logical_list => namespace_set_logical_list
        generic :: set => set_string, set_integer, set_real, set_logical, &
            set_string_list, set_integer_list, set_real_list, set_logical_list
        procedure :: set_value => namespace_set_box

        procedure :: append_string  => namespace_append_string
        procedure :: append_integer => namespace_append_integer
        procedure :: append_real    => namespace_append_real
        procedure :: append_logical => namespace_append_logical
        generic :: append => append_string, append_integer, append_real, &
            append_logical
        procedure :: append_value => namespace_append_box

        procedure :: get_string       => namespace_get_string
        procedure :: get_integer      => namespace_get_integer
        procedure :: get_real         => namespace_get_real
        procedure :: get_logical      => namespace_get_logical
        procedure :: get_string_list  => namespace_get_string_list
        procedure :: get_integer_list => namespace_get_integer_list
        procedure :: get_real_list    => namespace_get_real_list
        procedure :: get_logical_list => namespace_get_logical_list
        generic :: get => get_string, get_integer, get_real, get_logical, &
            get_string_list, get_integer_list, get_real_list, get_logical_list

        procedure, private :: find => namespace_find
    end type Namespace

contains

    subroutine namespace_clear(self)
        class(Namespace), intent(inout) :: self

        if (allocated(self%entries)) deallocate(self%entries)
    end subroutine namespace_clear

    pure integer function namespace_size(self) result(number)
        class(Namespace), intent(in) :: self

        if (allocated(self%entries)) then
            number = size(self%entries)
        else
            number = 0
        end if
    end function namespace_size

    pure logical function namespace_contains(self, key) result(found)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key

        found = self%find(key) > 0
    end function namespace_contains

    pure integer function namespace_find(self, key) result(index)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        integer :: current

        index = 0
        do current = 1, self%size()
            if (self%entries(current)%key == trim(key)) then
                index = current
                return
            end if
        end do
    end function namespace_find

    !> Deep-copy entries from another namespace.
    !>
    !> By default an incoming value replaces an existing value with the same
    !> key.  Passing `overwrite=.false.` preserves values already held by
    !> `self`.  ValueBox assignment is deliberately routed through set_value so
    !> polymorphic values are cloned rather than shared.
    subroutine namespace_merge(self, other, overwrite)
        class(Namespace), intent(inout) :: self
        type(Namespace), intent(in) :: other
        logical, intent(in), optional :: overwrite
        logical :: replace
        integer :: index

        replace = .true.
        if (present(overwrite)) replace = overwrite

        do index = 1, other%size()
            if (.not. replace .and. self%contains(other%entries(index)%key)) cycle
            call self%set_value(other%entries(index)%key, &
                other%entries(index)%value)
        end do
    end subroutine namespace_merge

    subroutine namespace_set_box(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        type(ValueBox), intent(in) :: value
        type(NamespaceEntry), allocatable :: temporary(:)
        integer :: index, old_size

        index = self%find(key)
        if (index > 0) then
            self%entries(index)%value = value%clone()
            return
        end if

        old_size = self%size()
        allocate(temporary(old_size + 1))
        if (old_size > 0) temporary(:old_size) = self%entries
        temporary(old_size + 1)%key = trim(key)
        temporary(old_size + 1)%value = value%clone()
        call move_alloc(temporary, self%entries)
    end subroutine namespace_set_box

    subroutine namespace_set_string(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key, value

            call self%set_value(key, new_value(value))
    end subroutine namespace_set_string

    subroutine namespace_set_integer(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer(ip), intent(in) :: value

        call self%set_value(key, new_value(value))
    end subroutine namespace_set_integer

    subroutine namespace_set_real(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        real(wp), intent(in) :: value

        call self%set_value(key, new_value(value))
    end subroutine namespace_set_real

    subroutine namespace_set_logical(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        logical, intent(in) :: value

        call self%set_value(key, new_value(value))
    end subroutine namespace_set_logical

    subroutine namespace_set_string_list(self, key, values)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: values(:)

        call self%set_value(key, new_value(values))
    end subroutine namespace_set_string_list

    subroutine namespace_set_integer_list(self, key, values)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer(ip), intent(in) :: values(:)

        call self%set_value(key, new_value(values))
    end subroutine namespace_set_integer_list

    subroutine namespace_set_real_list(self, key, values)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        real(wp), intent(in) :: values(:)

        call self%set_value(key, new_value(values))
    end subroutine namespace_set_real_list

    subroutine namespace_set_logical_list(self, key, values)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        logical, intent(in) :: values(:)

        call self%set_value(key, new_value(values))
    end subroutine namespace_set_logical_list

    subroutine namespace_append_string(self, key, value, stat)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key, value
        integer, intent(out), optional :: stat

        call namespace_append_box(self, key, new_value(value), stat)
    end subroutine namespace_append_string

    subroutine namespace_append_integer(self, key, value, stat)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer(ip), intent(in) :: value
        integer, intent(out), optional :: stat

        call namespace_append_box(self, key, new_value(value), stat)
    end subroutine namespace_append_integer

    subroutine namespace_append_real(self, key, value, stat)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        real(wp), intent(in) :: value
        integer, intent(out), optional :: stat

        call namespace_append_box(self, key, new_value(value), stat)
    end subroutine namespace_append_real

    subroutine namespace_append_logical(self, key, value, stat)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        logical, intent(in) :: value
        integer, intent(out), optional :: stat

        call namespace_append_box(self, key, new_value(value), stat)
    end subroutine namespace_append_logical

    subroutine namespace_append_box(self, key, value, stat)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        type(ValueBox), intent(in) :: value
        integer, intent(out), optional :: stat
        type(ListValue) :: new_list
        type(ValueBox) :: list_box
        integer :: index

        index = self%find(key)
        if (index == 0) then
            call new_list%append(value)
            call list_box%set(new_list)
            call self%set_value(key, list_box)
            call set_optional_status(stat, FCLAP_OK)
            return
        end if

        select type (list => self%entries(index)%value%item)
        type is (ListValue)
            if (list%item_type() /= "untyped" .and. &
                list%item_type() /= value%type_name()) then
                call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                return
            end if
            call list%append(value)
            call set_optional_status(stat, FCLAP_OK)
        class default
            call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
        end select
    end subroutine namespace_append_box

    subroutine namespace_get_string(self, key, value, stat)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(inout) :: value
        integer, intent(out), optional :: stat
        integer :: index

        if (.not. namespace_lookup(self, key, index, stat)) return
        select type (stored => self%entries(index)%value%item)
        type is (StringValue)
            if (allocated(stored%value)) value = stored%value
            call set_optional_status(stat, FCLAP_OK)
        class default
            call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
        end select
    end subroutine namespace_get_string

    subroutine namespace_get_integer(self, key, value, stat)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        integer(ip), intent(inout) :: value
        integer, intent(out), optional :: stat
        integer :: index

        if (.not. namespace_lookup(self, key, index, stat)) return
        select type (stored => self%entries(index)%value%item)
        type is (IntegerValue)
            value = stored%value
            call set_optional_status(stat, FCLAP_OK)
        class default
            call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
        end select
    end subroutine namespace_get_integer

    subroutine namespace_get_real(self, key, value, stat)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        real(wp), intent(inout) :: value
        integer, intent(out), optional :: stat
        integer :: index

        if (.not. namespace_lookup(self, key, index, stat)) return
        select type (stored => self%entries(index)%value%item)
        type is (RealValue)
            value = stored%value
            call set_optional_status(stat, FCLAP_OK)
        class default
            call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
        end select
    end subroutine namespace_get_real

    subroutine namespace_get_logical(self, key, value, stat)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        logical, intent(inout) :: value
        integer, intent(out), optional :: stat
        integer :: index

        if (.not. namespace_lookup(self, key, index, stat)) return
        select type (stored => self%entries(index)%value%item)
        type is (LogicalValue)
            value = stored%value
            call set_optional_status(stat, FCLAP_OK)
        class default
            call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
        end select
    end subroutine namespace_get_logical

    subroutine namespace_get_string_list(self, key, values, stat)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(inout) :: values(:)
        integer, intent(out), optional :: stat
        character(len=:), allocatable :: temporary(:)
        integer :: index, item_index, max_length

        if (.not. namespace_lookup_list(self, key, index, stat)) return
        select type (list => self%entries(index)%value%item)
        type is (ListValue)
            if (list%item_type() /= "string") then
                call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                return
            end if
            max_length = 0
            do item_index = 1, list%size()
                select type (stored => list%items(item_index)%item)
                type is (StringValue)
                    if (allocated(stored%value)) max_length = max(max_length, len(stored%value))
                class default
                    call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                    return
                end select
            end do
            allocate(character(len=max_length) :: temporary(list%size()))
            do item_index = 1, list%size()
                select type (stored => list%items(item_index)%item)
                type is (StringValue)
                    if (allocated(stored%value)) temporary(item_index) = stored%value
                end select
            end do
            call move_alloc(temporary, values)
            call set_optional_status(stat, FCLAP_OK)
        end select
    end subroutine namespace_get_string_list

    subroutine namespace_get_integer_list(self, key, values, stat)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        integer(ip), allocatable, intent(inout) :: values(:)
        integer, intent(out), optional :: stat
        integer(ip), allocatable :: temporary(:)
        integer :: index, item_index

        if (.not. namespace_lookup_list(self, key, index, stat)) return
        select type (list => self%entries(index)%value%item)
        type is (ListValue)
            if (list%item_type() /= "integer") then
                call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                return
            end if
            allocate(temporary(list%size()))
            do item_index = 1, list%size()
                select type (stored => list%items(item_index)%item)
                type is (IntegerValue)
                    temporary(item_index) = stored%value
                class default
                    call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                    return
                end select
            end do
            call move_alloc(temporary, values)
            call set_optional_status(stat, FCLAP_OK)
        end select
    end subroutine namespace_get_integer_list

    subroutine namespace_get_real_list(self, key, values, stat)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        real(wp), allocatable, intent(inout) :: values(:)
        integer, intent(out), optional :: stat
        real(wp), allocatable :: temporary(:)
        integer :: index, item_index

        if (.not. namespace_lookup_list(self, key, index, stat)) return
        select type (list => self%entries(index)%value%item)
        type is (ListValue)
            if (list%item_type() /= "real") then
                call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                return
            end if
            allocate(temporary(list%size()))
            do item_index = 1, list%size()
                select type (stored => list%items(item_index)%item)
                type is (RealValue)
                    temporary(item_index) = stored%value
                class default
                    call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                    return
                end select
            end do
            call move_alloc(temporary, values)
            call set_optional_status(stat, FCLAP_OK)
        end select
    end subroutine namespace_get_real_list

    subroutine namespace_get_logical_list(self, key, values, stat)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        logical, allocatable, intent(inout) :: values(:)
        integer, intent(out), optional :: stat
        logical, allocatable :: temporary(:)
        integer :: index, item_index

        if (.not. namespace_lookup_list(self, key, index, stat)) return
        select type (list => self%entries(index)%value%item)
        type is (ListValue)
            if (list%item_type() /= "logical") then
                call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                return
            end if
            allocate(temporary(list%size()))
            do item_index = 1, list%size()
                select type (stored => list%items(item_index)%item)
                type is (LogicalValue)
                    temporary(item_index) = stored%value
                class default
                    call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
                    return
                end select
            end do
            call move_alloc(temporary, values)
            call set_optional_status(stat, FCLAP_OK)
        end select
    end subroutine namespace_get_logical_list

    logical function namespace_lookup(self, key, index, stat) result(found)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(out) :: index
        integer, intent(out), optional :: stat

        index = self%find(key)
        found = index > 0
        if (found) then
            call set_optional_status(stat, FCLAP_OK)
        else
            call set_optional_status(stat, ERR_NAMESPACE_MISSING_KEY)
        end if
    end function namespace_lookup

    logical function namespace_lookup_list(self, key, index, stat) result(found)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(out) :: index
        integer, intent(out), optional :: stat

        found = namespace_lookup(self, key, index, stat)
        if (.not. found) return
        select type (stored => self%entries(index)%value%item)
        type is (ListValue)
            found = .true.
        class default
            found = .false.
            call set_optional_status(stat, ERR_NAMESPACE_TYPE_MISMATCH)
        end select
    end function namespace_lookup_list

    subroutine set_optional_status(stat, value)
        integer, intent(out), optional :: stat
        integer, intent(in) :: value

        if (present(stat)) stat = value
    end subroutine set_optional_status

end module fclap_namespace
