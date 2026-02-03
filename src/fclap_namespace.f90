!> Namespace module for fclap
!>
!> This module defines the Namespace type (similar to Python's argparse.Namespace)
!> which stores the results of parsing command-line arguments.
!> It provides getter methods for retrieving values by key.

module fclap_namespace
    use fclap_constants, only: MAX_ACTIONS, MAX_ARG_LEN, MAX_LIST_VALUES, &
        TYPE_STRING, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL
    implicit none
    private

    ! ============================================================================
    ! VALUE CONTAINER TYPE
    ! ============================================================================

    !> Container for storing values of different types
    !> Supports single values and lists for append actions
    type, public :: ValueContainer
        !> The type of value stored (TYPE_STRING, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL)
        integer :: value_type = TYPE_STRING
        !> Storage for string values
        character(len=:), allocatable :: string_value
        !> Storage for integer values
        integer :: integer_value = 0
        !> Storage for real/float values
        real :: real_value = 0.0
        !> Storage for logical/boolean values
        logical :: logical_value = .false.
        !> Array storage for string list values (used with append action)
        character(len=MAX_ARG_LEN) :: string_list(MAX_LIST_VALUES)
        !> Array storage for integer list values (used with append action)
        integer :: integer_list(MAX_LIST_VALUES)
        !> Array storage for real list values (used with append action)
        real :: real_list(MAX_LIST_VALUES)
        !> Number of items currently stored in list arrays
        integer :: list_count = 0
        !> Flag indicating whether a value has been explicitly set
        logical :: is_set = .false.
    contains
        procedure :: set_string => value_set_string
        procedure :: set_integer => value_set_integer
        procedure :: set_real => value_set_real
        procedure :: set_logical => value_set_logical
        procedure :: append_string => value_append_string
        procedure :: append_integer => value_append_integer
        procedure :: append_real => value_append_real
        procedure :: to_string => value_to_string
    end type ValueContainer

    ! ============================================================================
    ! ARGUMENT ENTRY TYPE
    ! ============================================================================

    !> Entry in the namespace storing a key-value pair
    type, public :: ArgumentEntry
        !> The argument destination name (key for lookup)
        character(len=:), allocatable :: key
        !> The value container holding the argument's value
        type(ValueContainer) :: value
    end type ArgumentEntry

    ! ============================================================================
    ! NAMESPACE TYPE
    ! ============================================================================

    !> Namespace type to store parsed arguments
    !> Similar to Python's argparse.Namespace
    !> Provides dictionary-like access to parsed argument values
    type, public :: Namespace
        !> Array of argument entries storing all parsed key-value pairs
        type(ArgumentEntry), allocatable :: entries(:)
        !> Current number of entries stored in the entries array
        integer :: num_entries = 0
    contains
        procedure :: init => namespace_init
        procedure :: set_string => namespace_set_string
        procedure :: set_integer => namespace_set_integer
        procedure :: set_real => namespace_set_real
        procedure :: set_logical => namespace_set_logical
        procedure :: append_string => namespace_append_string
        procedure :: append_integer => namespace_append_integer
        procedure :: increment => namespace_increment
        procedure :: get_string => namespace_get_string
        procedure :: get_integer => namespace_get_integer
        procedure :: get_real => namespace_get_real
        procedure :: get_logical => namespace_get_logical
        procedure :: get_string_list => namespace_get_string_list
        procedure :: get_integer_list => namespace_get_integer_list
        procedure :: has_key => namespace_has_key
        procedure :: show => namespace_show
        procedure, private :: find_or_create => namespace_find_or_create
        procedure, private :: find => namespace_find
    end type Namespace

contains

    ! ============================================================================
    ! VALUE CONTAINER IMPLEMENTATIONS
    ! ============================================================================

    subroutine value_set_string(self, val)
        class(ValueContainer), intent(inout) :: self
        character(len=*), intent(in) :: val

        self%string_value = trim(val)
        self%value_type = TYPE_STRING
        self%is_set = .true.
    end subroutine value_set_string

    subroutine value_set_integer(self, val)
        class(ValueContainer), intent(inout) :: self
        integer, intent(in) :: val

        self%integer_value = val
        self%value_type = TYPE_INTEGER
        self%is_set = .true.
    end subroutine value_set_integer

    subroutine value_set_real(self, val)
        class(ValueContainer), intent(inout) :: self
        real, intent(in) :: val

        self%real_value = val
        self%value_type = TYPE_REAL
        self%is_set = .true.
    end subroutine value_set_real

    subroutine value_set_logical(self, val)
        class(ValueContainer), intent(inout) :: self
        logical, intent(in) :: val

        self%logical_value = val
        self%value_type = TYPE_LOGICAL
        self%is_set = .true.
    end subroutine value_set_logical

    subroutine value_append_string(self, val)
        class(ValueContainer), intent(inout) :: self
        character(len=*), intent(in) :: val

        if (self%list_count < MAX_LIST_VALUES) then
            self%list_count = self%list_count + 1
            self%string_list(self%list_count) = trim(val)
            self%value_type = TYPE_STRING
            self%is_set = .true.
        end if
    end subroutine value_append_string

    subroutine value_append_integer(self, val)
        class(ValueContainer), intent(inout) :: self
        integer, intent(in) :: val

        if (self%list_count < MAX_LIST_VALUES) then
            self%list_count = self%list_count + 1
            self%integer_list(self%list_count) = val
            self%value_type = TYPE_INTEGER
            self%is_set = .true.
        end if
    end subroutine value_append_integer

    subroutine value_append_real(self, val)
        class(ValueContainer), intent(inout) :: self
        real, intent(in) :: val

        if (self%list_count < MAX_LIST_VALUES) then
            self%list_count = self%list_count + 1
            self%real_list(self%list_count) = val
            self%value_type = TYPE_REAL
            self%is_set = .true.
        end if
    end subroutine value_append_real

    function value_to_string(self) result(str)
        class(ValueContainer), intent(in) :: self
        character(len=:), allocatable :: str
        character(len=32) :: tmp
        integer :: i

        if (.not. self%is_set) then
            str = "None"
            return
        end if

        if (self%list_count > 0) then
            str = "["
            do i = 1, self%list_count
                if (i > 1) str = str // ", "
                select case(self%value_type)
                case(TYPE_STRING)
                    str = str // "'" // trim(self%string_list(i)) // "'"
                case(TYPE_INTEGER)
                    write(tmp, '(I0)') self%integer_list(i)
                    str = str // trim(tmp)
                case(TYPE_REAL)
                    write(tmp, '(G0)') self%real_list(i)
                    str = str // trim(tmp)
                end select
            end do
            str = str // "]"
        else
            select case(self%value_type)
            case(TYPE_STRING)
                if (allocated(self%string_value)) then
                    str = "'" // trim(self%string_value) // "'"
                else
                    str = "None"
                end if
            case(TYPE_INTEGER)
                write(tmp, '(I0)') self%integer_value
                str = trim(tmp)
            case(TYPE_REAL)
                write(tmp, '(G0)') self%real_value
                str = trim(tmp)
            case(TYPE_LOGICAL)
                if (self%logical_value) then
                    str = "True"
                else
                    str = "False"
                end if
            case default
                str = "None"
            end select
        end if
    end function value_to_string

    ! ============================================================================
    ! NAMESPACE IMPLEMENTATIONS
    ! ============================================================================

    subroutine namespace_init(self)
        class(Namespace), intent(inout) :: self

        if (allocated(self%entries)) deallocate(self%entries)
        allocate(self%entries(MAX_ACTIONS))
        self%num_entries = 0
    end subroutine namespace_init

    function namespace_find_or_create(self, key) result(idx)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer :: idx
        integer :: i

        do i = 1, self%num_entries
            if (allocated(self%entries(i)%key)) then
                if (self%entries(i)%key == key) then
                    idx = i
                    return
                end if
            end if
        end do

        self%num_entries = self%num_entries + 1
        idx = self%num_entries
        self%entries(idx)%key = trim(key)
    end function namespace_find_or_create

    function namespace_find(self, key) result(idx)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        integer :: idx
        integer :: i

        idx = 0
        do i = 1, self%num_entries
            if (allocated(self%entries(i)%key)) then
                if (self%entries(i)%key == key) then
                    idx = i
                    return
                end if
            end if
        end do
    end function namespace_find

    subroutine namespace_set_string(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key, value
        integer :: idx

        idx = self%find_or_create(key)
        call self%entries(idx)%value%set_string(value)
    end subroutine namespace_set_string

    subroutine namespace_set_integer(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        integer :: idx

        idx = self%find_or_create(key)
        call self%entries(idx)%value%set_integer(value)
    end subroutine namespace_set_integer

    subroutine namespace_set_real(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        real, intent(in) :: value
        integer :: idx

        idx = self%find_or_create(key)
        call self%entries(idx)%value%set_real(value)
    end subroutine namespace_set_real

    subroutine namespace_set_logical(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        logical, intent(in) :: value
        integer :: idx

        idx = self%find_or_create(key)
        call self%entries(idx)%value%set_logical(value)
    end subroutine namespace_set_logical

    subroutine namespace_append_string(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key, value
        integer :: idx

        idx = self%find_or_create(key)
        call self%entries(idx)%value%append_string(value)
    end subroutine namespace_append_string

    subroutine namespace_append_integer(self, key, value)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        integer :: idx

        idx = self%find_or_create(key)
        call self%entries(idx)%value%append_integer(value)
    end subroutine namespace_append_integer

    subroutine namespace_increment(self, key)
        class(Namespace), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer :: idx

        idx = self%find_or_create(key)
        if (.not. self%entries(idx)%value%is_set) then
            self%entries(idx)%value%integer_value = 0
        end if
        self%entries(idx)%value%integer_value = self%entries(idx)%value%integer_value + 1
        self%entries(idx)%value%value_type = TYPE_INTEGER
        self%entries(idx)%value%is_set = .true.
    end subroutine namespace_increment

    function namespace_get_string(self, key, default) result(value)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(in), optional :: default
        character(len=:), allocatable :: value
        integer :: idx
        logical :: found

        found = .false.
        idx = self%find(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                found = .true.
                if (allocated(self%entries(idx)%value%string_value)) then
                    value = self%entries(idx)%value%string_value
                else if (present(default)) then
                    value = default
                else
                    value = ""
                end if
            end if
        end if
        if (.not. found) then
            if (present(default)) then
                value = default
            else
                value = ""
            end if
        end if
    end function namespace_get_string

    function namespace_get_integer(self, key, default) result(value)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(in), optional :: default
        integer :: value
        integer :: idx
        logical :: found

        found = .false.
        idx = self%find(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                found = .true.
                value = self%entries(idx)%value%integer_value
            end if
        end if
        if (.not. found) then
            if (present(default)) then
                value = default
            else
                value = 0
            end if
        end if
    end function namespace_get_integer

    function namespace_get_real(self, key, default) result(value)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        real, intent(in), optional :: default
        real :: value
        integer :: idx
        logical :: found

        found = .false.
        idx = self%find(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                found = .true.
                value = self%entries(idx)%value%real_value
            end if
        end if
        if (.not. found) then
            if (present(default)) then
                value = default
            else
                value = 0.0
            end if
        end if
    end function namespace_get_real

    function namespace_get_logical(self, key, default) result(value)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        logical, intent(in), optional :: default
        logical :: value
        integer :: idx
        logical :: found

        found = .false.
        idx = self%find(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                found = .true.
                value = self%entries(idx)%value%logical_value
            end if
        end if
        if (.not. found) then
            if (present(default)) then
                value = default
            else
                value = .false.
            end if
        end if
    end function namespace_get_logical

    subroutine namespace_get_string_list(self, key, values, count)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(out) :: values(:)
        integer, intent(out) :: count
        integer :: idx, i

        count = 0
        idx = self%find(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                count = min(self%entries(idx)%value%list_count, size(values))
                do i = 1, count
                    values(i) = self%entries(idx)%value%string_list(i)
                end do
            end if
        end if
    end subroutine namespace_get_string_list

    subroutine namespace_get_integer_list(self, key, values, count)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(out) :: values(:)
        integer, intent(out) :: count
        integer :: idx, i

        count = 0
        idx = self%find(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                count = min(self%entries(idx)%value%list_count, size(values))
                do i = 1, count
                    values(i) = self%entries(idx)%value%integer_list(i)
                end do
            end if
        end if
    end subroutine namespace_get_integer_list

    function namespace_has_key(self, key) result(exists)
        class(Namespace), intent(in) :: self
        character(len=*), intent(in) :: key
        logical :: exists
        integer :: idx

        idx = self%find(key)
        exists = .false.
        if (idx > 0) then
            exists = self%entries(idx)%value%is_set
        end if
    end function namespace_has_key

    !> Print namespace in format: Namespace(key1=value1, key2=value2, ...)
    subroutine namespace_show(self, unit)
        class(Namespace), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: out_unit, i
        character(len=:), allocatable :: val_str

        out_unit = 6  ! stdout
        if (present(unit)) out_unit = unit

        write(out_unit, '(A)', advance='no') "Namespace("
        do i = 1, self%num_entries
            if (i > 1) write(out_unit, '(A)', advance='no') ", "
            val_str = self%entries(i)%value%to_string()
            write(out_unit, '(A,A,A)', advance='no') &
                trim(self%entries(i)%key), "=", trim(val_str)
        end do
        write(out_unit, '(A)') ")"
    end subroutine namespace_show

end module fclap_namespace
