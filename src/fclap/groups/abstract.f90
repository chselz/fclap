!> Shared group storage and stable parser-owned handles.
module fclap_groups_abstract
    use fclap_error_codes, only : ERR_INVALID_GROUP
    use fclap_error_stack, only : ErrorStack
    use fclap_formatter_model, only : HelpGroup
    implicit none
    private

    public :: GroupType, GroupBox, GroupHandle
    public :: new_group_handle, new_group_owner_id

    integer, save :: next_owner_id = 1

    !> Stable value identifying one group owned by one parser initialization.
    type :: GroupHandle
        private
        integer :: owner = 0
        integer :: index = 0
    contains
        procedure :: is_valid => group_handle_is_valid
        procedure :: belongs_to => group_handle_belongs_to
        procedure :: group_index => group_handle_index
    end type GroupHandle

    type, abstract :: GroupType
        character(len=:), allocatable :: title
        character(len=:), allocatable :: description
        logical :: required = .false.
        integer, allocatable :: members(:)
    contains
        procedure :: append_member => group_append_member
        procedure :: member_count => group_member_count
        procedure :: contains_argument => group_contains_argument
        procedure(group_help_snapshot_interface), deferred :: help_snapshot
        procedure :: make_help_snapshot => group_make_help_snapshot
        procedure, non_overridable :: validate_members => group_validate_members
        procedure :: validate_presence => group_validate_presence
    end type GroupType

    !> Box permitting one owned array of heterogeneous concrete groups.
    type :: GroupBox
        class(GroupType), allocatable :: item
    end type GroupBox

    abstract interface
        function group_help_snapshot_interface(self) result(snapshot)
            import :: GroupType, HelpGroup
            class(GroupType), intent(in) :: self
            type(HelpGroup) :: snapshot
        end function group_help_snapshot_interface
    end interface

contains

    function new_group_handle(owner, index) result(handle)
        integer, intent(in) :: owner, index
        type(GroupHandle) :: handle

        if (owner <= 0 .or. index <= 0) return
        handle%owner = owner
        handle%index = index
    end function new_group_handle

    integer function new_group_owner_id() result(owner)
        owner = next_owner_id
        if (next_owner_id == huge(next_owner_id)) then
            next_owner_id = 1
        else
            next_owner_id = next_owner_id + 1
        end if
    end function new_group_owner_id

    pure logical function group_handle_is_valid(self) result(valid)
        class(GroupHandle), intent(in) :: self

        valid = self%owner > 0 .and. self%index > 0
    end function group_handle_is_valid

    pure logical function group_handle_belongs_to(self, owner) result(matches)
        class(GroupHandle), intent(in) :: self
        integer, intent(in) :: owner

        matches = self%is_valid() .and. self%owner == owner
    end function group_handle_belongs_to

    pure integer function group_handle_index(self) result(index)
        class(GroupHandle), intent(in) :: self

        index = self%index
    end function group_handle_index

    subroutine group_append_member(self, argument_index)
        class(GroupType), intent(inout) :: self
        integer, intent(in) :: argument_index
        integer, allocatable :: temporary(:)
        integer :: old_size

        if (argument_index <= 0) return
        if (self%contains_argument(argument_index)) return
        old_size = self%member_count()
        allocate(temporary(old_size + 1))
        if (old_size > 0) temporary(:old_size) = self%members
        temporary(old_size + 1) = argument_index
        call move_alloc(temporary, self%members)
    end subroutine group_append_member

    pure integer function group_member_count(self) result(number)
        class(GroupType), intent(in) :: self

        if (allocated(self%members)) then
            number = size(self%members)
        else
            number = 0
        end if
    end function group_member_count

    pure logical function group_contains_argument(self, argument_index) &
        result(found)
        class(GroupType), intent(in) :: self
        integer, intent(in) :: argument_index

        found = .false.
        if (.not. allocated(self%members)) return
        found = any(self%members == argument_index)
    end function group_contains_argument

    !> Copy common group definition data into a formatter-owned snapshot.
    function group_make_help_snapshot(self, is_mutex) result(snapshot)
        class(GroupType), intent(in) :: self
        logical, intent(in) :: is_mutex
        type(HelpGroup) :: snapshot

        if (allocated(self%title)) snapshot%title = self%title
        if (allocated(self%description)) then
            snapshot%description = self%description
        end if
        if (allocated(self%members)) then
            snapshot%argument_indices = self%members
        else
            allocate(snapshot%argument_indices(0))
        end if
        snapshot%required = self%required
        snapshot%is_mutex = is_mutex
    end function group_make_help_snapshot

    !> Verify the shared invariant that every member names a known argument.
    subroutine group_validate_members(self, seen, group_index, errors)
        class(GroupType), intent(in) :: self
        logical, intent(in) :: seen(:)
        integer, intent(in) :: group_index
        type(ErrorStack), intent(inout) :: errors
        character(len=32) :: number
        integer :: index

        if (.not. allocated(self%members)) return
        do index = 1, size(self%members)
            if (self%members(index) >= 1 .and. &
                self%members(index) <= size(seen)) cycle
            write(number, '(i0)') group_index
            call errors%add( &
                "group contains an invalid argument index", &
                code=ERR_INVALID_GROUP, arg_name="group " // trim(number))
            return
        end do
    end subroutine group_validate_members

    !> Default validation for ordinary groups with no additional constraint.
    subroutine group_validate_presence(self, seen, group_index, errors)
        class(GroupType), intent(in) :: self
        logical, intent(in) :: seen(:)
        integer, intent(in) :: group_index
        type(ErrorStack), intent(inout) :: errors

        call self%validate_members(seen, group_index, errors)
    end subroutine group_validate_presence

end module fclap_groups_abstract
