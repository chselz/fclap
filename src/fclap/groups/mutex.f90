!> Concrete group kind for mutually exclusive arguments.
!>
!> `GroupType` owns the common group metadata and member list.  This derived
!> type intentionally adds no storage.  It owns the mutex-specific validation
!> behavior, while the parse engine supplies fresh explicit-presence state so
!> parser definitions remain reusable.
module fclap_groups_mutex
    use fclap_error_codes, only : ERR_MUTEX_CONFLICT, ERR_MUTEX_REQUIRED
    use fclap_error_stack, only : ErrorStack
    use fclap_formatter_model, only : HelpGroup
    use fclap_groups_abstract, only : GroupType

    implicit none
    private

    public :: MutexGroup

    !> A group whose explicitly supplied members may not occur together.
    type, extends(GroupType) :: MutexGroup
    contains
        procedure :: help_snapshot => mutex_help_snapshot
        procedure :: validate_presence => mutex_validate_presence
    end type MutexGroup

contains

    !> Build the formatter-facing representation of a mutex group.
    function mutex_help_snapshot(self) result(snapshot)
        class(MutexGroup), intent(in) :: self
        type(HelpGroup) :: snapshot

        snapshot = self%make_help_snapshot(.true.)
    end function mutex_help_snapshot

    !> Add mutex diagnostics for one parse operation's presence state.
    subroutine mutex_validate_presence(self, seen, group_index, errors)
        class(MutexGroup), intent(in) :: self
        logical, intent(in) :: seen(:)
        integer, intent(in) :: group_index
        type(ErrorStack), intent(inout) :: errors
        character(len=:), allocatable :: group_name
        integer :: error_count, index, member, seen_count

        error_count = errors%count()
        call self%validate_members(seen, group_index, errors)
        if (errors%count() > error_count) return

        seen_count = 0
        do index = 1, self%member_count()
            member = self%members(index)
            if (member < 1 .or. member > size(seen)) cycle
            if (seen(member)) seen_count = seen_count + 1
        end do

        group_name = mutex_group_name(self, group_index)
        if (seen_count > 1) then
            call errors%add( &
                "mutually exclusive arguments were supplied", &
                code=ERR_MUTEX_CONFLICT, arg_name=group_name)
        else if (self%required .and. seen_count == 0) then
            call errors%add( &
                "one argument from the mutually exclusive group is required", &
                code=ERR_MUTEX_REQUIRED, arg_name=group_name)
        end if
    end subroutine mutex_validate_presence

    function mutex_group_name(self, group_index) result(name)
        class(MutexGroup), intent(in) :: self
        integer, intent(in) :: group_index
        character(len=:), allocatable :: name
        character(len=32) :: number

        if (allocated(self%title)) then
            if (len_trim(self%title) > 0) then
                name = trim(self%title)
                return
            end if
        end if
        write(number, '(i0)') group_index
        name = "mutually exclusive group " // trim(number)
    end function mutex_group_name

end module fclap_groups_mutex
