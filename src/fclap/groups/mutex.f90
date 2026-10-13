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
        procedure :: accepts_member => mutex_accepts_member
    end type MutexGroup

contains

    !> Build the formatter-facing representation of a mutex group.
    function mutex_help_snapshot(self) result(snapshot)
        class(MutexGroup), intent(in) :: self
        type(HelpGroup) :: snapshot

        snapshot = self%make_help_snapshot(.true.)
    end function mutex_help_snapshot

    !> Members must be omissible, otherwise every other member conflicts.
    !>
    !> A required option, or a positional that needs at least one value, is
    !> always present; use `required=.true.` on the group instead.
    logical function mutex_accepts_member(self, required, positional, &
        min_count, reason) result(accepted)
        class(MutexGroup), intent(in) :: self
        logical, intent(in) :: required, positional
        integer, intent(in) :: min_count
        character(len=:), allocatable, intent(out) :: reason

        accepted = .not. required .and. .not. (positional .and. min_count > 0)
        if (accepted) then
            reason = ""
        else
            reason = "mutually exclusive arguments must be optional"
        end if
    end function mutex_accepts_member

    !> Add mutex diagnostics for one parse operation's presence state.
    subroutine mutex_validate_presence(self, seen, labels, group_index, errors)
        class(MutexGroup), intent(in) :: self
        logical, intent(in) :: seen(:)
        character(len=*), intent(in) :: labels(:)
        integer, intent(in) :: group_index
        type(ErrorStack), intent(inout) :: errors
        character(len=:), allocatable :: group_name, first_seen, members
        integer :: error_count, index, member, seen_count

        error_count = errors%count()
        call self%validate_members(seen, group_index, errors)
        if (errors%count() > error_count) return

        seen_count = 0
        members = ""
        first_seen = ""
        group_name = mutex_group_name(self, group_index)
        do index = 1, self%member_count()
            member = self%members(index)
            if (member < 1 .or. member > size(seen)) cycle
            if (len(members) > 0) members = members // " "
            members = members // member_label(labels, member)
            if (.not. seen(member)) cycle
            seen_count = seen_count + 1
            if (seen_count == 1) then
                first_seen = member_label(labels, member)
            else if (seen_count == 2) then
                call errors%add("argument " // member_label(labels, member) // &
                    ": not allowed with argument " // first_seen, &
                    code=ERR_MUTEX_CONFLICT, arg_name=group_name)
            end if
        end do

        if (self%required .and. seen_count == 0) then
            call errors%add("one of the arguments " // members // &
                " is required", code=ERR_MUTEX_REQUIRED, arg_name=group_name)
        end if
    end subroutine mutex_validate_presence

    function member_label(labels, member) result(label)
        character(len=*), intent(in) :: labels(:)
        integer, intent(in) :: member
        character(len=:), allocatable :: label

        if (member <= size(labels)) then
            label = trim(labels(member))
        else
            label = ""
        end if
    end function member_label

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
