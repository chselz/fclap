!> Presence rules between arguments: requires, conflicts, required unless.
!>
!> A rule refers to arguments by their index in the owning parser's argument
!> list, like group members. Rules are checked after parsing against the
!> explicit-presence state of one parse operation, so a parser definition
!> stays reusable.
module fclap_rules
    use fclap_argument, only : Argument
    use fclap_error_codes, only : ERR_ARGUMENT_CONFLICT, &
        ERR_MISSING_DEPENDENCY, ERR_MISSING_REQUIRED
    use fclap_error_entry, only : ErrorEntry
    use fclap_error_stack, only : ErrorStack
    implicit none
    private

    public :: ArgumentRule
    public :: RULE_REQUIRES, RULE_CONFLICTS, RULE_REQUIRED_UNLESS
    public :: rule_kind_name

    !> The subject, when given, needs every other argument
    integer, parameter :: RULE_REQUIRES = 1
    !> The subject, when given, excludes every other argument
    integer, parameter :: RULE_CONFLICTS = 2
    !> The subject must be given unless one of the other arguments is
    integer, parameter :: RULE_REQUIRED_UNLESS = 3

    !> One presence rule between a subject argument and other arguments.
    type :: ArgumentRule
        !> One of the `RULE_*` constants
        integer :: kind = 0
        !> Index of the argument the rule is about
        integer :: subject = 0
        !> Indices of the arguments the subject depends on or excludes
        integer, allocatable :: others(:)
    contains
        procedure :: validate_presence => rule_validate_presence
        procedure :: remap => rule_remap
    end type ArgumentRule

contains

    !> Name of a rule kind for definition diagnostics.
    function rule_kind_name(kind) result(name)
        integer, intent(in) :: kind
        character(len=:), allocatable :: name

        select case (kind)
        case (RULE_REQUIRES)
            name = "requires"
        case (RULE_CONFLICTS)
            name = "conflict"
        case (RULE_REQUIRED_UNLESS)
            name = "required-unless"
        case default
            name = "unknown"
        end select
    end function rule_kind_name

    !> Add diagnostics for one parse operation's explicit-presence state.
    !>
    !> Diagnostics name the subject destination, so command-line formatting
    !> prefixes them with `argument -x/--name:` like other argument errors.
    !> An argument listed twice is reported once, and a conflicting pair that
    !> is already in `errors`, e.g. from the mirrored rule, is not repeated.
    subroutine rule_validate_presence(self, arguments, seen, errors)
        class(ArgumentRule), intent(in) :: self
        type(Argument), intent(in) :: arguments(:)
        logical, intent(in) :: seen(:)
        type(ErrorStack), intent(inout) :: errors
        logical, allocatable :: others_seen(:)
        integer, allocatable :: others(:)
        character(len=:), allocatable :: dest
        integer :: index

        if (.not. valid_index(self%subject, seen)) return
        if (.not. allocated(self%others)) return
        others = unique_indices(self%others)
        allocate(others_seen(size(others)))
        do index = 1, size(others)
            others_seen(index) = .false.
            if (valid_index(others(index), seen)) then
                others_seen(index) = seen(others(index))
            end if
        end do
        dest = arguments(self%subject)%dest

        select case (self%kind)
        case (RULE_REQUIRES)
            if (.not. seen(self%subject)) return
            if (all(others_seen)) return
            call errors%add("requires " // &
                join_labels(arguments, pack(others, .not. others_seen), &
                ", "), code=ERR_MISSING_DEPENDENCY, arg_name=dest)
        case (RULE_CONFLICTS)
            if (.not. seen(self%subject)) return
            do index = 1, size(others)
                if (.not. others_seen(index)) cycle
                if (conflict_reported(errors, arguments, self%subject, &
                    others(index))) cycle
                call errors%add(conflict_message(arguments, others(index)), &
                    code=ERR_ARGUMENT_CONFLICT, arg_name=dest)
            end do
        case (RULE_REQUIRED_UNLESS)
            if (seen(self%subject)) return
            if (any(others_seen)) return
            call errors%add("required unless " // &
                join_labels(arguments, others, " or ") // " is given", &
                code=ERR_MISSING_REQUIRED, arg_name=dest)
        case default
            ! Rules are only created with the kinds above.
        end select
    end subroutine rule_validate_presence

    !> Translate argument indices, e.g. when a parent's rules are copied.
    !>
    !> `index_map(old)` is the new index of argument `old`, or zero when that
    !> argument was not copied. `kept` is false if any referenced argument is
    !> missing, in which case the rule must be dropped.
    subroutine rule_remap(self, index_map, kept)
        class(ArgumentRule), intent(inout) :: self
        integer, intent(in) :: index_map(:)
        logical, intent(out) :: kept
        integer :: index

        kept = .false.
        if (self%subject < 1 .or. self%subject > size(index_map)) return
        self%subject = index_map(self%subject)
        if (self%subject == 0) return
        if (.not. allocated(self%others)) return
        do index = 1, size(self%others)
            if (self%others(index) < 1 .or. &
                self%others(index) > size(index_map)) return
            self%others(index) = index_map(self%others(index))
            if (self%others(index) == 0) return
        end do
        kept = .true.
    end subroutine rule_remap

    function conflict_message(arguments, other) result(message)
        type(Argument), intent(in) :: arguments(:)
        integer, intent(in) :: other
        character(len=:), allocatable :: message

        message = "not allowed with argument " // arguments(other)%display_label()
    end function conflict_message

    !> Report whether the conflict between `first` and `second` is already in
    !> `errors`, reported for either of the two arguments.
    logical function conflict_reported(errors, arguments, first, second) &
        result(reported)
        type(ErrorStack), intent(in) :: errors
        type(Argument), intent(in) :: arguments(:)
        integer, intent(in) :: first, second
        type(ErrorEntry) :: entry
        integer :: index

        reported = .false.
        do index = 1, errors%count()
            entry = errors%get(index)
            if (entry%code /= ERR_ARGUMENT_CONFLICT) cycle
            if (.not. allocated(entry%arg_name) .or. &
                .not. allocated(entry%message)) cycle
            reported = (entry%arg_name == arguments(first)%dest .and. &
                entry%message == conflict_message(arguments, second)) .or. &
                (entry%arg_name == arguments(second)%dest .and. &
                entry%message == conflict_message(arguments, first))
            if (reported) return
        end do
    end function conflict_reported

    !> Drop repeated entries, keeping the first occurrence of each index.
    pure function unique_indices(indices) result(unique)
        integer, intent(in) :: indices(:)
        integer, allocatable :: unique(:)
        logical :: keep(size(indices))
        integer :: index

        do index = 1, size(indices)
            keep(index) = .not. any(indices(:index - 1) == indices(index))
        end do
        unique = pack(indices, keep)
    end function unique_indices

    pure logical function valid_index(index, seen) result(valid)
        integer, intent(in) :: index
        logical, intent(in) :: seen(:)

        valid = index >= 1 .and. index <= size(seen)
    end function valid_index

    function join_labels(arguments, indices, separator) result(text)
        type(Argument), intent(in) :: arguments(:)
        integer, intent(in) :: indices(:)
        character(len=*), intent(in) :: separator
        character(len=:), allocatable :: text
        integer :: index

        text = ""
        do index = 1, size(indices)
            if (indices(index) < 1 .or. indices(index) > size(arguments)) cycle
            if (len(text) > 0) text = text // separator
            text = text // arguments(indices(index))%display_label()
        end do
    end function join_labels

end module fclap_rules
