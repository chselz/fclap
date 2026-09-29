!> Mutable state owned by one parsing operation.
module fclap_parse_context
    use fclap_error_stack, only : ErrorStack
    use fclap_namespace, only : Namespace
    implicit none
    private

    public :: ParseContext

    type :: ParseContext
        character(len=:), allocatable :: tokens(:)
        integer :: cursor = 1
        integer :: positional_cursor = 1
        logical :: options_enabled = .true.
        logical, allocatable :: seen(:)
        logical, allocatable :: completed(:)
        type(Namespace) :: namespace
        type(ErrorStack) :: errors
        logical :: subcommands_enabled = .false.
        logical :: subcommand_required = .false.
        character(len=:), allocatable :: subcommand_names(:)
        character(len=:), allocatable :: selected_command_path(:)
    contains
        procedure :: init => context_init
        procedure :: at_end => context_at_end
        procedure :: current_token => context_current_token
        procedure :: advance => context_advance
        procedure :: mark_seen => context_mark_seen
        procedure :: was_seen => context_was_seen
        procedure :: mark_completed => context_mark_completed
        procedure :: was_completed => context_was_completed
        procedure :: configure_subcommands => context_configure_subcommands
        procedure :: is_subcommand => context_is_subcommand
        procedure :: select_command => context_select_command
    end type ParseContext

contains

    subroutine context_init(self, tokens, argument_count)
        class(ParseContext), intent(out) :: self
        character(len=*), intent(in) :: tokens(:)
        integer, intent(in) :: argument_count
        integer :: index, token_length

        token_length = 1
        do index = 1, size(tokens)
            token_length = max(token_length, len_trim(tokens(index)))
        end do
        allocate(character(len=token_length) :: self%tokens(size(tokens)))
        do index = 1, size(tokens)
            self%tokens(index) = trim(tokens(index))
        end do

        allocate(self%seen(max(0, argument_count)), source=.false.)
        allocate(self%completed(max(0, argument_count)), source=.false.)
        call self%namespace%init()
        call self%errors%clear()
    end subroutine context_init

    subroutine context_configure_subcommands(self, names, required)
        class(ParseContext), intent(inout) :: self
        character(len=*), intent(in) :: names(:)
        logical, intent(in), optional :: required
        integer :: index, name_length

        self%subcommands_enabled = .true.
        self%subcommand_required = .false.
        if (present(required)) self%subcommand_required = required

        name_length = 1
        do index = 1, size(names)
            name_length = max(name_length, len_trim(names(index)))
        end do
        allocate(character(len=name_length) :: self%subcommand_names(size(names)))
        do index = 1, size(names)
            self%subcommand_names(index) = trim(names(index))
        end do
    end subroutine context_configure_subcommands

    pure logical function context_is_subcommand(self, token) result(found)
        class(ParseContext), intent(in) :: self
        character(len=*), intent(in) :: token

        found = .false.
        if (.not. allocated(self%subcommand_names)) return
        if (size(self%subcommand_names) == 0) return
        found = any(self%subcommand_names == trim(token))
    end function context_is_subcommand

    subroutine context_select_command(self, name)
        class(ParseContext), intent(inout) :: self
        character(len=*), intent(in) :: name

        if (allocated(self%selected_command_path)) &
            deallocate(self%selected_command_path)
        allocate(character(len=max(1, len_trim(name))) :: &
            self%selected_command_path(1))
        self%selected_command_path(1) = trim(name)
    end subroutine context_select_command

    pure logical function context_at_end(self) result(at_end)
        class(ParseContext), intent(in) :: self

        at_end = .not. allocated(self%tokens)
        if (.not. at_end) at_end = self%cursor > size(self%tokens)
    end function context_at_end

    function context_current_token(self) result(token)
        class(ParseContext), intent(in) :: self
        character(len=:), allocatable :: token

        if (self%at_end()) then
            token = ""
        else
            token = trim(self%tokens(self%cursor))
        end if
    end function context_current_token

    subroutine context_advance(self)
        class(ParseContext), intent(inout) :: self

        if (.not. self%at_end()) self%cursor = self%cursor + 1
    end subroutine context_advance

    subroutine context_mark_seen(self, index)
        class(ParseContext), intent(inout) :: self
        integer, intent(in) :: index

        if (.not. allocated(self%seen)) return
        if (index < 1 .or. index > size(self%seen)) return
        self%seen(index) = .true.
    end subroutine context_mark_seen

    pure logical function context_was_seen(self, index) result(seen)
        class(ParseContext), intent(in) :: self
        integer, intent(in) :: index

        seen = .false.
        if (.not. allocated(self%seen)) return
        if (index < 1 .or. index > size(self%seen)) return
        seen = self%seen(index)
    end function context_was_seen

    subroutine context_mark_completed(self, index)
        class(ParseContext), intent(inout) :: self
        integer, intent(in) :: index

        if (.not. allocated(self%completed)) return
        if (index < 1 .or. index > size(self%completed)) return
        self%completed(index) = .true.
    end subroutine context_mark_completed

    pure logical function context_was_completed(self, index) result(completed)
        class(ParseContext), intent(in) :: self
        integer, intent(in) :: index

        completed = .false.
        if (.not. allocated(self%completed)) return
        if (index < 1 .or. index > size(self%completed)) return
        completed = self%completed(index)
    end function context_was_completed

end module fclap_parse_context
