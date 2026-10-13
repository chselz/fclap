!> Self-contained result returned by non-terminating parser entry points.
module fclap_parse_result
    use fclap_error_stack, only : ErrorStack
    use fclap_namespace, only : Namespace
    implicit none
    private

    integer, parameter, public :: PARSE_SUCCESS = 0
    integer, parameter, public :: PARSE_FAILURE = 1
    integer, parameter, public :: PARSE_HELP = 2
    integer, parameter, public :: PARSE_VERSION = 3

    !> Where a destination's value came from, ordered by precedence
    integer, parameter, public :: SOURCE_NONE = 0
    integer, parameter, public :: SOURCE_DEFAULT = 1
    integer, parameter, public :: SOURCE_ENVIRONMENT = 2
    integer, parameter, public :: SOURCE_COMMAND_LINE = 3

    public :: ParseResult

    type :: ParseResult
        type(Namespace) :: namespace
        type(ErrorStack) :: errors
        integer :: outcome = PARSE_SUCCESS
        character(len=:), allocatable :: text
        character(len=:), allocatable :: selected_command_path(:)
        !> Destinations with a recorded value source
        character(len=:), allocatable, private :: source_dests(:)
        !> `SOURCE_*` constant for each entry of `source_dests`
        integer, allocatable, private :: source_kinds(:)
    contains
        procedure :: succeeded => parse_result_succeeded
        procedure :: failed => parse_result_failed
        procedure :: value_source => parse_result_value_source
        procedure :: set_source => parse_result_set_source
        procedure :: merge_sources => parse_result_merge_sources
    end type ParseResult

contains

    pure logical function parse_result_succeeded(self) result(succeeded)
        class(ParseResult), intent(in) :: self

        succeeded = self%outcome /= PARSE_FAILURE
    end function parse_result_succeeded

    pure logical function parse_result_failed(self) result(failed)
        class(ParseResult), intent(in) :: self

        failed = self%outcome == PARSE_FAILURE
    end function parse_result_failed

    !> Where the value stored under `dest` came from: `SOURCE_COMMAND_LINE`,
    !> `SOURCE_ENVIRONMENT`, `SOURCE_DEFAULT`, or `SOURCE_NONE` when the
    !> namespace has no value for it.
    pure integer function parse_result_value_source(self, dest) result(source)
        class(ParseResult), intent(in) :: self
        character(len=*), intent(in) :: dest
        integer :: index

        source = SOURCE_NONE
        if (.not. allocated(self%source_dests)) return
        do index = 1, size(self%source_dests)
            if (self%source_dests(index) /= trim(dest)) cycle
            source = self%source_kinds(index)
            return
        end do
    end function parse_result_value_source

    !> Record or replace the value source of one destination.
    subroutine parse_result_set_source(self, dest, source)
        class(ParseResult), intent(inout) :: self
        character(len=*), intent(in) :: dest
        integer, intent(in) :: source
        character(len=:), allocatable :: dests(:)
        integer, allocatable :: kinds(:)
        integer :: index, count, width

        count = 0
        if (allocated(self%source_dests)) then
            count = size(self%source_dests)
            do index = 1, count
                if (self%source_dests(index) /= trim(dest)) cycle
                self%source_kinds(index) = source
                return
            end do
        end if

        width = len_trim(dest)
        if (count > 0) width = max(width, len(self%source_dests))
        allocate(character(len=width) :: dests(count + 1))
        allocate(kinds(count + 1))
        if (count > 0) then
            dests(:count) = self%source_dests
            kinds(:count) = self%source_kinds
        end if
        dests(count + 1) = trim(dest)
        kinds(count + 1) = source
        call move_alloc(dests, self%source_dests)
        call move_alloc(kinds, self%source_kinds)
    end subroutine parse_result_set_source

    !> Take over every source recorded by `other`, e.g. a subcommand's result.
    subroutine parse_result_merge_sources(self, other)
        class(ParseResult), intent(inout) :: self
        type(ParseResult), intent(in) :: other
        integer :: index

        if (.not. allocated(other%source_dests)) return
        do index = 1, size(other%source_dests)
            call self%set_source(trim(other%source_dests(index)), &
                other%source_kinds(index))
        end do
    end subroutine parse_result_merge_sources

end module fclap_parse_result
