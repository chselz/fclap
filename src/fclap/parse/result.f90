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

    public :: ParseResult

    type :: ParseResult
        type(Namespace) :: namespace
        type(ErrorStack) :: errors
        integer :: outcome = PARSE_SUCCESS
        character(len=:), allocatable :: text
        character(len=:), allocatable :: selected_command_path(:)
    contains
        procedure :: succeeded => parse_result_succeeded
        procedure :: failed => parse_result_failed
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

end module fclap_parse_result
