!> One structured diagnostic: code, severity, message, and context.
module fclap_error_entry
    use fclap_error_codes, only : ERROR_FATAL

    implicit none

    private
    public :: ErrorEntry

    !> A single structured diagnostic produced by configuration or parsing.
    type :: ErrorEntry
        !> Description of what exact failed e.g. invalid choice
        integer :: code = 0
        !> severity of the error (fatal/warning)
        integer :: severity = ERROR_FATAL
        !> Error message
        character(len=:), allocatable :: message
        !> Destination or name of the argument the diagnostic refers to
        character(len=:), allocatable :: arg_name
        !> Command-line token that caused the diagnostic
        character(len=:), allocatable :: token
        !> One-based position of `token` on the command line, zero if unknown
        integer :: token_index = 0
        !> Likely intended spelling of `token`, e.g. a known option name
        character(len=:), allocatable :: suggestion
    contains
        procedure :: init => error_entry_init
    end type ErrorEntry

contains

    subroutine error_entry_init(self, message, code, severity, arg_name, token, &
        token_index, suggestion)
        class(ErrorEntry), intent(out) :: self
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code, severity
        character(len=*), intent(in), optional :: arg_name, token
        integer, intent(in), optional :: token_index
        character(len=*), intent(in), optional :: suggestion

        self%message = trim(message)
        if (present(code)) self%code = code
        if (present(severity)) self%severity = severity
        if (present(arg_name)) self%arg_name = trim(arg_name)
        if (present(token)) self%token = trim(token)
        if (present(token_index)) self%token_index = token_index
        if (present(suggestion)) then
            if (len_trim(suggestion) > 0) self%suggestion = trim(suggestion)
        end if
    end subroutine error_entry_init

end module fclap_error_entry
