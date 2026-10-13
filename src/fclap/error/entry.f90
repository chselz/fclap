!> One structured diagnostic: code, severity, message, and context.
module fclap_error_entry
    use fclap_error_codes, only : ERROR_FATAL, ERROR_WARNING

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
        procedure :: to_string => error_entry_to_string
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

    function error_entry_to_string(self) result(str)
        class(ErrorEntry), intent(in) :: self
        character(len=:), allocatable :: str
        character(len=32) :: prefix
        character(len=32) :: number

        if (self%severity == ERROR_FATAL) then
            prefix = "FATAL"
        else if (self%severity == ERROR_WARNING) then
            prefix = "WARNING"
        else
            prefix = "ERROR"
        end if

        str = "[" // trim(prefix) // "]"
        if (allocated(self%message)) str = str // " " // self%message
        if (allocated(self%arg_name)) then
            str = str // " (argument: " // trim(self%arg_name) // ")"
        end if
        if (allocated(self%token)) then
            str = str // " (token: " // trim(self%token) // ")"
        end if
        if (self%token_index > 0) then
            write(number, '(i0)') self%token_index
            str = str // " (index: " // trim(number) // ")"
        end if
        if (allocated(self%suggestion)) then
            str = str // " (suggestion: " // self%suggestion // ")"
        end if
    end function
    
end module fclap_error_entry
