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
        character(len=:), allocatable :: arg_name
        character(len=:), allocatable :: flag
        integer :: flag_index = 0
    contains
        procedure :: init => error_entry_init
        procedure :: to_string => error_entry_to_string
    end type ErrorEntry

contains

    subroutine error_entry_init(self, message, code, severity, arg_name, flag, flag_index)
        class(ErrorEntry), intent(out) :: self
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code, severity
        character(len=*), intent(in), optional :: arg_name, flag
        integer, intent(in), optional :: flag_index

        self%message = trim(message)
        if (present(code)) self%code = code
        if (present(severity)) self%severity = severity
        if (present(arg_name)) self%arg_name = trim(arg_name)
        if (present(flag)) self%flag = trim(flag)
        if (present(flag_index)) self%flag_index = flag_index
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
        if (allocated(self%flag)) then
            str = str // " (flag: " // trim(self%flag) // ")"
        end if
        if (self%flag_index > 0) then
            write(number, '(i0)') self%flag_index
            str = str // " (index: " // trim(number) // ")"
        end if
    end function
    
end module fclap_error_entry
