module fclap_error_entry
    use fclap_error_codes, only : ERROR_FATAL, ERROR_WARNING

    implicit none

    private
    public :: error_entry

    !> A single error instance
    type :: error_entry
        !> 
        integer :: code = 0
        !> severity of the error (default fatal)
        integer :: severity = ERROR_FATAL
        !> error message of a single occured error
        character(len=:), allocatable :: message
        !> Optional: Which arg caused the error
        character(len=:), allocatable :: arg_name
    contains
        procedure :: to_string => error_entry_to_string
    end type

contains

    function error_entry_to_string(self) result(str)
        class(error_entry), intent(in) :: self
        character(len=:), allocatable :: str
        character(len=20) :: sev_str

        if (self%severity == ERROR_FATAL) then
            sev_str = "[FATAL]"
        else if (self%severity == ERROR_WARNING) then
            sev_str = "[WARNING] "
        else
            sev_str = "[ERR]"
        end if

        str = trim(sev_str) // " " // self%message
        if (allocated(self%arg_name)) then
            str = str // " (arg: " // trim(self%arg_name) // ")"
        end if
    end function
    
end module fclap_error_entry