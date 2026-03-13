module fclap_error_codes
    implicit none
    
    ! Define specific codes publicly
    integer, parameter, public :: ERR_UNKNOWN_ARG   = 101
    integer, parameter, public :: ERR_MISSING_VALUE = 102
    integer, parameter, public :: ERR_INVALID_TYPE  = 103
    integer, parameter, public :: ERR_MISSING_POS   = 104

    ! Define error severity constants
    !> integer code for a fatal error
    integer, parameter, public :: ERROR_FATAL = 1
    !> integer code for a warning
    integer, parameter, public :: ERROR_WARNING = 2
  
end module fclap_error_codes