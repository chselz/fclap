module fclap_error_codes
    implicit none

    integer, parameter, public :: FCLAP_OK = 0

    ! Configuration errors (1000-1999)
    integer, parameter, public :: ERR_INVALID_ARGUMENT_NAME = 1001
    integer, parameter, public :: ERR_DUPLICATE_OPTION      = 1002
    integer, parameter, public :: ERR_DUPLICATE_DEST        = 1003
    integer, parameter, public :: ERR_INVALID_NARGS         = 1004
    integer, parameter, public :: ERR_INCOMPATIBLE_ACTION   = 1005
    integer, parameter, public :: ERR_INVALID_DEFAULT       = 1006
    integer, parameter, public :: ERR_INVALID_GROUP         = 1007
    integer, parameter, public :: ERR_INVALID_TYPE_NAME     = 1008
    integer, parameter, public :: ERR_INVALID_LIFECYCLE     = 1009

    ! Single-parser runtime errors (2000-2099)
    integer, parameter, public :: ERR_UNKNOWN_ARGUMENT    = 2001
    integer, parameter, public :: ERR_MISSING_VALUE       = 2002
    integer, parameter, public :: ERR_EXTRA_POSITIONAL    = 2003
    integer, parameter, public :: ERR_MISSING_REQUIRED    = 2004
    integer, parameter, public :: ERR_INVALID_VALUE       = 2005
    integer, parameter, public :: ERR_INVALID_CHOICE      = 2006
    integer, parameter, public :: ERR_VALIDATION_FAILED   = 2007
    integer, parameter, public :: ERR_MUTEX_CONFLICT      = 2008
    integer, parameter, public :: ERR_MUTEX_REQUIRED      = 2009
    integer, parameter, public :: ERR_REMOVED_ARGUMENT    = 2010
    integer, parameter, public :: ERR_DEPRECATED_ARGUMENT = 2011

    ! Reserved subparser errors (2100-2199)
    integer, parameter, public :: ERR_UNKNOWN_SUBCOMMAND = 2101
    integer, parameter, public :: ERR_MISSING_SUBCOMMAND = 2102

    ! Namespace errors (3000-3099)
    integer, parameter, public :: ERR_NAMESPACE_MISSING_KEY   = 3001
    integer, parameter, public :: ERR_NAMESPACE_TYPE_MISMATCH = 3002

    integer, parameter, public :: ERROR_FATAL   = 1
    integer, parameter, public :: ERROR_WARNING = 2

end module fclap_error_codes
