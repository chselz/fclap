!> @file fclap.f90
!> @brief Public API for fclap - Fortran Command Line Argument Parser.
!>
!> @details This module provides the main entry point to fclap,
!> a Fortran Command Line Argument Parser inspired by Python's argparse.
!> It re-exports all public types and procedures needed for command line parsing.
!>
!> This is the only module users need to import for most use cases.
!>
!> @section usage Basic Usage
!> @code{.f90}
!>   use fclap, only: ArgumentParser, Namespace
!>   type(ArgumentParser) :: parser
!>   type(Namespace) :: args
!>
!>   call parser%init(prog="myapp", description="My application")
!>   call parser%add_argument("filename", help="Input file")
!>   call parser%add_argument("-v", "--verbose", action="store_true")
!>   args = parser%parse_args()
!> @endcode
!>
!> @section features Features
!> - Positional and optional arguments
!> - Type conversion (string, integer, real, logical)
!> - Multiple nargs modes (?, *, +, N)
!> - Actions: store, store_true, store_false, count, append
!> - Argument groups and mutually exclusive groups
!> - Subparsers for git-style subcommands
!> - Auto-generated help and version output
!>
!> @author fclap contributors
!> @version 0.1.0

module fclap
    ! Re-export from internal fclap modules
    use fclap_constants, only: &
        ! Nargs specifiers
        ARG_OPTIONAL, ARG_ZERO_OR_MORE, ARG_ONE_OR_MORE, &
        ARG_PARSER, ARG_REMAINDER, ARG_SINGLE, &
        ! Special values
        SUPPRESS, &
        ! Status values for argument visibility
        STATUS_ACTIVE, STATUS_DEPRECATED, STATUS_REMOVED, &
        ! Type constants
        TYPE_STRING, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL, &
        ! Action type constants
        ACT_STORE, ACT_STORE_TRUE, ACT_STORE_FALSE, &
        ACT_COUNT, ACT_APPEND, ACT_HELP, ACT_VERSION, &
        ACT_NOT_LESS_THAN, ACT_NOT_BIGGER_THAN, &
        ! Group type constants
        GROUP_STANDARD, GROUP_MUTEX
    use fclap_errors, only: fclap_error
    use fclap_namespace, only: Namespace, ValueContainer
    use fclap_actions, only: Action, not_less_than, not_bigger_than
    use fclap_parser, only: ArgumentParser, ArgumentGroup, MutuallyExclusiveGroup, &
        get_prog_name
    use fclap_version, only: get_fclap_version, fclap_version_compact, &
        fclap_version_string

    implicit none
    private

    ! Public types - main user-facing API
    public :: ArgumentParser      ! Main parser type
    public :: Namespace           ! Parsed arguments container
    public :: ArgumentGroup       ! Argument grouping for help
    public :: MutuallyExclusiveGroup ! Mutually exclusive options

    ! Public types - advanced usage
    public :: Action              ! Action definition type
    public :: ValueContainer      ! Value storage type
    public :: fclap_error         ! Error handling type

    ! Public constants - nargs specifiers
    public :: ARG_OPTIONAL        ! '?' - zero or one argument
    public :: ARG_ZERO_OR_MORE    ! '*' - zero or more arguments
    public :: ARG_ONE_OR_MORE     ! '+' - one or more arguments
    public :: ARG_PARSER          ! 'A...' - parser mode
    public :: ARG_REMAINDER       ! '...' - all remaining arguments
    public :: ARG_SINGLE          ! Single argument (default)

    ! Public constants - special values
    public :: SUPPRESS            ! Suppress help/default display

    ! Public constants - argument status
    public :: STATUS_ACTIVE       ! Normal active argument
    public :: STATUS_DEPRECATED   ! Deprecated with warning
    public :: STATUS_REMOVED      ! Removed, will error

    ! Public constants - type specifiers
    public :: TYPE_STRING
    public :: TYPE_INTEGER
    public :: TYPE_REAL
    public :: TYPE_LOGICAL

    ! Public constants - action types
    public :: ACT_STORE
    public :: ACT_STORE_TRUE
    public :: ACT_STORE_FALSE
    public :: ACT_COUNT
    public :: ACT_APPEND
    public :: ACT_HELP
    public :: ACT_VERSION
    public :: ACT_NOT_LESS_THAN
    public :: ACT_NOT_BIGGER_THAN

    ! Public procedures - bound-checking action factories
    public :: not_less_than       ! Factory: action=not_less_than(bound)
    public :: not_bigger_than     ! Factory: action=not_bigger_than(bound)

    ! Public constants - group types
    public :: GROUP_STANDARD
    public :: GROUP_MUTEX

    ! Public procedures
    public :: get_prog_name       ! Get program name from command line
    public :: get_fclap_version   ! Get version components
    public :: fclap_version_compact ! Compact version string
    public :: fclap_version_string  ! Full version string

end module fclap
