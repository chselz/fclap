!> @file fclap_constants.f90
!> @brief Constants module for fclap - configuration and enumeration values.
!>
!> @details This module contains all configuration constants, parameter values,
!> and enumerations used throughout the fclap library.
!> It has no dependencies on other modules.
!>
!> Constants are organized into categories:
!> - Nargs constants: Control argument consumption
!> - Size limits: Maximum array sizes
!> - Value types: Type identifiers
!> - Action types: Action behavior specifiers
!> - Status constants: Argument lifecycle states
!> - Group types: Argument group identifiers

module fclap_constants
    implicit none
    private

    ! ============================================================================
    ! NARGS CONSTANTS
    ! ============================================================================
    ! These control how many command-line arguments are consumed by an argument.

    !> Constant for optional nargs (?): 0 or 1 values
    integer, parameter, public :: ARG_OPTIONAL = -1
    !> Constant for zero or more nargs (*): 0 or more values
    integer, parameter, public :: ARG_ZERO_OR_MORE = -2
    !> Constant for one or more nargs (+): 1 or more values
    integer, parameter, public :: ARG_ONE_OR_MORE = -3
    !> Constant for parser nargs (sub-parser)
    integer, parameter, public :: ARG_PARSER = -4
    !> Constant for remainder nargs: all remaining arguments
    integer, parameter, public :: ARG_REMAINDER = -5
    !> Constant for single argument (default)
    integer, parameter, public :: ARG_SINGLE = 1

    ! ============================================================================
    ! SPECIAL STRING CONSTANTS
    ! ============================================================================

    !> Constant to suppress default/help output
    character(len=*), parameter, public :: SUPPRESS = "==SUPPRESS=="

    ! ============================================================================
    ! SIZE LIMITS
    ! ============================================================================
    ! Maximum array sizes for various data structures.

    !> Maximum length for argument names, values, and strings
    integer, parameter, public :: MAX_ARG_LEN = 256
    !> Maximum number of option strings per argument (e.g., -v, --verbose)
    integer, parameter, public :: MAX_OPTION_STRINGS = 4
    !> Maximum number of choices for constrained argument values
    integer, parameter, public :: MAX_CHOICES = 32
    !> Maximum number of values in a list (for append action or nargs=*)
    integer, parameter, public :: MAX_LIST_VALUES = 64
    !> Maximum number of arguments/actions that can be added to a parser
    integer, parameter, public :: MAX_ACTIONS = 128
    !> Maximum number of sub-parsers that can be added
    integer, parameter, public :: MAX_SUBPARSERS = 32
    !> Maximum number of argument groups
    integer, parameter, public :: MAX_GROUPS = 32
    !> Maximum number of actions in a group
    integer, parameter, public :: MAX_GROUP_ACTIONS = 32
    !> Maximum number of parent parsers
    integer, parameter, public :: MAX_PARENTS = 8

    ! ============================================================================
    ! VALUE TYPE CONSTANTS
    ! ============================================================================
    ! These identify the data type stored in a value container.

    !> Value type constant for string/character values
    integer, parameter, public :: TYPE_STRING = 1
    !> Value type constant for integer values
    integer, parameter, public :: TYPE_INTEGER = 2
    !> Value type constant for real/float values
    integer, parameter, public :: TYPE_REAL = 3
    !> Value type constant for logical/boolean values
    integer, parameter, public :: TYPE_LOGICAL = 4

    ! ============================================================================
    ! ACTION TYPE CONSTANTS
    ! ============================================================================
    ! These identify what action to take when an argument is encountered.

    !> Action type: store a single value (default action)
    integer, parameter, public :: ACT_STORE = 1
    !> Action type: store True when flag is present (no value consumed)
    integer, parameter, public :: ACT_STORE_TRUE = 2
    !> Action type: store False when flag is present (no value consumed)
    integer, parameter, public :: ACT_STORE_FALSE = 3
    !> Action type: count occurrences of the flag
    integer, parameter, public :: ACT_COUNT = 4
    !> Action type: append value to a list (allows repeated use)
    integer, parameter, public :: ACT_APPEND = 5
    !> Action type: print help message and exit
    integer, parameter, public :: ACT_HELP = 6
    !> Action type: print version string and exit
    integer, parameter, public :: ACT_VERSION = 7

    ! ============================================================================
    ! ARGUMENT STATUS CONSTANTS
    ! ============================================================================
    ! These control argument lifecycle (active, deprecated, removed).

    !> Status: argument is active and fully supported (default)
    integer, parameter, public :: STATUS_ACTIVE = 0
    !> Status: argument is deprecated, prints warning but still works
    integer, parameter, public :: STATUS_DEPRECATED = 1
    !> Status: argument has been removed, prints error and rejects usage
    integer, parameter, public :: STATUS_REMOVED = 2

    ! ============================================================================
    ! GROUP TYPE CONSTANTS
    ! ============================================================================
    ! These identify the type of argument group.

    !> Group type: standard argument group (for help organization)
    integer, parameter, public :: GROUP_STANDARD = 1
    !> Group type: mutually exclusive group (only one argument allowed)
    integer, parameter, public :: GROUP_MUTEX = 2

end module fclap_constants
