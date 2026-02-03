!> Command Line Argument Parser module
!>
!> This module provides a comprehensive command-line argument parsing library
!> inspired by Python's argparse module. It supports optional arguments,
!> positional arguments, various actions, sub-parsers, and automatic help generation.
!>
!> Usage example:
!> ```fortran
!> use fclap_argparser
!> type(ArgParser) :: parser
!> type(ParsedArgs) :: args
!>
!> call parser%init(description="My program description")
!> call parser%add_argument("-v", "--verbose", action="store_true", help="Enable verbose output")
!> call parser%add_argument("input", help="Input file")
!> args = parser%parse_args()
!> ```

module fclap_argparser
    use argparse_utils, only: get_prog_name
    implicit none
    private

    ! ============================================================================
    ! CONSTANTS
    ! ============================================================================

    !> Constant for optional nargs (?)
    integer, parameter, public :: NARGS_OPTIONAL = -1
    !> Constant for zero or more nargs (*)
    integer, parameter, public :: NARGS_ZERO_OR_MORE = -2
    !> Constant for one or more nargs (+)
    integer, parameter, public :: NARGS_ONE_OR_MORE = -3
    !> Constant for parser nargs (sub-parser)
    integer, parameter, public :: NARGS_PARSER = -4
    !> Constant for remainder nargs
    integer, parameter, public :: NARGS_REMAINDER = -5
    !> Constant for single argument (default)
    integer, parameter, public :: NARGS_SINGLE = 1

    !> Constant to suppress default/help output
    character(len=*), parameter, public :: SUPPRESS = "==SUPPRESS=="

    !> Maximum length for argument names, values, and strings
    integer, parameter :: MAX_ARG_LEN = 256
    !> Maximum number of option strings per argument (e.g., -v, --verbose, -V, --version)
    integer, parameter :: MAX_OPTION_STRINGS = 4
    !> Maximum number of choices for constrained argument values
    integer, parameter :: MAX_CHOICES = 32
    !> Maximum number of values in a list (for append action or nargs=*)
    integer, parameter :: MAX_LIST_VALUES = 64
    !> Maximum number of arguments/actions that can be added to a parser
    integer, parameter :: MAX_ACTIONS = 128
    !> Maximum number of sub-parsers that can be added
    integer, parameter :: MAX_SUBPARSERS = 32
    !> Maximum number of argument groups
    integer, parameter :: MAX_GROUPS = 32
    !> Maximum number of actions in a group
    integer, parameter :: MAX_GROUP_ACTIONS = 32
    !> Maximum number of parent parsers
    integer, parameter :: MAX_PARENTS = 8

    ! ============================================================================
    ! VALUE TYPE CONSTANTS
    ! ============================================================================

    !> Value type constant for string/character values
    integer, parameter :: TYPE_STRING = 1
    !> Value type constant for integer values
    integer, parameter :: TYPE_INTEGER = 2
    !> Value type constant for real/float values
    integer, parameter :: TYPE_REAL = 3
    !> Value type constant for logical/boolean values
    integer, parameter :: TYPE_LOGICAL = 4

    ! ============================================================================
    ! ERROR HANDLING TYPE
    ! ============================================================================

    !> Error type for argparse errors
    type, public :: argparse_error
        !> The error message describing what went wrong
        character(len=:), allocatable :: message
        !> The argument name or value that caused the error
        character(len=:), allocatable :: argument
        !> Flag indicating whether an error has occurred
        logical :: has_error = .false.
    contains
        procedure :: init_error => error_init
        procedure :: print_error => error_print
        procedure :: clear => error_clear
    end type argparse_error

    ! ============================================================================
    ! VALUE CONTAINER TYPE (for polymorphic storage)
    ! ============================================================================

    !> Container for storing values of different types
    !> Supports single values and lists for append actions
    type :: value_container
        !> The type of value stored (TYPE_STRING, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL)
        integer :: value_type = TYPE_STRING
        !> Storage for string values
        character(len=:), allocatable :: string_value
        !> Storage for integer values
        integer :: integer_value = 0
        !> Storage for real/float values
        real :: real_value = 0.0
        !> Storage for logical/boolean values
        logical :: logical_value = .false.
        !> Array storage for string list values (used with append action)
        character(len=MAX_ARG_LEN) :: string_list(MAX_LIST_VALUES)
        !> Array storage for integer list values (used with append action)
        integer :: integer_list(MAX_LIST_VALUES)
        !> Array storage for real list values (used with append action)
        real :: real_list(MAX_LIST_VALUES)
        !> Number of items currently stored in list arrays
        integer :: list_count = 0
        !> Flag indicating whether a value has been explicitly set
        logical :: is_set = .false.
    contains
        procedure :: set_string => value_set_string
        procedure :: set_integer => value_set_integer
        procedure :: set_real => value_set_real
        procedure :: set_logical => value_set_logical
        procedure :: append_string => value_append_string
        procedure :: append_integer => value_append_integer
        procedure :: append_real => value_append_real
        procedure :: to_string => value_to_string
    end type value_container

    ! ============================================================================
    ! ARGUMENT ENTRY TYPE (for ParsedArgs storage)
    ! ============================================================================

    !> Entry in the parsed args storing a key-value pair
    !> Used internally by ParsedArgs to store argument name-value pairs
    type :: argument_entry
        !> The argument destination name (key for lookup)
        character(len=:), allocatable :: key
        !> The value container holding the argument's value
        type(value_container) :: value
    end type argument_entry

    ! ============================================================================
    ! PARSED ARGS TYPE (Namespace equivalent)
    ! ============================================================================

    !> ParsedArgs type to store parsed arguments
    !> Similar to Python's argparse.Namespace
    !> Provides dictionary-like access to parsed argument values
    type, public :: ParsedArgs
        !> Array of argument entries storing all parsed key-value pairs
        type(argument_entry), allocatable :: entries(:)
        !> Current number of entries stored in the entries array
        integer :: num_entries = 0
    contains
        procedure :: init => parsed_args_init
        procedure :: set_string => parsed_args_set_string
        procedure :: set_integer => parsed_args_set_integer
        procedure :: set_real => parsed_args_set_real
        procedure :: set_logical => parsed_args_set_logical
        procedure :: append_string => parsed_args_append_string
        procedure :: append_integer => parsed_args_append_integer
        procedure :: increment => parsed_args_increment
        procedure :: get_string => parsed_args_get_string
        procedure :: get_integer => parsed_args_get_integer
        procedure :: get_real => parsed_args_get_real
        procedure :: get_logical => parsed_args_get_logical
        procedure :: get_string_list => parsed_args_get_string_list
        procedure :: get_integer_list => parsed_args_get_integer_list
        procedure :: has_key => parsed_args_has_key
        procedure :: print => parsed_args_print
        procedure, private :: find_or_create_entry => parsed_args_find_or_create
        procedure, private :: find_entry => parsed_args_find
    end type ParsedArgs

    ! ============================================================================
    ! ACTION INFO TYPE (simple action data storage without polymorphism)
    ! ============================================================================

    ! ============================================================================
    ! ACTION TYPE CONSTANTS
    ! ============================================================================

    !> Action type: store a single value (default action)
    integer, parameter :: ACT_STORE = 1
    !> Action type: store True when flag is present (no value consumed)
    integer, parameter :: ACT_STORE_TRUE = 2
    !> Action type: store False when flag is present (no value consumed)
    integer, parameter :: ACT_STORE_FALSE = 3
    !> Action type: count occurrences of the flag
    integer, parameter :: ACT_COUNT = 4
    !> Action type: append value to a list (allows repeated use)
    integer, parameter :: ACT_APPEND = 5
    !> Action type: print help message and exit
    integer, parameter :: ACT_HELP = 6
    !> Action type: print version string and exit
    integer, parameter :: ACT_VERSION = 7

    ! ============================================================================
    ! ARGUMENT STATUS CONSTANTS
    ! ============================================================================

    !> Status: argument is active and fully supported (default)
    integer, parameter, public :: STATUS_ACTIVE = 0
    !> Status: argument is deprecated, prints warning but still works
    integer, parameter, public :: STATUS_DEPRECATED = 1
    !> Status: argument has been removed, prints error and rejects usage
    integer, parameter, public :: STATUS_REMOVED = 2

    ! ============================================================================
    ! GROUP TYPE CONSTANTS
    ! ============================================================================

    !> Group type: standard argument group (for help organization)
    integer, parameter :: GROUP_STANDARD = 1
    !> Group type: mutually exclusive group (only one argument allowed)
    integer, parameter :: GROUP_MUTEX = 2

    !> Action info - stores all data about an argument action
    !> Represents a single argument definition with all its properties
    type :: action_info
        !> Destination name where the argument value will be stored
        character(len=:), allocatable :: dest
        !> Array of option strings (e.g., "-v", "--verbose")
        character(len=MAX_ARG_LEN) :: option_strings(MAX_OPTION_STRINGS)
        !> Number of option strings defined for this argument
        integer :: num_option_strings = 0
        !> Number of arguments consumed (NARGS_* constants or positive integer)
        integer :: nargs = NARGS_SINGLE
        !> Help text description displayed in usage/help output
        character(len=:), allocatable :: help_text
        !> Whether this argument is required (positional args are always required)
        logical :: required = .false.
        !> Default value if argument is not provided on command line
        type(value_container) :: default_value
        !> Flag indicating whether a default value has been set
        logical :: has_default = .false.
        !> Metavar for display in usage/help (e.g., "FILE" instead of "filename")
        character(len=:), allocatable :: metavar
        !> Array of valid choices that constrain acceptable values
        character(len=MAX_ARG_LEN) :: choices(MAX_CHOICES)
        !> Number of choices defined for this argument
        integer :: num_choices = 0
        !> Expected value type (TYPE_STRING, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL)
        integer :: value_type = TYPE_STRING
        !> Whether this is a positional argument (vs optional/flag)
        logical :: is_positional = .false.
        !> The action type (ACT_STORE, ACT_STORE_TRUE, ACT_COUNT, etc.)
        integer :: action_type = ACT_STORE
        !> Version string for ACT_VERSION action
        character(len=:), allocatable :: version_string
        !> Argument status (STATUS_ACTIVE, STATUS_DEPRECATED, STATUS_REMOVED)
        integer :: status = STATUS_ACTIVE
        !> Whether argument is visible in help output (hidden if .false.)
        logical :: visible = .true.
        !> Deprecation message shown when deprecated argument is used
        character(len=:), allocatable :: deprecated_message
        !> Removal message shown when removed argument is used
        character(len=:), allocatable :: removed_message
    contains
        procedure :: matches_option => action_matches_option
        procedure :: get_display_name => action_get_display_name
        procedure :: is_valid_choice => action_is_valid_choice
        procedure :: execute => action_execute
        procedure :: check_status => action_check_status
    end type action_info

    ! ============================================================================
    ! ARGUMENT GROUP TYPES
    ! ============================================================================

    !> Argument group for organizing arguments in help output
    !> Also serves as base for mutually exclusive groups
    type, public :: argument_group
        !> Group type (GROUP_STANDARD or GROUP_MUTEX)
        integer :: group_type = GROUP_STANDARD
        !> Title displayed as section header in help output
        character(len=:), allocatable :: title
        !> Description displayed below the title in help output
        character(len=:), allocatable :: description
        !> Array of destination names for actions in this group
        character(len=MAX_ARG_LEN) :: action_dests(MAX_GROUP_ACTIONS)
        !> Number of actions in this group
        integer :: num_actions = 0
        !> For mutex groups: whether at least one argument is required
        logical :: required = .false.
        !> Index of the parent parser (for internal use)
        integer :: parser_idx = 0
    contains
        procedure :: add_action_dest => group_add_action_dest
        procedure :: has_action => group_has_action
    end type argument_group

    !> Mutually exclusive group type (extends argument_group conceptually)
    !> Only one argument from this group can be used at a time
    type, public :: mutually_exclusive_group
        !> Title for the group (usually empty for mutex groups)
        character(len=:), allocatable :: title
        !> Array of destination names for actions in this group
        character(len=MAX_ARG_LEN) :: action_dests(MAX_GROUP_ACTIONS)
        !> Number of actions in this group
        integer :: num_actions = 0
        !> Whether at least one argument from this group is required
        logical :: required = .false.
    contains
        procedure :: add_action_dest => mutex_add_action_dest
        procedure :: has_action => mutex_has_action
    end type mutually_exclusive_group

    ! ============================================================================
    ! SUBPARSER CONTAINER
    ! ============================================================================

    !> Main argument parser type
    !> Handles argument definition, parsing, and help generation
    type :: ArgParser
        !> Program name displayed in usage/help (defaults to executable name)
        character(len=:), allocatable :: prog
        !> Program description displayed after usage in help output
        character(len=:), allocatable :: description
        !> Text displayed at the end of help output
        character(len=:), allocatable :: epilog
        !> Program version string for --version action
        character(len=:), allocatable :: version
        !> Global default value applied to all arguments without explicit defaults
        type(value_container) :: argument_default
        !> Whether to automatically add -h/--help argument
        logical :: add_help = .true.
        !> Array of action_info objects defining all arguments
        type(action_info), allocatable :: actions(:)
        !> Current number of arguments/actions defined
        integer :: num_actions = 0
        !> Whether subparsers have been added to this parser
        logical :: has_subparsers = .false.
        !> Destination name for storing selected subparser command
        character(len=:), allocatable :: subparser_dest
        !> Title displayed above subparser list in help output
        character(len=:), allocatable :: subparser_title
        !> Maximum line width for help text formatting
        integer :: width = 80
        !> Column position where help text descriptions start
        integer :: max_help_position = 24
        !> Storage for the most recent parsing error
        type(argparse_error) :: last_error
        !> Array of subparser command names
        character(len=MAX_ARG_LEN) :: subparser_names(MAX_SUBPARSERS)
        !> Array of subparser help text descriptions
        character(len=MAX_ARG_LEN) :: subparser_helps(MAX_SUBPARSERS)
        !> Current number of subparsers defined
        integer :: num_subparsers = 0
        !> Array of argument groups for help organization
        type(argument_group) :: groups(MAX_GROUPS)
        !> Current number of argument groups
        integer :: num_groups = 0
        !> Array of mutually exclusive groups
        type(mutually_exclusive_group) :: mutex_groups(MAX_GROUPS)
        !> Current number of mutually exclusive groups
        integer :: num_mutex_groups = 0
        !> Tracks which destinations were set during parsing (for mutex validation)
        character(len=MAX_ARG_LEN) :: seen_dests(MAX_ACTIONS)
        !> Number of destinations seen during parsing
        integer :: num_seen_dests = 0
    contains
        procedure :: init => parser_init
        procedure :: init_with_parents => parser_init_with_parents
        procedure :: add_argument => parser_add_argument
        procedure :: add_argument_group => parser_add_argument_group
        procedure :: add_mutually_exclusive_group => parser_add_mutex_group
        procedure :: add_subparsers => parser_add_subparsers
        procedure :: add_parser => parser_add_parser
        procedure :: parse_args => parser_parse_args
        procedure :: parse_args_array => parser_parse_args_array
        procedure :: print_help => parser_print_help
        procedure :: print_usage => parser_print_usage
        procedure :: format_usage => parser_format_usage
        procedure :: format_help => parser_format_help
        procedure :: error => parser_error
        procedure, private :: find_action_for_option => parser_find_action_for_option
        procedure, private :: get_positional_actions => parser_get_positional_actions
        procedure, private :: set_defaults => parser_set_defaults
        procedure, private :: check_required => parser_check_required
        procedure, private :: consume_values => parser_consume_values
        procedure, private :: copy_actions_from => parser_copy_actions_from
        procedure, private :: validate_mutex_groups => parser_validate_mutex_groups
        procedure, private :: mark_seen => parser_mark_seen
        procedure, private :: find_action_by_dest => parser_find_action_by_dest
    end type ArgParser

    public :: ArgParser

contains

    ! ============================================================================
    ! TYPE ALIAS
    ! ============================================================================

    ! Namespace is an alias for ParsedArgs (for Python compatibility naming)
    ! This is handled via the public statement above

    ! ============================================================================
    ! ERROR HANDLING IMPLEMENTATIONS
    ! ============================================================================

    !> Initialize an error with message
    subroutine error_init(self, message, argument)
        class(argparse_error), intent(inout) :: self
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: argument

        self%message = trim(message)
        self%has_error = .true.
        if (present(argument)) then
            self%argument = trim(argument)
        end if
    end subroutine error_init

    !> Print the error message
    subroutine error_print(self, unit)
        class(argparse_error), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: out_unit

        out_unit = 0  ! stderr equivalent
        if (present(unit)) out_unit = unit

        if (self%has_error) then
            if (allocated(self%argument)) then
                write(*, '(A,A,A,A)') "error: ", trim(self%message), " -- ", trim(self%argument)
            else
                write(*, '(A,A)') "error: ", trim(self%message)
            end if
        end if
    end subroutine error_print

    !> Clear the error state
    subroutine error_clear(self)
        class(argparse_error), intent(inout) :: self

        self%has_error = .false.
        if (allocated(self%message)) deallocate(self%message)
        if (allocated(self%argument)) deallocate(self%argument)
    end subroutine error_clear

    ! ============================================================================
    ! VALUE CONTAINER IMPLEMENTATIONS
    ! ============================================================================

    !> Set string value
    subroutine value_set_string(self, val)
        class(value_container), intent(inout) :: self
        character(len=*), intent(in) :: val

        self%string_value = trim(val)
        self%value_type = TYPE_STRING
        self%is_set = .true.
    end subroutine value_set_string

    !> Set integer value
    subroutine value_set_integer(self, val)
        class(value_container), intent(inout) :: self
        integer, intent(in) :: val

        self%integer_value = val
        self%value_type = TYPE_INTEGER
        self%is_set = .true.
    end subroutine value_set_integer

    !> Set real value
    subroutine value_set_real(self, val)
        class(value_container), intent(inout) :: self
        real, intent(in) :: val

        self%real_value = val
        self%value_type = TYPE_REAL
        self%is_set = .true.
    end subroutine value_set_real

    !> Set logical value
    subroutine value_set_logical(self, val)
        class(value_container), intent(inout) :: self
        logical, intent(in) :: val

        self%logical_value = val
        self%value_type = TYPE_LOGICAL
        self%is_set = .true.
    end subroutine value_set_logical

    !> Append string to list
    subroutine value_append_string(self, val)
        class(value_container), intent(inout) :: self
        character(len=*), intent(in) :: val

        if (self%list_count < MAX_LIST_VALUES) then
            self%list_count = self%list_count + 1
            self%string_list(self%list_count) = trim(val)
            self%value_type = TYPE_STRING
            self%is_set = .true.
        end if
    end subroutine value_append_string

    !> Append integer to list
    subroutine value_append_integer(self, val)
        class(value_container), intent(inout) :: self
        integer, intent(in) :: val

        if (self%list_count < MAX_LIST_VALUES) then
            self%list_count = self%list_count + 1
            self%integer_list(self%list_count) = val
            self%value_type = TYPE_INTEGER
            self%is_set = .true.
        end if
    end subroutine value_append_integer

    !> Append real to list
    subroutine value_append_real(self, val)
        class(value_container), intent(inout) :: self
        real, intent(in) :: val

        if (self%list_count < MAX_LIST_VALUES) then
            self%list_count = self%list_count + 1
            self%real_list(self%list_count) = val
            self%value_type = TYPE_REAL
            self%is_set = .true.
        end if
    end subroutine value_append_real

    !> Convert value to string representation
    function value_to_string(self) result(str)
        class(value_container), intent(in) :: self
        character(len=:), allocatable :: str
        character(len=32) :: tmp
        integer :: i

        if (.not. self%is_set) then
            str = "None"
            return
        end if

        if (self%list_count > 0) then
            ! List value
            str = "["
            do i = 1, self%list_count
                if (i > 1) str = str // ", "
                select case(self%value_type)
                case(TYPE_STRING)
                    str = str // "'" // trim(self%string_list(i)) // "'"
                case(TYPE_INTEGER)
                    write(tmp, '(I0)') self%integer_list(i)
                    str = str // trim(tmp)
                case(TYPE_REAL)
                    write(tmp, '(G0)') self%real_list(i)
                    str = str // trim(tmp)
                end select
            end do
            str = str // "]"
        else
            ! Single value
            select case(self%value_type)
            case(TYPE_STRING)
                if (allocated(self%string_value)) then
                    str = "'" // trim(self%string_value) // "'"
                else
                    str = "None"
                end if
            case(TYPE_INTEGER)
                write(tmp, '(I0)') self%integer_value
                str = trim(tmp)
            case(TYPE_REAL)
                write(tmp, '(G0)') self%real_value
                str = trim(tmp)
            case(TYPE_LOGICAL)
                if (self%logical_value) then
                    str = "True"
                else
                    str = "False"
                end if
            case default
                str = "None"
            end select
        end if
    end function value_to_string

    ! ============================================================================
    ! PARSED ARGS (NAMESPACE) IMPLEMENTATIONS
    ! ============================================================================

    !> Initialize parsed args
    subroutine parsed_args_init(self)
        class(ParsedArgs), intent(inout) :: self

        if (allocated(self%entries)) deallocate(self%entries)
        allocate(self%entries(MAX_ACTIONS))
        self%num_entries = 0
    end subroutine parsed_args_init

    !> Find or create entry for key
    function parsed_args_find_or_create(self, key) result(idx)
        class(ParsedArgs), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer :: idx
        integer :: i

        ! Search for existing entry
        do i = 1, self%num_entries
            if (allocated(self%entries(i)%key)) then
                if (self%entries(i)%key == key) then
                    idx = i
                    return
                end if
            end if
        end do

        ! Create new entry
        self%num_entries = self%num_entries + 1
        idx = self%num_entries
        self%entries(idx)%key = trim(key)
    end function parsed_args_find_or_create

    !> Find entry index for key
    function parsed_args_find(self, key) result(idx)
        class(ParsedArgs), intent(in) :: self
        character(len=*), intent(in) :: key
        integer :: idx
        integer :: i

        idx = 0
        do i = 1, self%num_entries
            if (allocated(self%entries(i)%key)) then
                if (self%entries(i)%key == key) then
                    idx = i
                    return
                end if
            end if
        end do
    end function parsed_args_find

    !> Set string value in parsed args
    subroutine parsed_args_set_string(self, key, value)
        class(ParsedArgs), intent(inout) :: self
        character(len=*), intent(in) :: key, value
        integer :: idx

        idx = self%find_or_create_entry(key)
        call self%entries(idx)%value%set_string(value)
    end subroutine parsed_args_set_string

    !> Set integer value in parsed args
    subroutine parsed_args_set_integer(self, key, value)
        class(ParsedArgs), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        integer :: idx

        idx = self%find_or_create_entry(key)
        call self%entries(idx)%value%set_integer(value)
    end subroutine parsed_args_set_integer

    !> Set real value in parsed args
    subroutine parsed_args_set_real(self, key, value)
        class(ParsedArgs), intent(inout) :: self
        character(len=*), intent(in) :: key
        real, intent(in) :: value
        integer :: idx

        idx = self%find_or_create_entry(key)
        call self%entries(idx)%value%set_real(value)
    end subroutine parsed_args_set_real

    !> Set logical value in parsed args
    subroutine parsed_args_set_logical(self, key, value)
        class(ParsedArgs), intent(inout) :: self
        character(len=*), intent(in) :: key
        logical, intent(in) :: value
        integer :: idx

        idx = self%find_or_create_entry(key)
        call self%entries(idx)%value%set_logical(value)
    end subroutine parsed_args_set_logical

    !> Append string to list in parsed args
    subroutine parsed_args_append_string(self, key, value)
        class(ParsedArgs), intent(inout) :: self
        character(len=*), intent(in) :: key, value
        integer :: idx

        idx = self%find_or_create_entry(key)
        call self%entries(idx)%value%append_string(value)
    end subroutine parsed_args_append_string

    !> Append integer to list in parsed args
    subroutine parsed_args_append_integer(self, key, value)
        class(ParsedArgs), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        integer :: idx

        idx = self%find_or_create_entry(key)
        call self%entries(idx)%value%append_integer(value)
    end subroutine parsed_args_append_integer

    !> Increment counter in parsed args
    subroutine parsed_args_increment(self, key)
        class(ParsedArgs), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer :: idx

        idx = self%find_or_create_entry(key)
        if (.not. self%entries(idx)%value%is_set) then
            self%entries(idx)%value%integer_value = 0
        end if
        self%entries(idx)%value%integer_value = self%entries(idx)%value%integer_value + 1
        self%entries(idx)%value%value_type = TYPE_INTEGER
        self%entries(idx)%value%is_set = .true.
    end subroutine parsed_args_increment

    !> Get string value from parsed args
    function parsed_args_get_string(self, key, default) result(value)
        class(ParsedArgs), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(in), optional :: default
        character(len=:), allocatable :: value
        integer :: idx
        logical :: found

        found = .false.
        idx = self%find_entry(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                found = .true.
                if (allocated(self%entries(idx)%value%string_value)) then
                    value = self%entries(idx)%value%string_value
                else if (present(default)) then
                    value = default
                else
                    value = ""
                end if
            end if
        end if
        if (.not. found) then
            if (present(default)) then
                value = default
            else
                value = ""
            end if
        end if
    end function parsed_args_get_string

    !> Get integer value from parsed args
    function parsed_args_get_integer(self, key, default) result(value)
        class(ParsedArgs), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(in), optional :: default
        integer :: value
        integer :: idx
        logical :: found

        found = .false.
        idx = self%find_entry(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                found = .true.
                value = self%entries(idx)%value%integer_value
            end if
        end if
        if (.not. found) then
            if (present(default)) then
                value = default
            else
                value = 0
            end if
        end if
    end function parsed_args_get_integer

    !> Get real value from parsed args
    function parsed_args_get_real(self, key, default) result(value)
        class(ParsedArgs), intent(in) :: self
        character(len=*), intent(in) :: key
        real, intent(in), optional :: default
        real :: value
        integer :: idx
        logical :: found

        found = .false.
        idx = self%find_entry(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                found = .true.
                value = self%entries(idx)%value%real_value
            end if
        end if
        if (.not. found) then
            if (present(default)) then
                value = default
            else
                value = 0.0
            end if
        end if
    end function parsed_args_get_real

    !> Get logical value from parsed args
    function parsed_args_get_logical(self, key, default) result(value)
        class(ParsedArgs), intent(in) :: self
        character(len=*), intent(in) :: key
        logical, intent(in), optional :: default
        logical :: value
        integer :: idx
        logical :: found

        found = .false.
        idx = self%find_entry(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                found = .true.
                value = self%entries(idx)%value%logical_value
            end if
        end if
        if (.not. found) then
            if (present(default)) then
                value = default
            else
                value = .false.
            end if
        end if
    end function parsed_args_get_logical

    !> Get string list from parsed args
    subroutine parsed_args_get_string_list(self, key, values, count)
        class(ParsedArgs), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(out) :: values(:)
        integer, intent(out) :: count
        integer :: idx, i

        count = 0
        idx = self%find_entry(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                count = min(self%entries(idx)%value%list_count, size(values))
                do i = 1, count
                    values(i) = self%entries(idx)%value%string_list(i)
                end do
            end if
        end if
    end subroutine parsed_args_get_string_list

    !> Get integer list from parsed args
    subroutine parsed_args_get_integer_list(self, key, values, count)
        class(ParsedArgs), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(out) :: values(:)
        integer, intent(out) :: count
        integer :: idx, i

        count = 0
        idx = self%find_entry(key)
        if (idx > 0) then
            if (self%entries(idx)%value%is_set) then
                count = min(self%entries(idx)%value%list_count, size(values))
                do i = 1, count
                    values(i) = self%entries(idx)%value%integer_list(i)
                end do
            end if
        end if
    end subroutine parsed_args_get_integer_list

    !> Check if key exists in parsed args
    function parsed_args_has_key(self, key) result(exists)
        class(ParsedArgs), intent(in) :: self
        character(len=*), intent(in) :: key
        logical :: exists
        integer :: idx

        idx = self%find_entry(key)
        exists = .false.
        if (idx > 0) then
            exists = self%entries(idx)%value%is_set
        end if
    end function parsed_args_has_key

    !> Print parsed args in format: Namespace(key1=value1, key2=value2, ...)
    subroutine parsed_args_print(self, unit)
        class(ParsedArgs), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: out_unit, i
        character(len=:), allocatable :: val_str

        out_unit = 6  ! stdout
        if (present(unit)) out_unit = unit

        write(out_unit, '(A)', advance='no') "Namespace("
        do i = 1, self%num_entries
            if (i > 1) write(out_unit, '(A)', advance='no') ", "
            val_str = self%entries(i)%value%to_string()
            write(out_unit, '(A,A,A)', advance='no') &
                trim(self%entries(i)%key), "=", trim(val_str)
        end do
        write(out_unit, '(A)') ")"
    end subroutine parsed_args_print

    ! ============================================================================
    ! ACTION INFO IMPLEMENTATIONS
    ! ============================================================================

    !> Check if action matches given option string
    function action_matches_option(self, opt) result(matches)
        class(action_info), intent(in) :: self
        character(len=*), intent(in) :: opt
        logical :: matches
        integer :: i

        matches = .false.
        do i = 1, self%num_option_strings
            if (trim(self%option_strings(i)) == trim(opt)) then
                matches = .true.
                return
            end if
        end do
    end function action_matches_option

    !> Get display name for action (longest option string or dest)
    function action_get_display_name(self) result(name)
        class(action_info), intent(in) :: self
        character(len=:), allocatable :: name
        integer :: i, max_len, idx

        if (self%num_option_strings > 0) then
            max_len = 0
            idx = 1
            do i = 1, self%num_option_strings
                if (len_trim(self%option_strings(i)) > max_len) then
                    max_len = len_trim(self%option_strings(i))
                    idx = i
                end if
            end do
            name = trim(self%option_strings(idx))
        else
            name = self%dest
        end if
    end function action_get_display_name

    !> Check if value is in choices (if choices defined)
    function action_is_valid_choice(self, value) result(valid)
        class(action_info), intent(in) :: self
        character(len=*), intent(in) :: value
        logical :: valid
        integer :: i

        if (self%num_choices == 0) then
            valid = .true.
            return
        end if

        valid = .false.
        do i = 1, self%num_choices
            if (trim(self%choices(i)) == trim(value)) then
                valid = .true.
                return
            end if
        end do
    end function action_is_valid_choice

    !> Check argument status and handle deprecated/removed arguments
    !> Returns .true. if argument can proceed, .false. if removed
    function action_check_status(self, error) result(can_proceed)
        class(action_info), intent(in) :: self
        type(argparse_error), intent(inout) :: error
        logical :: can_proceed
        character(len=:), allocatable :: display_name, msg

        can_proceed = .true.
        display_name = self%get_display_name()

        select case(self%status)
        case(STATUS_ACTIVE)
            ! Normal operation, nothing to do
            can_proceed = .true.

        case(STATUS_DEPRECATED)
            ! Print warning but allow usage
            if (allocated(self%deprecated_message)) then
                msg = self%deprecated_message
            else
                msg = "argument '" // trim(display_name) // "' is deprecated and may be removed in a future version"
            end if
            write(*, '(A,A)') "warning: ", trim(msg)
            can_proceed = .true.

        case(STATUS_REMOVED)
            ! Print error and reject usage
            if (allocated(self%removed_message)) then
                msg = self%removed_message
            else
                msg = "argument '" // trim(display_name) // "' has been removed"
            end if
            call error%init_error(msg, display_name)
            can_proceed = .false.

        case default
            can_proceed = .true.
        end select
    end function action_check_status

    !> Execute action based on action type
    subroutine action_execute(self, args, values, num_values, error)
        class(action_info), intent(in) :: self
        type(ParsedArgs), intent(inout) :: args
        character(len=*), intent(in) :: values(:)
        integer, intent(in) :: num_values
        type(argparse_error), intent(inout) :: error
        integer :: int_val, ios, i
        real :: real_val

        select case(self%action_type)
        case(ACT_STORE)
            ! Store action - stores a value
            if (num_values < 1) then
                call error%init_error("expected one argument", self%dest)
                return
            end if

            ! Validate choice if applicable
            if (.not. self%is_valid_choice(values(1))) then
                call error%init_error("invalid choice", values(1))
                return
            end if

            select case(self%value_type)
            case(TYPE_INTEGER)
                read(values(1), *, iostat=ios) int_val
                if (ios /= 0) then
                    call error%init_error("invalid integer value", values(1))
                    return
                end if
                call args%set_integer(self%dest, int_val)

            case(TYPE_REAL)
                read(values(1), *, iostat=ios) real_val
                if (ios /= 0) then
                    call error%init_error("invalid real value", values(1))
                    return
                end if
                call args%set_real(self%dest, real_val)

            case default
                call args%set_string(self%dest, values(1))
            end select

        case(ACT_STORE_TRUE)
            call args%set_logical(self%dest, .true.)

        case(ACT_STORE_FALSE)
            call args%set_logical(self%dest, .false.)

        case(ACT_COUNT)
            call args%increment(self%dest)

        case(ACT_APPEND)
            if (num_values < 1) then
                call error%init_error("expected at least one argument", self%dest)
                return
            end if

            do i = 1, num_values
                if (.not. self%is_valid_choice(values(i))) then
                    call error%init_error("invalid choice", values(i))
                    return
                end if

                select case(self%value_type)
                case(TYPE_INTEGER)
                    read(values(i), *, iostat=ios) int_val
                    if (ios /= 0) then
                        call error%init_error("invalid integer value", values(i))
                        return
                    end if
                    call args%append_integer(self%dest, int_val)

                case default
                    call args%append_string(self%dest, values(i))
                end select
            end do

        case(ACT_HELP)
            call args%set_logical("__help__", .true.)

        case(ACT_VERSION)
            if (allocated(self%version_string)) then
                write(*, '(A)') trim(self%version_string)
            end if
            stop

        end select
    end subroutine action_execute

    ! ============================================================================
    ! ARGUMENT PARSER IMPLEMENTATIONS
    ! ============================================================================

    !> Initialize parser
    subroutine parser_init(self, prog, description, epilog, add_help, version)
        class(ArgParser), intent(inout) :: self
        character(len=*), intent(in), optional :: prog
        character(len=*), intent(in), optional :: description
        character(len=*), intent(in), optional :: epilog
        logical, intent(in), optional :: add_help
        character(len=*), intent(in), optional :: version

        if (present(prog)) then
            self%prog = trim(prog)
        else
            self%prog = get_prog_name()
        end if

        if (present(description)) self%description = trim(description)
        if (present(epilog)) self%epilog = trim(epilog)
        if (present(version)) self%version = trim(version)

        self%add_help = .true.
        if (present(add_help)) self%add_help = add_help

        if (allocated(self%actions)) deallocate(self%actions)
        allocate(self%actions(MAX_ACTIONS))
        self%num_actions = 0

        self%has_subparsers = .false.
        self%num_subparsers = 0

        ! Initialize group counters
        self%num_groups = 0
        self%num_mutex_groups = 0
        self%num_seen_dests = 0

        ! Add help argument if requested
        if (self%add_help) then
            call self%add_argument("-h", "--help", action="help", &
                                   help="show this help message and exit")
        end if

        ! Add version argument if version string provided
        if (present(version)) then
            call self%add_argument("-V", "--version", action="version", &
                                   help="show program's version number and exit")
        end if
    end subroutine parser_init

    !> Add argument to parser
    subroutine parser_add_argument(self, name1, name2, name3, name4, &
                                   action, nargs, type_name, default_val, &
                                   choices, required, help, metavar, dest, &
                                   status, visible, deprecated_msg, removed_msg, &
                                   group_idx, mutex_group_idx)
        class(ArgParser), intent(inout) :: self
        character(len=*), intent(in) :: name1
        character(len=*), intent(in), optional :: name2, name3, name4
        character(len=*), intent(in), optional :: action
        integer, intent(in), optional :: nargs
        character(len=*), intent(in), optional :: type_name
        character(len=*), intent(in), optional :: default_val
        character(len=*), intent(in), optional :: choices(:)
        logical, intent(in), optional :: required
        character(len=*), intent(in), optional :: help
        character(len=*), intent(in), optional :: metavar
        character(len=*), intent(in), optional :: dest
        !> Argument status: STATUS_ACTIVE (default), STATUS_DEPRECATED, or STATUS_REMOVED
        integer, intent(in), optional :: status
        !> Whether argument appears in help output (default: .true.)
        logical, intent(in), optional :: visible
        !> Custom message for deprecated arguments
        character(len=*), intent(in), optional :: deprecated_msg
        !> Custom message for removed arguments
        character(len=*), intent(in), optional :: removed_msg
        !> Index of argument group to add this argument to (from add_argument_group)
        integer, intent(in), optional :: group_idx
        !> Index of mutex group to add this argument to (from add_mutually_exclusive_group)
        integer, intent(in), optional :: mutex_group_idx

        character(len=MAX_ARG_LEN) :: option_strings(MAX_OPTION_STRINGS)
        integer :: num_options, num_choices, i, act_type, actual_nargs, actual_type
        character(len=:), allocatable :: actual_dest
        logical :: is_positional, is_required

        ! Collect option strings
        num_options = 1
        option_strings = ""
        option_strings(1) = trim(name1)

        if (present(name2)) then
            num_options = num_options + 1
            option_strings(num_options) = trim(name2)
        end if
        if (present(name3)) then
            num_options = num_options + 1
            option_strings(num_options) = trim(name3)
        end if
        if (present(name4)) then
            num_options = num_options + 1
            option_strings(num_options) = trim(name4)
        end if

        ! Determine if positional or optional
        is_positional = (name1(1:1) /= '-')

        ! Determine destination
        if (present(dest)) then
            actual_dest = trim(dest)
        else if (is_positional) then
            actual_dest = trim(name1)
        else
            ! Use longest option without dashes
            actual_dest = ""
            do i = 1, num_options
                if (len_trim(option_strings(i)) > len(actual_dest)) then
                    actual_dest = trim(option_strings(i))
                end if
            end do
            ! Remove leading dashes
            do while (len(actual_dest) > 0 .and. actual_dest(1:1) == '-')
                actual_dest = actual_dest(2:)
            end do
            ! Replace remaining dashes with underscores
            do i = 1, len(actual_dest)
                if (actual_dest(i:i) == '-') actual_dest(i:i) = '_'
            end do
        end if

        ! Determine action type
        act_type = ACT_STORE
        if (present(action)) then
            select case(trim(action))
            case("store_true")
                act_type = ACT_STORE_TRUE
            case("store_false")
                act_type = ACT_STORE_FALSE
            case("count")
                act_type = ACT_COUNT
            case("append")
                act_type = ACT_APPEND
            case("help")
                act_type = ACT_HELP
            case("version")
                act_type = ACT_VERSION
            case default
                act_type = ACT_STORE
            end select
        end if

        ! Determine nargs
        actual_nargs = NARGS_SINGLE
        if (present(nargs)) actual_nargs = nargs

        ! For certain actions, nargs should be 0
        if (act_type == ACT_STORE_TRUE .or. act_type == ACT_STORE_FALSE .or. &
            act_type == ACT_COUNT .or. act_type == ACT_HELP .or. &
            act_type == ACT_VERSION) then
            actual_nargs = 0
        end if

        ! Determine value type
        actual_type = TYPE_STRING
        if (present(type_name)) then
            select case(trim(type_name))
            case("integer", "int")
                actual_type = TYPE_INTEGER
            case("real", "float", "double")
                actual_type = TYPE_REAL
            case("logical", "bool")
                actual_type = TYPE_LOGICAL
            end select
        end if

        ! Determine if required
        is_required = .false.
        if (present(required)) is_required = required
        if (is_positional) is_required = .true.

        ! Add to actions list
        self%num_actions = self%num_actions + 1
        self%actions(self%num_actions)%dest = actual_dest
        self%actions(self%num_actions)%num_option_strings = num_options
        do i = 1, num_options
            self%actions(self%num_actions)%option_strings(i) = option_strings(i)
        end do
        self%actions(self%num_actions)%nargs = actual_nargs
        self%actions(self%num_actions)%required = is_required
        self%actions(self%num_actions)%value_type = actual_type
        self%actions(self%num_actions)%is_positional = is_positional
        self%actions(self%num_actions)%action_type = act_type

        if (present(help)) self%actions(self%num_actions)%help_text = trim(help)
        if (present(metavar)) self%actions(self%num_actions)%metavar = trim(metavar)

        if (present(default_val)) then
            self%actions(self%num_actions)%has_default = .true.
            call self%actions(self%num_actions)%default_value%set_string(default_val)
        end if

        ! Collect choices
        if (present(choices)) then
            num_choices = min(size(choices), MAX_CHOICES)
            self%actions(self%num_actions)%num_choices = num_choices
            do i = 1, num_choices
                self%actions(self%num_actions)%choices(i) = trim(choices(i))
            end do
        end if

        ! Set version string for version action
        if (act_type == ACT_VERSION .and. allocated(self%version)) then
            self%actions(self%num_actions)%version_string = self%version
        end if

        ! Set status (active, deprecated, removed)
        if (present(status)) then
            self%actions(self%num_actions)%status = status
        else
            self%actions(self%num_actions)%status = STATUS_ACTIVE
        end if

        ! Set visibility for help output
        if (present(visible)) then
            self%actions(self%num_actions)%visible = visible
        else
            self%actions(self%num_actions)%visible = .true.
        end if

        ! Set custom deprecation message
        if (present(deprecated_msg)) then
            self%actions(self%num_actions)%deprecated_message = trim(deprecated_msg)
        end if

        ! Set custom removal message
        if (present(removed_msg)) then
            self%actions(self%num_actions)%removed_message = trim(removed_msg)
        end if

        ! Register in argument group if specified
        if (present(group_idx)) then
            if (group_idx > 0 .and. group_idx <= self%num_groups) then
                call self%groups(group_idx)%add_action_dest(actual_dest)
            end if
        end if

        ! Register in mutex group if specified
        if (present(mutex_group_idx)) then
            if (mutex_group_idx > 0 .and. mutex_group_idx <= self%num_mutex_groups) then
                call self%mutex_groups(mutex_group_idx)%add_action_dest(actual_dest)
            end if
        end if
    end subroutine parser_add_argument

    !> Add subparsers to the parser
    subroutine parser_add_subparsers(self, title, description, dest)
        class(ArgParser), intent(inout) :: self
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: description
        character(len=*), intent(in), optional :: dest

        self%has_subparsers = .true.
        self%num_subparsers = 0

        if (present(title)) then
            self%subparser_title = trim(title)
        else
            self%subparser_title = "commands"
        end if

        if (present(dest)) then
            self%subparser_dest = trim(dest)
        else
            self%subparser_dest = "command"
        end if
    end subroutine parser_add_subparsers

    !> Add a subparser (returns the name for reference - actual parsing is simple)
    subroutine parser_add_parser(self, name, help_text)
        class(ArgParser), intent(inout) :: self
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: help_text

        self%num_subparsers = self%num_subparsers + 1
        self%subparser_names(self%num_subparsers) = trim(name)

        if (present(help_text)) then
            self%subparser_helps(self%num_subparsers) = trim(help_text)
        else
            self%subparser_helps(self%num_subparsers) = ""
        end if
    end subroutine parser_add_parser

    !> Find action for given option string
    function parser_find_action_for_option(self, opt) result(idx)
        class(ArgParser), intent(in) :: self
        character(len=*), intent(in) :: opt
        integer :: idx
        integer :: i

        idx = 0
        do i = 1, self%num_actions
            if (self%actions(i)%matches_option(opt)) then
                idx = i
                return
            end if
        end do
    end function parser_find_action_for_option

    !> Get array of positional action indices
    subroutine parser_get_positional_actions(self, indices, count)
        class(ArgParser), intent(in) :: self
        integer, intent(out) :: indices(:)
        integer, intent(out) :: count
        integer :: i

        count = 0
        do i = 1, self%num_actions
            if (self%actions(i)%is_positional) then
                count = count + 1
                if (count <= size(indices)) then
                    indices(count) = i
                end if
            end if
        end do
    end subroutine parser_get_positional_actions

    !> Set default values in parsed args
    subroutine parser_set_defaults(self, args)
        class(ArgParser), intent(in) :: self
        type(ParsedArgs), intent(inout) :: args
        integer :: i

        do i = 1, self%num_actions
            select case(self%actions(i)%action_type)
            case(ACT_STORE_TRUE)
                call args%set_logical(self%actions(i)%dest, .false.)
            case(ACT_STORE_FALSE)
                call args%set_logical(self%actions(i)%dest, .true.)
            case(ACT_COUNT)
                call args%set_integer(self%actions(i)%dest, 0)
            case default
                if (self%actions(i)%has_default) then
                    if (allocated(self%actions(i)%default_value%string_value)) then
                        call args%set_string(self%actions(i)%dest, &
                                            self%actions(i)%default_value%string_value)
                    end if
                end if
            end select
        end do
    end subroutine parser_set_defaults

    !> Check that all required arguments are present
    subroutine parser_check_required(self, args, error)
        class(ArgParser), intent(in) :: self
        type(ParsedArgs), intent(in) :: args
        type(argparse_error), intent(inout) :: error
        integer :: i

        do i = 1, self%num_actions
            if (self%actions(i)%required) then
                if (.not. args%has_key(self%actions(i)%dest)) then
                    call error%init_error("the following arguments are required", &
                                          self%actions(i)%get_display_name())
                    return
                end if
            end if
        end do
    end subroutine parser_check_required

    !> Consume values for an action based on nargs
    subroutine parser_consume_values(self, action_idx, cmd_args, num_args, arg_idx, &
                                     values, num_values, error)
        class(ArgParser), intent(in) :: self
        integer, intent(in) :: action_idx
        character(len=*), intent(in) :: cmd_args(:)
        integer, intent(in) :: num_args
        integer, intent(inout) :: arg_idx
        character(len=MAX_ARG_LEN), intent(out) :: values(:)
        integer, intent(out) :: num_values
        type(argparse_error), intent(inout) :: error

        integer :: nargs_val

        num_values = 0
        nargs_val = self%actions(action_idx)%nargs

        select case(nargs_val)
        case(0)
            ! No values needed (store_true, etc.)
            num_values = 0

        case(NARGS_OPTIONAL)
            ! 0 or 1 values
            if (arg_idx <= num_args) then
                if (cmd_args(arg_idx)(1:1) /= '-') then
                    num_values = 1
                    values(1) = cmd_args(arg_idx)
                    arg_idx = arg_idx + 1
                end if
            end if

        case(NARGS_ZERO_OR_MORE)
            ! 0 or more values
            do while (arg_idx <= num_args)
                if (cmd_args(arg_idx)(1:1) == '-') exit
                num_values = num_values + 1
                values(num_values) = cmd_args(arg_idx)
                arg_idx = arg_idx + 1
            end do

        case(NARGS_ONE_OR_MORE)
            ! 1 or more values
            do while (arg_idx <= num_args)
                if (cmd_args(arg_idx)(1:1) == '-') exit
                num_values = num_values + 1
                values(num_values) = cmd_args(arg_idx)
                arg_idx = arg_idx + 1
            end do
            if (num_values < 1) then
                call error%init_error("expected at least one argument", &
                                      self%actions(action_idx)%dest)
            end if

        case(NARGS_REMAINDER)
            ! All remaining arguments
            do while (arg_idx <= num_args)
                num_values = num_values + 1
                values(num_values) = cmd_args(arg_idx)
                arg_idx = arg_idx + 1
            end do

        case default
            ! Specific number of values
            if (nargs_val > 0) then
                do while (num_values < nargs_val .and. arg_idx <= num_args)
                    if (cmd_args(arg_idx)(1:1) == '-' .and. num_values > 0) exit
                    num_values = num_values + 1
                    values(num_values) = cmd_args(arg_idx)
                    arg_idx = arg_idx + 1
                end do
                if (num_values < nargs_val) then
                    call error%init_error("expected argument(s)", &
                                          self%actions(action_idx)%dest)
                end if
            end if
        end select
    end subroutine parser_consume_values

    !> Parse command line arguments (from command line)
    function parser_parse_args(self) result(args)
        class(ArgParser), intent(inout) :: self
        type(ParsedArgs) :: args

        character(len=MAX_ARG_LEN), allocatable :: cmd_args(:)
        integer :: num_args, i

        num_args = command_argument_count()
        allocate(cmd_args(num_args))
        do i = 1, num_args
            call get_command_argument(i, cmd_args(i))
        end do

        args = self%parse_args_array(cmd_args)

        deallocate(cmd_args)
    end function parser_parse_args

    !> Parse command line arguments from array
    function parser_parse_args_array(self, cmd_args) result(args)
        class(ArgParser), intent(inout) :: self
        character(len=*), intent(in) :: cmd_args(:)
        type(ParsedArgs) :: args

        integer :: num_args, i, arg_idx, action_idx, positional_idx
        integer :: positional_indices(MAX_ACTIONS), num_positional
        character(len=MAX_ARG_LEN) :: values(MAX_LIST_VALUES)
        integer :: num_values
        character(len=MAX_ARG_LEN) :: current_arg
        logical :: found_subparser

        ! Initialize args
        call args%init()

        num_args = size(cmd_args)

        ! Reset seen destinations for mutex validation
        self%num_seen_dests = 0

        ! Set defaults
        call self%set_defaults(args)

        ! Get positional action indices
        call self%get_positional_actions(positional_indices, num_positional)
        positional_idx = 1

        ! Parse arguments
        arg_idx = 1
        do while (arg_idx <= num_args)
            current_arg = trim(cmd_args(arg_idx))

            if (len_trim(current_arg) > 0 .and. current_arg(1:1) == '-') then
                ! Optional argument
                action_idx = self%find_action_for_option(current_arg)

                if (action_idx > 0) then
                    ! Check argument status (deprecated/removed)
                    if (.not. self%actions(action_idx)%check_status(self%last_error)) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    arg_idx = arg_idx + 1

                    ! Consume values for this action
                    call self%consume_values(action_idx, cmd_args, num_args, arg_idx, &
                                            values, num_values, self%last_error)

                    if (self%last_error%has_error) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    ! Execute action
                    call self%actions(action_idx)%execute(args, values, &
                                                         num_values, self%last_error)

                    if (self%last_error%has_error) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    ! Mark this destination as seen (for mutex validation)
                    call self%mark_seen(self%actions(action_idx)%dest)

                    ! Check if help was requested
                    if (args%get_logical("__help__")) then
                        call self%print_help()
                        stop
                    end if
                else
                    ! Unknown option
                    call self%last_error%init_error("unrecognized arguments", current_arg)
                    call self%error(self%last_error%message)
                    return
                end if
            else
                ! Positional argument or subparser command
                if (self%has_subparsers .and. positional_idx > num_positional) then
                    ! Check if it's a valid subparser command
                    found_subparser = .false.
                    do i = 1, self%num_subparsers
                        if (trim(self%subparser_names(i)) == trim(current_arg)) then
                            found_subparser = .true.
                            exit
                        end if
                    end do

                    if (found_subparser) then
                        ! Store the command name
                        call args%set_string(self%subparser_dest, current_arg)
                        ! Note: For full subparser support, would need separate parser instances
                        arg_idx = arg_idx + 1
                    else
                        call self%last_error%init_error("invalid choice", current_arg)
                        call self%error(self%last_error%message)
                        return
                    end if
                else if (positional_idx <= num_positional) then
                    ! Handle positional argument
                    action_idx = positional_indices(positional_idx)

                    ! Check argument status (deprecated/removed)
                    if (.not. self%actions(action_idx)%check_status(self%last_error)) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    positional_idx = positional_idx + 1

                    call self%consume_values(action_idx, cmd_args, num_args, arg_idx, &
                                            values, num_values, self%last_error)

                    if (self%last_error%has_error) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    ! Execute action
                    call self%actions(action_idx)%execute(args, values, &
                                                         num_values, self%last_error)

                    if (self%last_error%has_error) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    ! Mark this destination as seen (for mutex validation)
                    call self%mark_seen(self%actions(action_idx)%dest)
                else
                    ! Too many positional arguments
                    call self%last_error%init_error("unrecognized arguments", current_arg)
                    call self%error(self%last_error%message)
                    return
                end if
            end if
        end do

        ! Validate mutually exclusive groups
        call self%validate_mutex_groups(self%last_error)
        if (self%last_error%has_error) then
            call self%error(self%last_error%message)
            return
        end if

        ! Check required arguments
        call self%check_required(args, self%last_error)

        if (self%last_error%has_error) then
            call self%error(self%last_error%message)
        end if
    end function parser_parse_args_array

    !> Format usage string
    function parser_format_usage(self) result(usage)
        class(ArgParser), intent(in) :: self
        character(len=:), allocatable :: usage
        character(len=:), allocatable :: prog_name, mutex_str
        integer :: i, j, k, m
        logical :: first_in_mutex, is_in_any_mutex
        logical :: mutex_shown(MAX_GROUPS)

        if (allocated(self%prog)) then
            prog_name = self%prog
        else
            prog_name = get_prog_name()
        end if

        usage = "usage: " // trim(prog_name)

        ! Track which mutex groups we've shown
        mutex_shown = .false.
        ! Add optional arguments (checking for mutex groups)
        do i = 1, self%num_actions
            if (.not. self%actions(i)%is_positional .and. self%actions(i)%visible) then
                ! Skip help/version in brief usage
                if (self%actions(i)%action_type == ACT_HELP .or. &
                    self%actions(i)%action_type == ACT_VERSION) cycle

                ! Check if this action is in a mutex group
                is_in_any_mutex = .false.
                do j = 1, self%num_mutex_groups
                    if (self%mutex_groups(j)%has_action(self%actions(i)%dest)) then
                        is_in_any_mutex = .true.
                        ! Only show mutex group once
                        if (.not. mutex_shown(j)) then
                            mutex_shown(j) = .true.
                            ! Format mutex group as (--opt1 | --opt2)
                            if (self%mutex_groups(j)%required) then
                                mutex_str = " ("
                            else
                                mutex_str = " ["
                            end if
                            first_in_mutex = .true.
                            do k = 1, self%mutex_groups(j)%num_actions
                                ! Find action for this dest
                                do m = 1, self%num_actions
                                    if (allocated(self%actions(m)%dest)) then
                                        if (trim(self%actions(m)%dest) == &
                                            trim(self%mutex_groups(j)%action_dests(k))) then
                                            if (.not. first_in_mutex) mutex_str = mutex_str // " | "
                                            first_in_mutex = .false.
                                            if (self%actions(m)%num_option_strings > 0) then
                                                mutex_str = mutex_str // trim(self%actions(m)%option_strings(1))
                                            else
                                                mutex_str = mutex_str // trim(self%actions(m)%dest)
                                            end if
                                            exit
                                        end if
                                    end if
                                end do
                            end do
                            if (self%mutex_groups(j)%required) then
                                mutex_str = mutex_str // ")"
                            else
                                mutex_str = mutex_str // "]"
                            end if
                            usage = usage // mutex_str
                        end if
                        exit
                    end if
                end do

                ! If not in any mutex group, add normally
                if (.not. is_in_any_mutex) then
                    usage = usage // " [" // trim(self%actions(i)%option_strings(1)) // "]"
                end if
            end if
        end do

        ! Add positional arguments
        do i = 1, self%num_actions
            if (self%actions(i)%is_positional) then
                if (allocated(self%actions(i)%metavar)) then
                    usage = usage // " " // self%actions(i)%metavar
                else
                    usage = usage // " " // self%actions(i)%dest
                end if
            end if
        end do

        ! Add subparsers placeholder
        if (self%has_subparsers) then
            usage = usage // " {command}"
        end if
    end function parser_format_usage

    !> Format full help message
    function parser_format_help(self) result(help_text)
        class(ArgParser), intent(in) :: self
        character(len=:), allocatable :: help_text
        character(len=:), allocatable :: line, opt_str
        integer :: i, j, k, padding
        logical :: has_positional, has_optional, in_group
        logical :: action_shown(MAX_ACTIONS)

        ! Usage
        help_text = self%format_usage() // new_line('A')

        ! Description
        if (allocated(self%description)) then
            help_text = help_text // new_line('A') // self%description // new_line('A')
        end if

        ! Track which actions have been shown
        action_shown = .false.

        ! Check for positional and optional arguments (only visible ones)
        has_positional = .false.
        has_optional = .false.
        do i = 1, self%num_actions
            if (self%actions(i)%visible) then
                if (self%actions(i)%is_positional) then
                    has_positional = .true.
                else
                    has_optional = .true.
                end if
            end if
        end do

        ! Positional arguments
        if (has_positional) then
            help_text = help_text // new_line('A') // "positional arguments:" // new_line('A')
            do i = 1, self%num_actions
                if (self%actions(i)%is_positional .and. self%actions(i)%visible) then
                    line = "  " // self%actions(i)%dest
                    if (allocated(self%actions(i)%help_text)) then
                        padding = max(2, self%max_help_position - len(line))
                        line = line // repeat(" ", padding) // self%actions(i)%help_text
                    end if
                    ! Add deprecation notice if deprecated
                    if (self%actions(i)%status == STATUS_DEPRECATED) then
                        line = line // " (DEPRECATED)"
                    end if
                    help_text = help_text // line // new_line('A')
                    action_shown(i) = .true.
                end if
            end do
        end if

        ! Display argument groups
        do j = 1, self%num_groups
            if (self%groups(j)%num_actions > 0) then
                help_text = help_text // new_line('A') // trim(self%groups(j)%title) // ":" // new_line('A')
                if (allocated(self%groups(j)%description)) then
                    help_text = help_text // "  " // self%groups(j)%description // new_line('A')
                end if
                
                ! Show actions in this group
                do k = 1, self%groups(j)%num_actions
                    do i = 1, self%num_actions
                        if (trim(self%actions(i)%dest) == trim(self%groups(j)%action_dests(k)) .and. &
                            self%actions(i)%visible .and. .not. self%actions(i)%is_positional) then
                            ! Build option string
                            opt_str = "  "
                            do padding = 1, self%actions(i)%num_option_strings
                                if (padding > 1) opt_str = opt_str // ", "
                                opt_str = opt_str // trim(self%actions(i)%option_strings(padding))
                            end do

                            ! Add metavar if applicable
                            if (self%actions(i)%action_type == ACT_STORE .or. &
                                self%actions(i)%action_type == ACT_APPEND) then
                                if (allocated(self%actions(i)%metavar)) then
                                    opt_str = opt_str // " " // self%actions(i)%metavar
                                else
                                    opt_str = opt_str // " " // self%actions(i)%dest
                                end if
                            end if

                            line = opt_str
                            if (allocated(self%actions(i)%help_text)) then
                                padding = max(2, self%max_help_position - len(line))
                                line = line // repeat(" ", padding) // self%actions(i)%help_text
                            end if
                            if (self%actions(i)%status == STATUS_DEPRECATED) then
                                line = line // " (DEPRECATED)"
                            end if
                            help_text = help_text // line // new_line('A')
                            action_shown(i) = .true.
                        end if
                    end do
                end do
            end if
        end do

        ! Optional arguments (only those not in a group)
        if (has_optional) then
            ! Check if there are any ungrouped optional arguments
            in_group = .true.
            do i = 1, self%num_actions
                if (.not. self%actions(i)%is_positional .and. self%actions(i)%visible .and. &
                    .not. action_shown(i)) then
                    in_group = .false.
                    exit
                end if
            end do

            if (.not. in_group) then
                help_text = help_text // new_line('A') // "optional arguments:" // new_line('A')
                do i = 1, self%num_actions
                    if (.not. self%actions(i)%is_positional .and. self%actions(i)%visible .and. &
                        .not. action_shown(i)) then
                        ! Build option string
                        opt_str = "  "
                        do padding = 1, self%actions(i)%num_option_strings
                            if (padding > 1) opt_str = opt_str // ", "
                            opt_str = opt_str // trim(self%actions(i)%option_strings(padding))
                        end do

                        ! Add metavar if applicable (not for store_true/false/count/help/version)
                        if (self%actions(i)%action_type == ACT_STORE .or. &
                            self%actions(i)%action_type == ACT_APPEND) then
                            if (allocated(self%actions(i)%metavar)) then
                                opt_str = opt_str // " " // self%actions(i)%metavar
                            else
                                opt_str = opt_str // " " // self%actions(i)%dest
                            end if
                        end if

                        line = opt_str
                        if (allocated(self%actions(i)%help_text)) then
                            padding = max(2, self%max_help_position - len(line))
                            line = line // repeat(" ", padding) // self%actions(i)%help_text
                        end if
                        ! Add deprecation notice if deprecated
                        if (self%actions(i)%status == STATUS_DEPRECATED) then
                            line = line // " (DEPRECATED)"
                        end if
                        help_text = help_text // line // new_line('A')
                    end if
                end do
            end if
        end if

        ! Subparsers
        if (self%has_subparsers .and. self%num_subparsers > 0) then
            if (allocated(self%subparser_title)) then
                help_text = help_text // new_line('A') // self%subparser_title // ":" // new_line('A')
            else
                help_text = help_text // new_line('A') // "commands:" // new_line('A')
            end if
            do i = 1, self%num_subparsers
                line = "  " // trim(self%subparser_names(i))
                if (len_trim(self%subparser_helps(i)) > 0) then
                    padding = max(2, self%max_help_position - len(line))
                    line = line // repeat(" ", padding) // trim(self%subparser_helps(i))
                end if
                help_text = help_text // line // new_line('A')
            end do
        end if

        ! Epilog
        if (allocated(self%epilog)) then
            help_text = help_text // new_line('A') // self%epilog // new_line('A')
        end if
    end function parser_format_help

    !> Print help message
    subroutine parser_print_help(self)
        class(ArgParser), intent(in) :: self

        write(*, '(A)') self%format_help()
    end subroutine parser_print_help

    !> Print usage message
    subroutine parser_print_usage(self)
        class(ArgParser), intent(in) :: self

        write(*, '(A)') self%format_usage()
    end subroutine parser_print_usage

    !> Print error message and exit
    subroutine parser_error(self, message)
        class(ArgParser), intent(in) :: self
        character(len=*), intent(in) :: message

        call self%print_usage()
        write(*, '(A,A,A)') trim(self%prog), ": error: ", trim(message)
        error stop 2
    end subroutine parser_error

    ! ============================================================================
    ! GROUP METHOD IMPLEMENTATIONS
    ! ============================================================================

    !> Add an action destination to a standard argument group
    subroutine group_add_action_dest(self, dest)
        class(argument_group), intent(inout) :: self
        character(len=*), intent(in) :: dest

        if (self%num_actions < MAX_GROUP_ACTIONS) then
            self%num_actions = self%num_actions + 1
            self%action_dests(self%num_actions) = trim(dest)
        end if
    end subroutine group_add_action_dest

    !> Check if group contains an action with given destination
    function group_has_action(self, dest) result(has)
        class(argument_group), intent(in) :: self
        character(len=*), intent(in) :: dest
        logical :: has
        integer :: i

        has = .false.
        do i = 1, self%num_actions
            if (trim(self%action_dests(i)) == trim(dest)) then
                has = .true.
                return
            end if
        end do
    end function group_has_action

    !> Add an action destination to a mutually exclusive group
    subroutine mutex_add_action_dest(self, dest)
        class(mutually_exclusive_group), intent(inout) :: self
        character(len=*), intent(in) :: dest

        if (self%num_actions < MAX_GROUP_ACTIONS) then
            self%num_actions = self%num_actions + 1
            self%action_dests(self%num_actions) = trim(dest)
        end if
    end subroutine mutex_add_action_dest

    !> Check if mutex group contains an action with given destination
    function mutex_has_action(self, dest) result(has)
        class(mutually_exclusive_group), intent(in) :: self
        character(len=*), intent(in) :: dest
        logical :: has
        integer :: i

        has = .false.
        do i = 1, self%num_actions
            if (trim(self%action_dests(i)) == trim(dest)) then
                has = .true.
                return
            end if
        end do
    end function mutex_has_action

    ! ============================================================================
    ! PARENT PARSER SUPPORT
    ! ============================================================================

    !> Initialize parser with parent parsers for argument inheritance
    subroutine parser_init_with_parents(self, parents, prog, description, epilog, add_help, version)
        class(ArgParser), intent(inout) :: self
        type(ArgParser), intent(in) :: parents(:)
        character(len=*), intent(in), optional :: prog
        character(len=*), intent(in), optional :: description
        character(len=*), intent(in), optional :: epilog
        logical, intent(in), optional :: add_help
        character(len=*), intent(in), optional :: version
        integer :: i

        ! First, initialize the parser normally
        call self%init(prog=prog, description=description, epilog=epilog, &
                       add_help=add_help, version=version)

        ! Copy actions from all parent parsers
        do i = 1, size(parents)
            call self%copy_actions_from(parents(i))
        end do
    end subroutine parser_init_with_parents

    !> Copy actions from another parser (used for parent parser inheritance)
    subroutine parser_copy_actions_from(self, parent)
        class(ArgParser), intent(inout) :: self
        type(ArgParser), intent(in) :: parent
        integer :: i, j, existing_idx

        do i = 1, parent%num_actions
            ! Skip help and version actions from parent (they should be added by child)
            if (parent%actions(i)%action_type == ACT_HELP .or. &
                parent%actions(i)%action_type == ACT_VERSION) then
                cycle
            end if

            ! Check if action with same dest already exists (child overrides parent)
            existing_idx = self%find_action_by_dest(parent%actions(i)%dest)

            if (existing_idx > 0) then
                ! Override existing action
                self%actions(existing_idx) = parent%actions(i)
            else
                ! Add new action
                if (self%num_actions < MAX_ACTIONS) then
                    self%num_actions = self%num_actions + 1
                    self%actions(self%num_actions) = parent%actions(i)
                end if
            end if
        end do

        ! Also copy argument groups from parent
        do i = 1, parent%num_groups
            if (self%num_groups < MAX_GROUPS) then
                self%num_groups = self%num_groups + 1
                self%groups(self%num_groups) = parent%groups(i)
            end if
        end do

        ! Copy mutex groups from parent
        do i = 1, parent%num_mutex_groups
            if (self%num_mutex_groups < MAX_GROUPS) then
                self%num_mutex_groups = self%num_mutex_groups + 1
                self%mutex_groups(self%num_mutex_groups) = parent%mutex_groups(i)
            end if
        end do
    end subroutine parser_copy_actions_from

    !> Find action by destination name
    function parser_find_action_by_dest(self, dest) result(idx)
        class(ArgParser), intent(in) :: self
        character(len=*), intent(in) :: dest
        integer :: idx
        integer :: i

        idx = 0
        do i = 1, self%num_actions
            if (allocated(self%actions(i)%dest)) then
                if (trim(self%actions(i)%dest) == trim(dest)) then
                    idx = i
                    return
                end if
            end if
        end do
    end function parser_find_action_by_dest

    ! ============================================================================
    ! ARGUMENT GROUP METHODS
    ! ============================================================================

    !> Create and add an argument group for organizing help output
    function parser_add_argument_group(self, title, description) result(group_idx)
        class(ArgParser), intent(inout) :: self
        character(len=*), intent(in) :: title
        character(len=*), intent(in), optional :: description
        integer :: group_idx

        if (self%num_groups >= MAX_GROUPS) then
            group_idx = 0
            return
        end if

        self%num_groups = self%num_groups + 1
        group_idx = self%num_groups

        self%groups(group_idx)%group_type = GROUP_STANDARD
        self%groups(group_idx)%title = trim(title)
        self%groups(group_idx)%num_actions = 0
        self%groups(group_idx)%required = .false.

        if (present(description)) then
            self%groups(group_idx)%description = trim(description)
        end if
    end function parser_add_argument_group

    !> Create and add a mutually exclusive group
    function parser_add_mutex_group(self, required) result(group_idx)
        class(ArgParser), intent(inout) :: self
        logical, intent(in), optional :: required
        integer :: group_idx

        if (self%num_mutex_groups >= MAX_GROUPS) then
            group_idx = 0
            return
        end if

        self%num_mutex_groups = self%num_mutex_groups + 1
        group_idx = self%num_mutex_groups

        self%mutex_groups(group_idx)%num_actions = 0
        self%mutex_groups(group_idx)%required = .false.

        if (present(required)) then
            self%mutex_groups(group_idx)%required = required
        end if
    end function parser_add_mutex_group

    ! ============================================================================
    ! MUTEX VALIDATION
    ! ============================================================================

    !> Mark a destination as seen during parsing
    subroutine parser_mark_seen(self, dest)
        class(ArgParser), intent(inout) :: self
        character(len=*), intent(in) :: dest

        if (self%num_seen_dests < MAX_ACTIONS) then
            self%num_seen_dests = self%num_seen_dests + 1
            self%seen_dests(self%num_seen_dests) = trim(dest)
        end if
    end subroutine parser_mark_seen

    !> Validate mutually exclusive groups after parsing
    subroutine parser_validate_mutex_groups(self, error)
        class(ArgParser), intent(in) :: self
        type(argparse_error), intent(inout) :: error
        integer :: i, j, k, seen_count
        character(len=MAX_ARG_LEN) :: first_seen, current
        character(len=1024) :: mutex_names
        logical :: found

        do i = 1, self%num_mutex_groups
            seen_count = 0
            first_seen = ""
            mutex_names = ""

            ! Build list of mutex argument names for error message
            do j = 1, self%mutex_groups(i)%num_actions
                if (j > 1) mutex_names = trim(mutex_names) // " "
                ! Find the action to get its display name
                do k = 1, self%num_actions
                    if (allocated(self%actions(k)%dest)) then
                        if (trim(self%actions(k)%dest) == trim(self%mutex_groups(i)%action_dests(j))) then
                            if (self%actions(k)%num_option_strings > 0) then
                                mutex_names = trim(mutex_names) // trim(self%actions(k)%option_strings(1))
                            else
                                mutex_names = trim(mutex_names) // trim(self%actions(k)%dest)
                            end if
                            exit
                        end if
                    end if
                end do
            end do

            ! Count how many actions from this mutex group were seen
            do j = 1, self%mutex_groups(i)%num_actions
                current = self%mutex_groups(i)%action_dests(j)

                ! Check if this dest was seen
                found = .false.
                do k = 1, self%num_seen_dests
                    if (trim(self%seen_dests(k)) == trim(current)) then
                        found = .true.
                        exit
                    end if
                end do

                if (found) then
                    seen_count = seen_count + 1
                    if (seen_count == 1) then
                        first_seen = current
                    else
                        ! Conflict! More than one mutex argument used
                        call error%init_error("not allowed with argument " // trim(first_seen), &
                                             trim(current))
                        return
                    end if
                end if
            end do

            ! Check required constraint
            if (self%mutex_groups(i)%required .and. seen_count == 0) then
                call error%init_error("one of the arguments is required: " // trim(mutex_names), "")
                return
            end if
        end do
    end subroutine parser_validate_mutex_groups

end module fclap_argparser
