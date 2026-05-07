!> @file fclap_parser.f90
!> @brief Parser module for fclap - the core command-line parsing engine.
!>
!> This module defines the main ArgumentParser type and provides
!> all functionality for defining arguments, parsing command-line input,
!> and generating help messages.
!>
!> @details The ArgumentParser is the central class of fclap. It handles:
!> - Registration of positional and optional arguments
!> - Parsing of command-line input into a Namespace object
!> - Generation of help and usage messages
!> - Support for subparsers (subcommands)
!> - Argument groups and mutually exclusive groups
!>
!> @example
!>   use fclap, only: ArgumentParser, Namespace
!>   type(ArgumentParser) :: parser
!>   type(Namespace) :: args
!>   call parser%init(prog="myapp", description="My application")
!>   call parser%add_argument("filename", help="Input file")
!>   args = parser%parse_args()

module fclap_parser
    use fclap_constants
    use fclap_utils_accuracy, only: wp
    use fclap_errors, only: fclap_error
    use fclap_namespace, only: Namespace, ValueContainer
    use fclap_actions, only: Action, not_less_than, not_bigger_than
    use fclap_formatter, only: format_usage_string, format_help_text, &
        GroupInfo, MutexGroupInfo
    implicit none
    private

    ! ============================================================================
    ! ARGUMENT GROUP TYPES
    ! ============================================================================

    !> @brief Argument group for organizing related arguments in help output.
    !>
    !> @details ArgumentGroup allows you to organize arguments into logical groups
    !> that appear together in the help output with a custom title and description.
    !> This is purely for documentation purposes and does not affect parsing behavior.
    type, public :: ArgumentGroup
        !> @brief Type of the group (GROUP_STANDARD or GROUP_MUTEX)
        integer :: group_type = GROUP_STANDARD
        !> @brief Title displayed above the group in help output
        character(len=:), allocatable :: title
        !> @brief Optional description text for the group
        character(len=:), allocatable :: description
        !> @brief Array of destination names for actions in this group
        character(len=MAX_ARG_LEN) :: action_dests(MAX_GROUP_ACTIONS)
        !> @brief Current number of actions in the group
        integer :: num_actions = 0
        !> @brief Whether at least one argument in the group is required
        logical :: required = .false.
        !> @brief Index of the parent parser
        integer :: parser_idx = 0
    contains
        !> @brief Add an action destination to this group
        procedure :: add_action_dest => group_add_action_dest
        !> @brief Check if an action is in this group
        procedure :: has_action => group_has_action
    end type ArgumentGroup

    !> @brief Mutually exclusive group type.
    !>
    !> @details A MutuallyExclusiveGroup ensures that only one of its member
    !> arguments can be used at a time. If the user specifies more than one,
    !> a parsing error will occur.
    !>
    !> @example
    !>   mutex = parser%add_mutually_exclusive_group(required=.true.)
    !>   call parser%add_argument("-v", "--verbose", mutex_group_idx=mutex)
    !>   call parser%add_argument("-q", "--quiet", mutex_group_idx=mutex)
    type, public :: MutuallyExclusiveGroup
        !> @brief Optional title for the group
        character(len=:), allocatable :: title
        !> @brief Array of destination names for actions in this group
        character(len=MAX_ARG_LEN) :: action_dests(MAX_GROUP_ACTIONS)
        !> @brief Current number of actions in the group
        integer :: num_actions = 0
        !> @brief Whether exactly one argument must be provided
        logical :: required = .false.
    contains
        !> @brief Add an action destination to this mutex group
        procedure :: add_action_dest => mutex_add_action_dest
        !> @brief Check if an action is in this mutex group
        procedure :: has_action => mutex_has_action
    end type MutuallyExclusiveGroup

    ! ============================================================================
    ! ARGUMENT PARSER TYPE
    ! ============================================================================

    !> @brief The core class for parsing command line arguments.
    !>
    !> @details This class handles the registration of arguments, parsing of the
    !> command line, and generation of help messages. It supports positional
    !> arguments, optional flags, subcommands, argument groups, and mutually
    !> exclusive options.
    !>
    !> @example
    !>   type(ArgumentParser) :: parser
    !>   call parser%init(prog="myapp", description="My application")
    !>   call parser%add_argument("filename", help="Input file")
    !>   call parser%add_argument("-v", "--verbose", action="store_true")
    !>   args = parser%parse_args()
    type, public :: ArgumentParser
        !> @brief The name of the program (e.g., from argv(0))
        character(len=:), allocatable :: prog
        !> @brief Description text displayed before arguments in help output
        character(len=:), allocatable :: description
        !> @brief Epilog text displayed after arguments in help output
        character(len=:), allocatable :: epilog
        !> @brief Version string for --version output
        character(len=:), allocatable :: version
        !> @brief Default value container for arguments without explicit defaults
        type(ValueContainer) :: argument_default
        !> @brief Whether to automatically add -h/--help argument
        logical :: add_help = .true.
        !> @brief Array of registered argument actions
        type(Action), allocatable :: actions(:)
        !> @brief Current number of registered actions
        integer :: num_actions = 0
        !> @brief Whether subparsers have been enabled
        logical :: has_subparsers = .false.
        !> @brief Destination name for subparser command
        character(len=:), allocatable :: subparser_dest
        !> @brief Title for subparser section in help
        character(len=:), allocatable :: subparser_title
        !> @brief Width for help text formatting
        integer :: width = 80
        !> @brief Maximum position for help text column
        integer :: max_help_position = 24
        !> @brief Last error that occurred during parsing
        type(fclap_error) :: last_error
        !> @brief Array of registered subparser names
        character(len=MAX_ARG_LEN) :: subparser_names(MAX_SUBPARSERS)
        !> @brief Array of help texts for subparsers
        character(len=MAX_ARG_LEN) :: subparser_helps(MAX_SUBPARSERS)
        !> @brief Current number of registered subparsers
        integer :: num_subparsers = 0
        !> @brief Array of subparser ArgumentParser objects (one per subcommand)
        type(ArgumentParser), allocatable :: subparser_parsers(:)
        !> @brief Array of argument groups
        type(ArgumentGroup) :: groups(MAX_GROUPS)
        !> @brief Current number of argument groups
        integer :: num_groups = 0
        !> @brief Array of mutually exclusive groups
        type(MutuallyExclusiveGroup) :: mutex_groups(MAX_GROUPS)
        !> @brief Current number of mutually exclusive groups
        integer :: num_mutex_groups = 0
        !> @brief Array of seen destination names during parsing
        character(len=MAX_ARG_LEN) :: seen_dests(MAX_ACTIONS)
        !> @brief Number of seen destinations
        integer :: num_seen_dests = 0
    contains
        !> @brief Initialize the parser with optional configuration.
        !> @param prog Optional program name (defaults to argv(0))
        !> @param description Optional description for help output
        !> @param epilog Optional text after arguments in help
        !> @param add_help Whether to add -h/--help (default: .true.)
        !> @param version Optional version string for --version
        procedure :: init => parser_init
        !> @brief Initialize parser inheriting from parent parsers.
        !> @param parents Array of parent parsers to inherit arguments from
        procedure :: init_with_parents => parser_init_with_parents
        !> @brief Add a new argument to the parser.
        !> @param name1 First option string or positional name
        !> @param action Optional action type (store, store_true, count, etc.)
        !> @param nargs Number of arguments to consume
        !> @param data_type Value type (string, int, real, logical)
        !> @param default_val Default value if not provided
        !> @param print_default Whether to show default in help text when set
        !> @param print_choices Whether to show choices list in help text
        !> @param choices Array of valid choices
        !> @param required Whether argument is required
        !> @param help Help text for the argument
        procedure :: add_argument => parser_add_argument
        !> @brief Create a new argument group for organizing help output.
        !> @param title Title for the group
        !> @param description Optional description text
        !> @return Index of the created group
        procedure :: add_argument_group => parser_add_argument_group
        !> @brief Create a mutually exclusive group.
        !> @param required Whether one option must be chosen
        !> @return Index of the created mutex group
        procedure :: add_mutually_exclusive_group => parser_add_mutex_group
        !> @brief Enable subparser (subcommand) support.
        !> @param title Title for subcommands section
        !> @param dest Destination name for the command
        procedure :: add_subparsers => parser_add_subparsers
        !> @brief Add a subparser (subcommand) with its own ArgumentParser.
        !> @param name Name of the subcommand
        !> @param subparser The ArgumentParser for this subcommand
        !> @param help_text Help text for the subcommand
        procedure :: add_parser => parser_add_parser
        !> @brief Parse command line arguments from the system.
        !> @return Namespace containing parsed values
        procedure :: parse_args => parser_parse_args
        !> @brief Parse arguments from an array of strings.
        !> @param argv Array of argument strings
        !> @return Namespace containing parsed values
        procedure :: parse_args_array => parser_parse_args_array
        !> @brief Print the help message to stdout.
        procedure :: print_help => parser_print_help
        !> @brief Print the usage message to stdout.
        procedure :: print_usage => parser_print_usage
        !> @brief Format and return the usage string.
        !> @return Formatted usage string
        procedure :: format_usage => parser_format_usage
        !> @brief Format and return the full help text.
        !> @return Formatted help text string
        procedure :: format_help => parser_format_help
        !> @brief Report a parsing error and exit.
        !> @param message Error message to display
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
    end type ArgumentParser

    public :: get_prog_name

contains

    ! ============================================================================
    ! UTILITY FUNCTIONS
    ! ============================================================================

    !> @brief Get the program name from the command line.
    !>
    !> @details Retrieves the program name from argv(0), stripping any path
    !> components to return just the executable name. If an override is
    !> provided, it will be used instead.
    !>
    !> @param override Optional custom program name to use
    !> @return The program name string
    function get_prog_name(override) result(prog_name)
        character(len=*), intent(in), optional :: override
        character(len=:), allocatable :: prog_name
        character(len=:), allocatable :: arg0
        integer :: length, status, idx

        if (present(override)) then
            prog_name = trim(override)
            return
        end if

        call get_command_argument(0, length=length, status=status)

        if (status == 0 .and. length > 0) then
            allocate(character(len=length) :: arg0)
            call get_command_argument(0, value=arg0, status=status)
            
            idx = scan(arg0, '/', back=.true.)
            if (idx == 0) idx = scan(arg0, '\', back=.true.)
            
            if (idx > 0) then
                prog_name = arg0(idx+1:)
            else
                prog_name = trim(arg0)
            end if
        else
            prog_name = "program"
        end if
    end function get_prog_name

    ! ============================================================================
    ! GROUP METHOD IMPLEMENTATIONS
    ! ============================================================================

    !> @brief Add an action destination to the argument group.
    !>
    !> @param self The ArgumentGroup instance
    !> @param dest The destination name of the action to add
    subroutine group_add_action_dest(self, dest)
        class(ArgumentGroup), intent(inout) :: self
        character(len=*), intent(in) :: dest

        if (self%num_actions < MAX_GROUP_ACTIONS) then
            self%num_actions = self%num_actions + 1
            self%action_dests(self%num_actions) = trim(dest)
        end if
    end subroutine group_add_action_dest

    !> @brief Check if an action is in this argument group.
    !>
    !> @param self The ArgumentGroup instance
    !> @param dest The destination name to check for
    !> @return .true. if the action is in the group, .false. otherwise
    function group_has_action(self, dest) result(has)
        class(ArgumentGroup), intent(in) :: self
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

    !> @brief Add an action destination to the mutually exclusive group.
    !>
    !> @param self The MutuallyExclusiveGroup instance
    !> @param dest The destination name of the action to add
    subroutine mutex_add_action_dest(self, dest)
        class(MutuallyExclusiveGroup), intent(inout) :: self
        character(len=*), intent(in) :: dest

        if (self%num_actions < MAX_GROUP_ACTIONS) then
            self%num_actions = self%num_actions + 1
            self%action_dests(self%num_actions) = trim(dest)
        end if
    end subroutine mutex_add_action_dest

    !> @brief Check if an action is in this mutually exclusive group.
    !>
    !> @param self The MutuallyExclusiveGroup instance
    !> @param dest The destination name to check for
    !> @return .true. if the action is in the group, .false. otherwise
    function mutex_has_action(self, dest) result(has)
        class(MutuallyExclusiveGroup), intent(in) :: self
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
    ! PARSER IMPLEMENTATIONS
    ! ============================================================================

    !> @brief Initialize the argument parser.
    !>
    !> @details Sets up the parser with the given configuration options.
    !> By default, -h/--help is automatically added. If version is provided,
    !> --version is also added.
    !>
    !> @param self The ArgumentParser instance
    !> @param prog Optional program name (defaults to argv(0))
    !> @param description Optional description text for help output
    !> @param epilog Optional text displayed after arguments in help
    !> @param add_help Whether to add -h/--help (default: .true.)
    !> @param version Optional version string (adds -V/--version if provided)
    subroutine parser_init(self, prog, description, epilog, add_help, version)
        class(ArgumentParser), intent(inout) :: self
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
        if (allocated(self%subparser_parsers)) deallocate(self%subparser_parsers)
        self%num_groups = 0
        self%num_mutex_groups = 0
        self%num_seen_dests = 0
        call self%last_error%clear()

        if (self%add_help) then
            call self%add_argument("-h", "--help", action="help", &
                                   help="show this help message and exit")
        end if

        if (present(version)) then
            call self%add_argument("--version", action="version", &
                                   help="show program's version number and exit")
        end if
    end subroutine parser_init

    !> @brief Initialize parser with inherited arguments from parent parsers.
    !>
    !> @details Creates a new parser that inherits all arguments (except -h/--help
    !> and --version) from the specified parent parsers. This is useful for
    !> sharing common arguments across multiple parsers.
    !>
    !> @param self The ArgumentParser instance
    !> @param parents Array of parent parsers to inherit from
    !> @param prog Optional program name
    !> @param description Optional description text
    !> @param epilog Optional epilog text
    !> @param add_help Whether to add help argument
    !> @param version Optional version string
    subroutine parser_init_with_parents(self, parents, prog, description, epilog, add_help, version)
        class(ArgumentParser), intent(inout) :: self
        type(ArgumentParser), intent(in) :: parents(:)
        character(len=*), intent(in), optional :: prog
        character(len=*), intent(in), optional :: description
        character(len=*), intent(in), optional :: epilog
        logical, intent(in), optional :: add_help
        character(len=*), intent(in), optional :: version
        integer :: i

        call self%init(prog=prog, description=description, epilog=epilog, &
                       add_help=add_help, version=version)

        do i = 1, size(parents)
            call self%copy_actions_from(parents(i))
        end do
    end subroutine parser_init_with_parents

    subroutine parser_copy_actions_from(self, parent)
        class(ArgumentParser), intent(inout) :: self
        type(ArgumentParser), intent(in) :: parent
        integer :: i, existing_idx

        do i = 1, parent%num_actions
            if (parent%actions(i)%action_type == ACT_HELP .or. &
                parent%actions(i)%action_type == ACT_VERSION) then
                cycle
            end if

            existing_idx = self%find_action_by_dest(parent%actions(i)%dest)

            if (existing_idx > 0) then
                self%actions(existing_idx) = parent%actions(i)
            else
                if (self%num_actions < MAX_ACTIONS) then
                    self%num_actions = self%num_actions + 1
                    self%actions(self%num_actions) = parent%actions(i)
                end if
            end if
        end do

        do i = 1, parent%num_groups
            if (self%num_groups < MAX_GROUPS) then
                self%num_groups = self%num_groups + 1
                self%groups(self%num_groups) = parent%groups(i)
            end if
        end do

        do i = 1, parent%num_mutex_groups
            if (self%num_mutex_groups < MAX_GROUPS) then
                self%num_mutex_groups = self%num_mutex_groups + 1
                self%mutex_groups(self%num_mutex_groups) = parent%mutex_groups(i)
            end if
        end do
    end subroutine parser_copy_actions_from

    function parser_find_action_by_dest(self, dest) result(idx)
        class(ArgumentParser), intent(in) :: self
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

    subroutine parser_add_argument(self, name1, name2, name3, name4, &
                                   action, nargs, data_type, default_val, &
                                   choices, required, help, metavar, dest, &
                                   status, visible, deprecated_msg, removed_msg, &
                                   group_idx, mutex_group_idx, print_default, print_choices)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: name1
        character(len=*), intent(in), optional :: name2, name3, name4
        character(len=*), intent(in), optional :: action
        integer, intent(in), optional :: nargs
        character(len=*), intent(in), optional :: data_type
        class(*), intent(in), optional :: default_val
        character(len=*), intent(in), optional :: choices(:)
        logical, intent(in), optional :: required
        character(len=*), intent(in), optional :: help
        character(len=*), intent(in), optional :: metavar
        character(len=*), intent(in), optional :: dest
        integer, intent(in), optional :: status
        logical, intent(in), optional :: visible
        character(len=*), intent(in), optional :: deprecated_msg
        character(len=*), intent(in), optional :: removed_msg
        integer, intent(in), optional :: group_idx
        integer, intent(in), optional :: mutex_group_idx
        logical, intent(in), optional :: print_default
        logical, intent(in), optional :: print_choices

        character(len=MAX_ARG_LEN) :: option_strings(MAX_OPTION_STRINGS)
        integer :: num_options, num_choices, i, act_type, actual_nargs, actual_type
        integer :: ios, default_int
        real(wp) :: default_real
        logical :: default_logical
        character(len=:), allocatable :: actual_dest
        character(len=:), allocatable :: expected_type
        logical :: is_positional, is_required

        call self%last_error%clear()

        num_options = 1
        option_strings = ""
        option_strings(1) = trim(name1)

        ! TODO: make name a vector that has arbitrary length
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

        is_positional = (name1(1:1) /= '-')

        if (present(dest)) then
            actual_dest = trim(dest)
        else if (is_positional) then
            actual_dest = trim(name1)
        else
            actual_dest = ""
            do i = 1, num_options
                if (len_trim(option_strings(i)) > len(actual_dest)) then
                    actual_dest = trim(option_strings(i))
                end if
            end do
            do while (len(actual_dest) > 0 .and. actual_dest(1:1) == '-')
                actual_dest = actual_dest(2:)
            end do
            do i = 1, len(actual_dest)
                if (actual_dest(i:i) == '-') actual_dest(i:i) = '_'
            end do
        end if

        act_type = ACT_STORE
        if (present(action)) then
            if (len(action) > 14 .and. action(1:14) == "not_less_than:") then
                act_type = ACT_NOT_LESS_THAN
            else if (len(action) > 16 .and. action(1:16) == "not_bigger_than:") then
                act_type = ACT_NOT_BIGGER_THAN
            else
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
        end if

        actual_nargs = ARG_SINGLE
        if (present(nargs)) actual_nargs = nargs

        if (act_type == ACT_STORE_TRUE .or. act_type == ACT_STORE_FALSE .or. &
            act_type == ACT_COUNT .or. act_type == ACT_HELP .or. &
            act_type == ACT_VERSION) then
            actual_nargs = 0
        end if

        actual_type = TYPE_STRING
        if (present(data_type)) then
            select case(trim(data_type))
            case("integer", "int")
                actual_type = TYPE_INTEGER
            case("real", "float", "double")
                actual_type = TYPE_REAL
            case("logical", "bool")
                actual_type = TYPE_LOGICAL
            end select
        end if

        is_required = .false.
        if (present(required)) is_required = required
        if (is_positional) then
            ! Positional args are required by default, unless nargs allows 0 values
            if (actual_nargs == ARG_OPTIONAL .or. actual_nargs == ARG_ZERO_OR_MORE) then
                if (.not. present(required)) is_required = .false.
            else
                is_required = .true.
            end if
        end if

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
            expected_type = "string"
            select case(actual_type)
            case(TYPE_INTEGER)
                expected_type = "integer"
            case(TYPE_REAL)
                expected_type = "real"
            case(TYPE_LOGICAL)
                expected_type = "logical"
            end select

            select type(default_val)
            type is (character(len=*))
                select case(actual_type)
                case(TYPE_INTEGER)
                    read(default_val, *, iostat=ios) default_int
                    if (ios == 0) then
                        call self%actions(self%num_actions)%default_value%set_integer(default_int)
                    else
                        call self%last_error%init("invalid default value for argument '" // &
                                                  trim(actual_dest) // "': expected " // &
                                                  expected_type)
                    end if
                case(TYPE_REAL)
                    read(default_val, *, iostat=ios) default_real
                    if (ios == 0) then
                        call self%actions(self%num_actions)%default_value%set_real(default_real)
                    else
                        call self%last_error%init("invalid default value for argument '" // &
                                                  trim(actual_dest) // "': expected " // &
                                                  expected_type)
                    end if
                case(TYPE_LOGICAL)
                    read(default_val, *, iostat=ios) default_logical
                    if (ios == 0) then
                        call self%actions(self%num_actions)%default_value%set_logical(default_logical)
                    else
                        ! TODO: Add permissive logical default parsing (1/0, yes/no, on/off).
                        call self%last_error%init("invalid default value for argument '" // &
                                                  trim(actual_dest) // "': expected " // &
                                                  expected_type)
                    end if
                case default
                    call self%actions(self%num_actions)%default_value%set_string(default_val)
                end select
            type is (integer)
                if (actual_type == TYPE_INTEGER) then
                    call self%actions(self%num_actions)%default_value%set_integer(default_val)
                else
                    call self%last_error%init("invalid default type for argument '" // &
                                              trim(actual_dest) // "': expected " // expected_type)
                end if
            type is (real)
                if (actual_type == TYPE_REAL) then
                    call self%actions(self%num_actions)%default_value%set_real(real(default_val, kind=wp))
                else
                    call self%last_error%init("invalid default type for argument '" // &
                                              trim(actual_dest) // "': expected " // expected_type)
                end if
            type is (real(kind=wp))
                if (actual_type == TYPE_REAL) then
                    call self%actions(self%num_actions)%default_value%set_real(default_val)
                else
                    call self%last_error%init("invalid default type for argument '" // &
                                              trim(actual_dest) // "': expected " // expected_type)
                end if
            type is (logical)
                if (actual_type == TYPE_LOGICAL) then
                    call self%actions(self%num_actions)%default_value%set_logical(default_val)
                else
                    call self%last_error%init("invalid default type for argument '" // &
                                              trim(actual_dest) // "': expected " // expected_type)
                end if
            class default
                call self%last_error%init("unsupported default type for argument '" // &
                                          trim(actual_dest) // "'")
            end select

            if (self%last_error%has_error) then
                self%num_actions = self%num_actions - 1
                return
            end if

            self%actions(self%num_actions)%has_default = .true.
        end if

        if (present(print_default)) then
            self%actions(self%num_actions)%print_default = print_default
        end if

        if (present(print_choices)) then
            self%actions(self%num_actions)%print_choices = print_choices
        end if

        if (present(choices)) then
            num_choices = min(size(choices), MAX_CHOICES)
            self%actions(self%num_actions)%num_choices = num_choices
            do i = 1, num_choices
                self%actions(self%num_actions)%choices(i) = trim(choices(i))
            end do
        end if

        if (act_type == ACT_VERSION .and. allocated(self%version)) then
            self%actions(self%num_actions)%version_string = self%version
        end if

        ! Extract and store the bound value for not_less_than / not_bigger_than
        if (present(action)) then
            if (act_type == ACT_NOT_LESS_THAN) then
                self%actions(self%num_actions)%bound_str = action(15:)
            else if (act_type == ACT_NOT_BIGGER_THAN) then
                self%actions(self%num_actions)%bound_str = action(17:)
            end if
        end if

        if (present(status)) then
            self%actions(self%num_actions)%status = status
        else
            self%actions(self%num_actions)%status = STATUS_ACTIVE
        end if

        if (present(visible)) then
            self%actions(self%num_actions)%visible = visible
        else
            self%actions(self%num_actions)%visible = .true.
        end if

        if (present(deprecated_msg)) then
            self%actions(self%num_actions)%deprecated_message = trim(deprecated_msg)
        end if

        if (present(removed_msg)) then
            self%actions(self%num_actions)%removed_message = trim(removed_msg)
        end if

        if (present(group_idx)) then
            if (group_idx > 0 .and. group_idx <= self%num_groups) then
                call self%groups(group_idx)%add_action_dest(actual_dest)
            end if
        end if

        if (present(mutex_group_idx)) then
            if (mutex_group_idx > 0 .and. mutex_group_idx <= self%num_mutex_groups) then
                call self%mutex_groups(mutex_group_idx)%add_action_dest(actual_dest)
            end if
        end if
    end subroutine parser_add_argument

    function parser_add_argument_group(self, title, description) result(group_idx)
        class(ArgumentParser), intent(inout) :: self
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

    function parser_add_mutex_group(self, required) result(group_idx)
        class(ArgumentParser), intent(inout) :: self
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

    subroutine parser_add_subparsers(self, title, description, dest)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in), optional :: title
        character(len=*), intent(in), optional :: description
        character(len=*), intent(in), optional :: dest

        self%has_subparsers = .true.
        self%num_subparsers = 0

        ! Allocate subparser storage
        if (allocated(self%subparser_parsers)) deallocate(self%subparser_parsers)
        allocate(self%subparser_parsers(MAX_SUBPARSERS))

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

    subroutine parser_add_parser(self, name, subparser, help_text)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ArgumentParser), intent(in), optional :: subparser
        character(len=*), intent(in), optional :: help_text

        if (self%num_subparsers >= MAX_SUBPARSERS) return

        self%num_subparsers = self%num_subparsers + 1
        self%subparser_names(self%num_subparsers) = trim(name)

        if (present(help_text)) then
            self%subparser_helps(self%num_subparsers) = trim(help_text)
        else
            self%subparser_helps(self%num_subparsers) = ""
        end if

        ! Store the subparser's ArgumentParser
        if (present(subparser)) then
            self%subparser_parsers(self%num_subparsers) = subparser
            ! Set the prog name to "parent_prog name" if not already set
            if (.not. allocated(self%subparser_parsers(self%num_subparsers)%prog) .or. &
                len_trim(self%subparser_parsers(self%num_subparsers)%prog) == 0) then
                self%subparser_parsers(self%num_subparsers)%prog = &
                    trim(self%prog) // " " // trim(name)
            end if
        else
            ! Create a default sub-parser with help and prog set
            call self%subparser_parsers(self%num_subparsers)%init( &
                prog=trim(self%prog) // " " // trim(name), add_help=.true.)
        end if
    end subroutine parser_add_parser

    function parser_find_action_for_option(self, opt) result(idx)
        class(ArgumentParser), intent(in) :: self
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

    subroutine parser_get_positional_actions(self, indices, count)
        class(ArgumentParser), intent(in) :: self
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

    subroutine parser_set_defaults(self, args)
        class(ArgumentParser), intent(in) :: self
        type(Namespace), intent(inout) :: args
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
                    select case(self%actions(i)%default_value%value_type)
                    case(TYPE_INTEGER)
                        call args%set_integer(self%actions(i)%dest, &
                                              self%actions(i)%default_value%integer_value)
                    case(TYPE_REAL)
                        call args%set_real(self%actions(i)%dest, &
                                           self%actions(i)%default_value%real_value)
                    case(TYPE_LOGICAL)
                        call args%set_logical(self%actions(i)%dest, &
                                              self%actions(i)%default_value%logical_value)
                    case default
                        if (allocated(self%actions(i)%default_value%string_value)) then
                            call args%set_string(self%actions(i)%dest, &
                                                 self%actions(i)%default_value%string_value)
                        end if
                    end select
                end if
            end select
        end do
    end subroutine parser_set_defaults

    subroutine parser_check_required(self, args, error)
        class(ArgumentParser), intent(in) :: self
        type(Namespace), intent(in) :: args
        type(fclap_error), intent(inout) :: error
        integer :: i

        do i = 1, self%num_actions
            if (self%actions(i)%required) then
                if (.not. args%has_key(self%actions(i)%dest)) then
                    call error%init("the following arguments are required", &
                                    self%actions(i)%get_display_name())
                    return
                end if
            end if
        end do
    end subroutine parser_check_required

    subroutine parser_consume_values(self, action_idx, cmd_args, num_args, arg_idx, &
                                     values, num_values, error)
        class(ArgumentParser), intent(in) :: self
        integer, intent(in) :: action_idx
        character(len=*), intent(in) :: cmd_args(:)
        integer, intent(in) :: num_args
        integer, intent(inout) :: arg_idx
        character(len=MAX_ARG_LEN), intent(out) :: values(:)
        integer, intent(out) :: num_values
        type(fclap_error), intent(inout) :: error

        integer :: nargs_val

        num_values = 0
        nargs_val = self%actions(action_idx)%nargs

        select case(nargs_val)
        case(0)
            num_values = 0

        case(ARG_OPTIONAL)
            if (arg_idx <= num_args) then
                if (cmd_args(arg_idx)(1:1) /= '-') then
                    num_values = 1
                    values(1) = cmd_args(arg_idx)
                    arg_idx = arg_idx + 1
                end if
            end if

        case(ARG_ZERO_OR_MORE)
            do while (arg_idx <= num_args)
                if (cmd_args(arg_idx)(1:1) == '-') exit
                num_values = num_values + 1
                values(num_values) = cmd_args(arg_idx)
                arg_idx = arg_idx + 1
            end do

        case(ARG_ONE_OR_MORE)
            do while (arg_idx <= num_args)
                if (cmd_args(arg_idx)(1:1) == '-') exit
                num_values = num_values + 1
                values(num_values) = cmd_args(arg_idx)
                arg_idx = arg_idx + 1
            end do
            if (num_values < 1) then
                call error%init("expected at least one argument", &
                                self%actions(action_idx)%dest)
            end if

        case(ARG_REMAINDER)
            do while (arg_idx <= num_args)
                num_values = num_values + 1
                values(num_values) = cmd_args(arg_idx)
                arg_idx = arg_idx + 1
            end do

        case default
            if (nargs_val > 0) then
                do while (num_values < nargs_val .and. arg_idx <= num_args)
                    if (cmd_args(arg_idx)(1:1) == '-' .and. num_values > 0) exit
                    num_values = num_values + 1
                    values(num_values) = cmd_args(arg_idx)
                    arg_idx = arg_idx + 1
                end do
                if (num_values < nargs_val) then
                    call error%init("expected argument(s)", &
                                    self%actions(action_idx)%dest)
                end if
            end if
        end select
    end subroutine parser_consume_values

    subroutine parser_mark_seen(self, dest)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: dest

        if (self%num_seen_dests < MAX_ACTIONS) then
            self%num_seen_dests = self%num_seen_dests + 1
            self%seen_dests(self%num_seen_dests) = trim(dest)
        end if
    end subroutine parser_mark_seen

    subroutine parser_validate_mutex_groups(self, error)
        class(ArgumentParser), intent(in) :: self
        type(fclap_error), intent(inout) :: error
        integer :: i, j, k, seen_count
        character(len=MAX_ARG_LEN) :: first_seen, current
        character(len=1024) :: mutex_names
        logical :: found

        do i = 1, self%num_mutex_groups
            seen_count = 0
            first_seen = ""
            mutex_names = ""

            do j = 1, self%mutex_groups(i)%num_actions
                if (j > 1) mutex_names = trim(mutex_names) // " "
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

            do j = 1, self%mutex_groups(i)%num_actions
                current = self%mutex_groups(i)%action_dests(j)

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
                        call error%init("not allowed with argument " // trim(first_seen), &
                                       trim(current))
                        return
                    end if
                end if
            end do

            if (self%mutex_groups(i)%required .and. seen_count == 0) then
                call error%init("one of the arguments is required: " // trim(mutex_names), "")
                return
            end if
        end do
    end subroutine parser_validate_mutex_groups

    recursive function parser_parse_args(self) result(args)
        class(ArgumentParser), intent(inout) :: self
        type(Namespace) :: args

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

    recursive function parser_parse_args_array(self, cmd_args) result(args)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: cmd_args(:)
        type(Namespace) :: args

        integer :: num_args, i, arg_idx, action_idx, positional_idx
        integer :: positional_indices(MAX_ACTIONS), num_positional
        character(len=MAX_ARG_LEN) :: values(MAX_LIST_VALUES)
        integer :: num_values
        character(len=MAX_ARG_LEN) :: current_arg
        logical :: found_subparser
        integer :: sub_idx
        type(Namespace) :: sub_args
        character(len=MAX_ARG_LEN), allocatable :: remaining_args(:)
        integer :: num_remaining

        call args%init()

        num_args = size(cmd_args)
        self%num_seen_dests = 0

        call self%set_defaults(args)
        call self%get_positional_actions(positional_indices, num_positional)
        positional_idx = 1

        arg_idx = 1
        do while (arg_idx <= num_args)
            current_arg = trim(cmd_args(arg_idx))

            if (len_trim(current_arg) > 0 .and. current_arg(1:1) == '-') then
                action_idx = self%find_action_for_option(current_arg)

                if (action_idx > 0) then
                    if (.not. self%actions(action_idx)%check_status(self%last_error)) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    arg_idx = arg_idx + 1

                    call self%consume_values(action_idx, cmd_args, num_args, arg_idx, &
                                            values, num_values, self%last_error)

                    if (self%last_error%has_error) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    call self%actions(action_idx)%execute(args, values, &
                                                         num_values, self%last_error)

                    if (self%last_error%has_error) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    call self%mark_seen(self%actions(action_idx)%dest)

                    if (args%has_key("__help__")) then
                        call self%print_help()
                        stop
                    end if
                else
                    call self%last_error%init("unrecognized arguments", current_arg)
                    call self%error(self%last_error%message)
                    return
                end if
            else
                if (self%has_subparsers .and. positional_idx > num_positional) then
                    found_subparser = .false.
                    sub_idx = 0
                    do i = 1, self%num_subparsers
                        if (trim(self%subparser_names(i)) == trim(current_arg)) then
                            found_subparser = .true.
                            sub_idx = i
                            exit
                        end if
                    end do

                    if (found_subparser) then
                        ! Store the subcommand name in the namespace
                        if (allocated(self%subparser_dest)) then
                            call args%set_string(self%subparser_dest, current_arg)
                        end if
                        arg_idx = arg_idx + 1

                        ! Delegate remaining arguments to the subparser
                        if (sub_idx > 0 .and. allocated(self%subparser_parsers)) then
                            num_remaining = num_args - arg_idx + 1
                            if (num_remaining > 0) then
                                allocate(remaining_args(num_remaining))
                                do i = 1, num_remaining
                                    remaining_args(i) = cmd_args(arg_idx + i - 1)
                                end do
                                sub_args = self%subparser_parsers(sub_idx)%parse_args_array(remaining_args)
                                deallocate(remaining_args)
                            else
                                allocate(remaining_args(0))
                                sub_args = self%subparser_parsers(sub_idx)%parse_args_array(remaining_args)
                                deallocate(remaining_args)
                            end if
                            ! Merge subparser results into main namespace
                            call args%merge(sub_args)
                        end if
                        ! All remaining args consumed by subparser
                        exit
                    else
                        call self%last_error%init("invalid choice", current_arg)
                        call self%error(self%last_error%message)
                        return
                    end if
                else if (positional_idx <= num_positional) then
                    action_idx = positional_indices(positional_idx)

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

                    call self%actions(action_idx)%execute(args, values, &
                                                         num_values, self%last_error)

                    if (self%last_error%has_error) then
                        call self%error(self%last_error%message)
                        return
                    end if

                    call self%mark_seen(self%actions(action_idx)%dest)
                else
                    call self%last_error%init("unrecognized arguments", current_arg)
                    call self%error(self%last_error%message)
                    return
                end if
            end if
        end do

        call self%validate_mutex_groups(self%last_error)
        if (self%last_error%has_error) then
            call self%error(self%last_error%message)
            return
        end if

        call self%check_required(args, self%last_error)

        if (self%last_error%has_error) then
            call self%error(self%last_error%message)
        end if
    end function parser_parse_args_array

    function parser_format_usage(self) result(usage)
        class(ArgumentParser), intent(in) :: self
        character(len=:), allocatable :: usage
        type(MutexGroupInfo) :: mutex_infos(MAX_GROUPS)
        integer :: i

        do i = 1, self%num_mutex_groups
            mutex_infos(i)%num_actions = self%mutex_groups(i)%num_actions
            mutex_infos(i)%required = self%mutex_groups(i)%required
            mutex_infos(i)%action_dests = self%mutex_groups(i)%action_dests
        end do

        usage = format_usage_string(self%prog, self%actions, self%num_actions, &
                                    mutex_infos, self%num_mutex_groups, &
                                    self%has_subparsers)
    end function parser_format_usage

    function parser_format_help(self) result(help_text)
        class(ArgumentParser), intent(in) :: self
        character(len=:), allocatable :: help_text
        type(GroupInfo) :: group_infos(MAX_GROUPS)
        type(MutexGroupInfo) :: mutex_infos(MAX_GROUPS)
        character(len=:), allocatable :: desc, epil, sub_title
        integer :: i

        do i = 1, self%num_groups
            group_infos(i)%group_type = self%groups(i)%group_type
            if (allocated(self%groups(i)%title)) then
                group_infos(i)%title = self%groups(i)%title
            end if
            if (allocated(self%groups(i)%description)) then
                group_infos(i)%description = self%groups(i)%description
            end if
            group_infos(i)%num_actions = self%groups(i)%num_actions
            group_infos(i)%action_dests = self%groups(i)%action_dests
            group_infos(i)%required = self%groups(i)%required
        end do

        do i = 1, self%num_mutex_groups
            mutex_infos(i)%num_actions = self%mutex_groups(i)%num_actions
            mutex_infos(i)%required = self%mutex_groups(i)%required
            mutex_infos(i)%action_dests = self%mutex_groups(i)%action_dests
        end do

        if (allocated(self%description)) then
            desc = self%description
        else
            desc = ""
        end if

        if (allocated(self%epilog)) then
            epil = self%epilog
        else
            epil = ""
        end if

        if (allocated(self%subparser_title)) then
            sub_title = self%subparser_title
        else
            sub_title = "commands"
        end if

        help_text = format_help_text(self%prog, desc, epil, &
                                     self%actions, self%num_actions, &
                                     group_infos, self%num_groups, &
                                     self%subparser_names, self%subparser_helps, &
                                     self%num_subparsers, sub_title, &
                                     self%has_subparsers, &
                                     mutex_infos, self%num_mutex_groups, &
                                     self%max_help_position)
    end function parser_format_help

    subroutine parser_print_help(self)
        class(ArgumentParser), intent(in) :: self

        write(*, '(A)') self%format_help()
    end subroutine parser_print_help

    subroutine parser_print_usage(self)
        class(ArgumentParser), intent(in) :: self

        write(*, '(A)') self%format_usage()
    end subroutine parser_print_usage

    subroutine parser_error(self, message)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: message

        call self%print_usage()
        write(*, '(A,A,A)') trim(self%prog), ": error: ", trim(message)
        ! critical line !!! no stop should be thrown per default this could however be enabled by user but in priciple the user should just check if error is allocated (this needs to be output of parsing function)
        ! in python the definiton of the argparser has an argument (exit_on_error) which is set to true per default
        ! maybe its better to just return with an allocated error type
        error stop 2
    end subroutine parser_error

end module fclap_parser
