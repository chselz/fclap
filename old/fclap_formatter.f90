!> @file fclap_formatter.f90
!> @brief Formatter module for fclap - help text generation.
!>
!> @details This module handles help text generation, including usage string
!> formatting and full help message generation. It provides formatting
!> functions used by ArgumentParser to display help information.

module fclap_formatter
    use fclap_constants, only: MAX_ARG_LEN, MAX_ACTIONS, MAX_GROUPS, &
        ACT_STORE, ACT_APPEND, ACT_HELP, ACT_VERSION, &
        STATUS_DEPRECATED, TYPE_STRING
    use fclap_actions, only: Action
    implicit none
    private

    ! Public formatting procedures
    public :: format_usage_string
    public :: format_help_text

    !> @brief Argument group info for formatter.
    !>
    !> @details Lightweight structure for passing group information
    !> to formatting functions.
    type, public :: GroupInfo
        integer :: group_type = 1
        character(len=:), allocatable :: title
        character(len=:), allocatable :: description
        character(len=MAX_ARG_LEN) :: action_dests(32)
        integer :: num_actions = 0
        logical :: required = .false.
    end type GroupInfo

    !> @brief Mutex group info for formatter.
    !>
    !> @details Lightweight structure for passing mutually exclusive
    !> group information to formatting functions.
    type, public :: MutexGroupInfo
        character(len=:), allocatable :: title
        character(len=MAX_ARG_LEN) :: action_dests(32)
        integer :: num_actions = 0
        logical :: required = .false.
    end type MutexGroupInfo

contains

    !> @brief Format usage string for display.
    !>
    !> @details Generates a formatted usage line showing program name,
    !> optional arguments, positional arguments, and subcommands.
    !> Mutually exclusive groups are shown with | separators.
    !>
    !> @param prog Program name
    !> @param actions Array of registered actions
    !> @param num_actions Number of actions
    !> @param mutex_groups Array of mutually exclusive groups
    !> @param num_mutex_groups Number of mutex groups
    !> @param has_subparsers Whether subparsers are enabled
    !> @return Formatted usage string
    function format_usage_string(prog, actions, num_actions, &
                                 mutex_groups, num_mutex_groups, &
                                 has_subparsers) result(usage)
        character(len=*), intent(in) :: prog
        type(Action), intent(in) :: actions(:)
        integer, intent(in) :: num_actions
        type(MutexGroupInfo), intent(in) :: mutex_groups(:)
        integer, intent(in) :: num_mutex_groups
        logical, intent(in) :: has_subparsers
        character(len=:), allocatable :: usage

        character(len=:), allocatable :: mutex_str
        integer :: i, j, k, m
        logical :: first_in_mutex, is_in_any_mutex
        logical :: mutex_shown(MAX_GROUPS)

        ! TODO there should be like in python the possibility to give at the init stage
        ! the possibility to write a custom usage message
        usage = "usage: " // trim(prog)

        mutex_shown = .false.

        do i = 1, num_actions
            if (.not. actions(i)%is_positional .and. actions(i)%visible) then
                if (actions(i)%action_type == ACT_HELP .or. &
                    actions(i)%action_type == ACT_VERSION) cycle

                is_in_any_mutex = .false.
                do j = 1, num_mutex_groups
                    if (mutex_has_action(mutex_groups(j), actions(i)%dest)) then
                        is_in_any_mutex = .true.
                        if (.not. mutex_shown(j)) then
                            mutex_shown(j) = .true.
                            if (mutex_groups(j)%required) then
                                mutex_str = " ("
                            else
                                mutex_str = " ["
                            end if
                            first_in_mutex = .true.
                            do k = 1, mutex_groups(j)%num_actions
                                do m = 1, num_actions
                                    if (allocated(actions(m)%dest)) then
                                        if (trim(actions(m)%dest) == &
                                            trim(mutex_groups(j)%action_dests(k))) then
                                            if (.not. first_in_mutex) mutex_str = mutex_str // " | "
                                            first_in_mutex = .false.
                                            if (actions(m)%num_option_strings > 0) then
                                                mutex_str = mutex_str // trim(actions(m)%option_strings(1))
                                            else
                                                mutex_str = mutex_str // trim(actions(m)%dest)
                                            end if
                                            exit
                                        end if
                                    end if
                                end do
                            end do
                            if (mutex_groups(j)%required) then
                                mutex_str = mutex_str // ")"
                            else
                                mutex_str = mutex_str // "]"
                            end if
                            usage = usage // mutex_str
                        end if
                        exit
                    end if
                end do

                if (.not. is_in_any_mutex) then
                    usage = usage // " [" // trim(actions(i)%option_strings(1)) // "]"
                end if
            end if
        end do

        do i = 1, num_actions
            if (actions(i)%is_positional) then
                if (allocated(actions(i)%metavar)) then
                    usage = usage // " " // actions(i)%metavar
                else
                    usage = usage // " " // actions(i)%dest
                end if
            end if
        end do

        if (has_subparsers) then
            usage = usage // " {command}"
        end if
    end function format_usage_string

    function mutex_has_action(group, dest) result(has)
        type(MutexGroupInfo), intent(in) :: group
        character(len=*), intent(in) :: dest
        logical :: has
        integer :: i

        has = .false.
        do i = 1, group%num_actions
            if (trim(group%action_dests(i)) == trim(dest)) then
                has = .true.
                return
            end if
        end do
    end function mutex_has_action

    !> Format full help message
    function format_help_text(prog, description, epilog, actions, num_actions, &
                              groups, num_groups, &
                              subparser_names, subparser_helps, num_subparsers, &
                              subparser_title, has_subparsers, &
                              mutex_groups, num_mutex_groups, &
                              max_help_position) result(help_text)
        character(len=*), intent(in) :: prog
        character(len=*), intent(in), optional :: description
        character(len=*), intent(in), optional :: epilog
        type(Action), intent(in) :: actions(:)
        integer, intent(in) :: num_actions
        type(GroupInfo), intent(in) :: groups(:)
        integer, intent(in) :: num_groups
        character(len=*), intent(in) :: subparser_names(:)
        character(len=*), intent(in) :: subparser_helps(:)
        integer, intent(in) :: num_subparsers
        character(len=*), intent(in), optional :: subparser_title
        logical, intent(in) :: has_subparsers
        type(MutexGroupInfo), intent(in) :: mutex_groups(:)
        integer, intent(in) :: num_mutex_groups
        integer, intent(in) :: max_help_position
        character(len=:), allocatable :: help_text

        character(len=:), allocatable :: line, opt_str, help_desc, choices_desc
        integer :: i, j, k, padding, help_start
        logical :: has_positional, has_optional, in_group
        logical :: action_shown(MAX_ACTIONS)

        help_text = format_usage_string(prog, actions, num_actions, &
                                        mutex_groups, num_mutex_groups, &
                                        has_subparsers) // new_line('A')

        if (present(description)) then
            if (len_trim(description) > 0) then
                help_text = help_text // new_line('A') // trim(description) // new_line('A')
            end if
        end if

        action_shown = .false.

        has_positional = .false.
        has_optional = .false.
        do i = 1, num_actions
            if (actions(i)%visible) then
                if (actions(i)%is_positional) then
                    has_positional = .true.
                else
                    has_optional = .true.
                end if
            end if
        end do

        if (has_positional) then
            help_text = help_text // new_line('A') // "positional arguments:" // new_line('A')
            do i = 1, num_actions
                if (actions(i)%is_positional .and. actions(i)%visible) then
                    line = "  " // actions(i)%dest
                    help_start = len(line) + max(2, max_help_position - len(line))
                    help_desc = format_action_help(actions(i))
                    if (len_trim(help_desc) > 0) then
                        padding = max(2, max_help_position - len(line))
                        line = line // repeat(" ", padding) // help_desc
                    end if
                    if (actions(i)%status == STATUS_DEPRECATED) then
                        line = line // " (DEPRECATED)"
                    end if
                    help_text = help_text // line // new_line('A')
                    choices_desc = format_action_choices(actions(i))
                    if (len_trim(choices_desc) > 0) then
                        help_text = help_text // repeat(" ", help_start) // choices_desc // new_line('A')
                    end if
                    action_shown(i) = .true.
                end if
            end do
        end if

        do j = 1, num_groups
            if (groups(j)%num_actions > 0) then
                help_text = help_text // new_line('A') // trim(groups(j)%title) // ":" // new_line('A')
                if (allocated(groups(j)%description)) then
                    help_text = help_text // "  " // groups(j)%description // new_line('A')
                end if

                do k = 1, groups(j)%num_actions
                    do i = 1, num_actions
                        if (trim(actions(i)%dest) == trim(groups(j)%action_dests(k)) .and. &
                            actions(i)%visible .and. .not. actions(i)%is_positional) then
                            opt_str = "  "
                            do padding = 1, actions(i)%num_option_strings
                                if (padding > 1) opt_str = opt_str // ", "
                                opt_str = opt_str // trim(actions(i)%option_strings(padding))
                            end do

                            if (actions(i)%action_type == ACT_STORE .or. &
                                actions(i)%action_type == ACT_APPEND) then
                                if (allocated(actions(i)%metavar)) then
                                    opt_str = opt_str // " " // actions(i)%metavar
                                else
                                    opt_str = opt_str // " " // actions(i)%dest
                                end if
                            end if

                            line = opt_str
                            help_start = len(line) + max(2, max_help_position - len(line))
                            help_desc = format_action_help(actions(i))
                            if (len_trim(help_desc) > 0) then
                                padding = max(2, max_help_position - len(line))
                                line = line // repeat(" ", padding) // help_desc
                            end if
                            if (actions(i)%status == STATUS_DEPRECATED) then
                                line = line // " (DEPRECATED)"
                            end if
                            help_text = help_text // line // new_line('A')
                            choices_desc = format_action_choices(actions(i))
                            if (len_trim(choices_desc) > 0) then
                                help_text = help_text // repeat(" ", help_start) // choices_desc // new_line('A')
                            end if
                            action_shown(i) = .true.
                        end if
                    end do
                end do
            end if
        end do

        if (has_optional) then
            in_group = .true.
            do i = 1, num_actions
                if (.not. actions(i)%is_positional .and. actions(i)%visible .and. &
                    .not. action_shown(i)) then
                    in_group = .false.
                    exit
                end if
            end do

            if (.not. in_group) then
                help_text = help_text // new_line('A') // "optional arguments:" // new_line('A')
                do i = 1, num_actions
                    if (.not. actions(i)%is_positional .and. actions(i)%visible .and. &
                        .not. action_shown(i)) then
                        opt_str = "  "
                        do padding = 1, actions(i)%num_option_strings
                            if (padding > 1) opt_str = opt_str // ", "
                            opt_str = opt_str // trim(actions(i)%option_strings(padding))
                        end do

                        if (actions(i)%action_type == ACT_STORE .or. &
                            actions(i)%action_type == ACT_APPEND) then
                            if (allocated(actions(i)%metavar)) then
                                opt_str = opt_str // " " // actions(i)%metavar
                            else
                                opt_str = opt_str // " " // actions(i)%dest
                            end if
                        end if

                        line = opt_str
                        help_start = len(line) + max(2, max_help_position - len(line))
                        help_desc = format_action_help(actions(i))
                        if (len_trim(help_desc) > 0) then
                            padding = max(2, max_help_position - len(line))
                            line = line // repeat(" ", padding) // help_desc
                        end if
                        if (actions(i)%status == STATUS_DEPRECATED) then
                            line = line // " (DEPRECATED)"
                        end if
                        help_text = help_text // line // new_line('A')
                        choices_desc = format_action_choices(actions(i))
                        if (len_trim(choices_desc) > 0) then
                            help_text = help_text // repeat(" ", help_start) // choices_desc // new_line('A')
                        end if
                    end if
                end do
            end if
        end if

        if (has_subparsers .and. num_subparsers > 0) then
            if (present(subparser_title)) then
                help_text = help_text // new_line('A') // trim(subparser_title) // ":" // new_line('A')
            else
                help_text = help_text // new_line('A') // "commands:" // new_line('A')
            end if
            do i = 1, num_subparsers
                line = "  " // trim(subparser_names(i))
                if (len_trim(subparser_helps(i)) > 0) then
                    padding = max(2, max_help_position - len(line))
                    line = line // repeat(" ", padding) // trim(subparser_helps(i))
                end if
                help_text = help_text // line // new_line('A')
            end do
        end if

        if (present(epilog)) then
            if (len_trim(epilog) > 0) then
                help_text = help_text // new_line('A') // trim(epilog) // new_line('A')
            end if
        end if
    end function format_help_text

    !> @brief Build help text for a single action, including optional default.
    !>
    !> @details Returns the action help description and appends a
    !> `(default: ...)` suffix when a default is set and `print_default`
    !> is enabled for the action.
    !>
    !> @param act Action metadata used to construct help text
    !> @return Final help description for this action
    function format_action_help(act) result(help_desc)
        type(Action), intent(in) :: act
        character(len=:), allocatable :: help_desc
        character(len=:), allocatable :: default_str

        help_desc = ""
        if (allocated(act%help_text)) then
            help_desc = act%help_text
        end if

        if (act%has_default .and. act%print_default) then
            default_str = act%default_value%to_string()
            if (len_trim(help_desc) > 0) then
                help_desc = trim(help_desc) // " "
            end if
            help_desc = trim(help_desc) // " (default: " // trim(default_str) // ")"
        end if
    end function format_action_help

    !> @brief Build formatted choices annotation for a single action.
    !>
    !> @details Returns an empty string unless `print_choices` is enabled and
    !> the action has one or more choices. String choices are quoted while
    !> non-string choices are emitted without quotes.
    !>
    !> @param act Action metadata containing choice values and display flags
    !> @return Formatted choices text, e.g. `[choices: 'a', 'b']`, or empty
    function format_action_choices(act) result(choices_desc)
        type(Action), intent(in) :: act
        character(len=:), allocatable :: choices_desc
        integer :: i

        choices_desc = ""
        if (.not. act%print_choices) return
        if (act%num_choices <= 0) return

        choices_desc = "[choices: "
        do i = 1, act%num_choices
            if (i > 1) choices_desc = choices_desc // ", "
            if (act%value_type == TYPE_STRING) then
                choices_desc = choices_desc // "'" // trim(act%choices(i)) // "'"
            else
                choices_desc = choices_desc // trim(act%choices(i))
            end if
        end do
        choices_desc = choices_desc // "]"
    end function format_action_choices

end module fclap_formatter
