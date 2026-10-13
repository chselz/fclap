!> Parser definition facade: registration, help, parsing, and dispatch.
!>
!> Argument definitions are validated by `fclap_registration`; tokens are
!> consumed by `fclap_parse_engine`. This module owns parser state.
module fclap_argparser
    use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
    use fclap_actions_abstract, only : ActionType
    use fclap_actions_builtin, only : HelpAction, VersionAction, help_action, &
        version_action
    use fclap_argument, only : Argument
    use fclap_error_codes, only : FCLAP_OK, ERR_INVALID_ARGUMENT_NAME, &
        ERR_DUPLICATE_OPTION, ERR_DUPLICATE_DEST, ERR_INVALID_NARGS, &
        ERR_INCOMPATIBLE_ACTION, ERR_INVALID_GROUP, ERR_UNKNOWN_ARGUMENT, &
        ERR_INVALID_DEFAULT, &
        ERR_EXTRA_POSITIONAL, ERR_UNKNOWN_SUBCOMMAND, ERR_MISSING_SUBCOMMAND, &
        ERR_MUTEX_CONFLICT, ERR_MUTEX_REQUIRED, ERR_INVALID_VALUE, &
        ERR_INVALID_CHOICE, ERR_VALIDATION_FAILED, ERR_INVALID_RULE, &
        ERR_NO_ARGUMENTS, ERROR_WARNING
    use fclap_error_entry, only : ErrorEntry
    use fclap_error_stack, only : ErrorStack
    use fclap_formatter_abstract, only : FormatterType
    use fclap_formatter_model, only : HelpModel
    use fclap_formatter_standard, only : StandardFormatter
    use fclap_groups_abstract, only : GroupBox, GroupHandle, GroupType, &
        new_group_handle, new_group_owner_id
    use fclap_groups_helppage, only : ArgumentGroup
    use fclap_groups_mutex, only : MutexGroup
    use fclap_namespace, only : Namespace
    use fclap_nargs, only : NARGS_REMAINDER
    use fclap_parse_engine, only : parse_definitions
    use fclap_parse_result, only : ParseResult, PARSE_SUCCESS, PARSE_FAILURE, &
        PARSE_HELP, PARSE_VERSION, SOURCE_COMMAND_LINE
    use fclap_registration, only : check_names, build_argument, &
        any_registered_name, check_env_source, check_global, check_delimiter
    use fclap_rules, only : ArgumentRule, RULE_REQUIRES, RULE_CONFLICTS, &
        RULE_REQUIRED_UNLESS, rule_kind_name
    use fclap_validators_abstract, only : ValidatorType
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : ListValue, StringValue
    implicit none
    private

    public :: ArgumentParser

    type :: ArgumentParser
        private
        character(len=:), allocatable :: prog
        character(len=:), allocatable :: usage
        character(len=:), allocatable :: description
        character(len=:), allocatable :: epilog
        character(len=:), allocatable :: version
        class(FormatterType), allocatable :: formatter
        logical :: add_help = .true.
        !> Show help instead of parsing an empty command line
        logical :: help_on_empty = .false.
        integer :: group_owner = 0
        type(Argument), allocatable :: arguments(:)
        type(GroupBox), allocatable :: groups(:)
        type(ArgumentRule), allocatable :: rules(:)
        type(ErrorStack) :: config_errors
        logical :: subparsers_enabled = .false.
        logical :: subparsers_required = .false.
        character(len=:), allocatable :: subparser_dest
        character(len=:), allocatable :: subparser_title
        character(len=:), allocatable :: subparser_description
        character(len=:), allocatable :: subparser_names(:)
        character(len=:), allocatable :: subparser_helps(:)
        !> Alternative subcommand names, each resolving to `alias_targets`
        character(len=:), allocatable :: alias_names(:)
        !> Index into `subparsers` of the command each alias names
        integer, allocatable :: alias_targets(:)
        type(ArgumentParser), allocatable :: subparsers(:)
    contains
        procedure :: init => parser_init
        procedure :: init_with_parents => parser_init_with_parents
        procedure :: add_argument => parser_add_argument
        procedure :: add_argument_group => parser_add_argument_group
        procedure :: add_mutually_exclusive_group => parser_add_mutex_group
        procedure, private :: parser_add_requires_one
        procedure, private :: parser_add_requires_many
        generic :: add_requires => parser_add_requires_one, &
            parser_add_requires_many
        procedure, private :: parser_add_conflict_one
        procedure, private :: parser_add_conflict_many
        generic :: add_conflict => parser_add_conflict_one, &
            parser_add_conflict_many
        procedure, private :: parser_add_required_unless_one
        procedure, private :: parser_add_required_unless_many
        generic :: add_required_unless => parser_add_required_unless_one, &
            parser_add_required_unless_many
        procedure :: add_subparsers => parser_add_subparsers
        procedure :: add_parser => parser_add_parser
        procedure :: argument_count => parser_argument_count
        procedure :: group_count => parser_group_count
        procedure :: rule_count => parser_rule_count
        procedure :: subparser_count => parser_subparser_count
        procedure :: get_argument => parser_get_argument
        procedure :: has_option => parser_has_option
        procedure :: has_dest => parser_has_dest
        procedure :: is_valid => parser_is_valid
        procedure :: get_config_errors => parser_get_config_errors
        procedure :: get_help_model => parser_get_help_model
        procedure :: format_usage => parser_format_usage
        procedure :: format_help => parser_format_help
        procedure :: print_usage => parser_print_usage
        procedure :: print_help => parser_print_help
        procedure :: parse_tokens => parser_parse_tokens
        procedure :: parse_known_tokens => parser_parse_known_tokens
        procedure :: try_parse_args => parser_try_parse_args
        procedure :: try_parse_known_args => parser_try_parse_known_args
        procedure :: parse_args => parser_parse_args
        procedure :: parse_known_args => parser_parse_known_args
        procedure, private :: parse_level => parser_parse_level
        procedure, private :: finish_process_parse => &
            parser_finish_process_parse
        procedure :: format_diagnostic => parser_format_diagnostic
        procedure :: error => parser_error
        procedure, private :: abort_invalid_definition => &
            parser_abort_invalid_definition
        procedure, private :: program_name => parser_program_name
        procedure :: print_diagnostics => parser_print_diagnostics
        procedure, private :: display_label => parser_display_label
        procedure, private :: append_argument => parser_append_argument
        procedure, private :: append_group => parser_append_group
        procedure, private :: add_rule => parser_add_rule
        procedure, private :: append_rule => parser_append_rule
        procedure, private :: find_reference => parser_find_reference
        procedure, private :: append_subparser => parser_append_subparser
        procedure, private :: append_alias => parser_append_alias
        procedure, private :: selectable_names => parser_selectable_names
        procedure, private :: find_subparser => parser_find_subparser
        procedure, private :: has_global_conflict => parser_has_global_conflict
        procedure, private :: inherit_global => parser_inherit_global
        procedure, private :: is_mutex_group => parser_is_mutex_group
        procedure, private :: copy_parent => parser_copy_parent
        procedure, private :: clone_from => parser_clone_from
        procedure, private :: refresh_owner_ids => parser_refresh_owner_ids
        procedure, private :: rebase_program => parser_rebase_program
        procedure, private :: reject => parser_reject
        procedure, private :: clear => parser_clear
    end type ArgumentParser

contains

    subroutine parser_init(self, prog, usage, description, epilog, version, &
        formatter, add_help, help_on_empty)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in), optional :: prog, usage, description
        character(len=*), intent(in), optional :: epilog, version
        class(FormatterType), intent(in), optional :: formatter
        logical, intent(in), optional :: add_help
        !> Treat an empty command line as a request for help (exit status 2)
        logical, intent(in), optional :: help_on_empty

        call self%clear()
        self%group_owner = new_group_owner_id()
        self%add_help = .true.
        if (present(add_help)) self%add_help = add_help
        if (present(help_on_empty)) self%help_on_empty = help_on_empty

        if (present(prog)) then
            self%prog = trim(prog)
        else
            self%prog = get_prog_name()
        end if
        if (present(usage)) self%usage = trim(usage)
        if (present(description)) self%description = trim(description)
        if (present(epilog)) self%epilog = trim(epilog)
        if (present(version)) self%version = trim(version)

        if (present(formatter)) then
            allocate(self%formatter, source=formatter)
        else
            allocate(StandardFormatter :: self%formatter)
        end if

        if (self%add_help) then
            call self%add_argument("-h", "--help", action=help_action(), &
                help="show this help message and exit")
        end if
        if (present(version)) then
            call self%add_argument("--version", action=version_action(), &
                help="show program's version number and exit")
        end if
    end subroutine parser_init

    subroutine parser_init_with_parents(self, parents, prog, usage, &
        description, epilog, version, formatter, add_help, help_on_empty)
        class(ArgumentParser), intent(inout) :: self
        type(ArgumentParser), intent(in) :: parents(:)
        character(len=*), intent(in), optional :: prog, usage, description
        character(len=*), intent(in), optional :: epilog, version
        class(FormatterType), intent(in), optional :: formatter
        logical, intent(in), optional :: add_help, help_on_empty
        integer :: index

        call self%init(prog, usage, description, epilog, version, formatter, &
            add_help, help_on_empty)
        do index = 1, size(parents)
            call self%copy_parent(parents(index))
        end do
    end subroutine parser_init_with_parents

    subroutine parser_add_argument(self, name1, name2, name3, name4, action, &
        nargs, data_type, default, const, choices, validator, required, help, &
        metavar, dest, visible, deprecated_msg, removed_msg, print_default, &
        print_choices, group, case_insensitive_choices, env, global, delimiter, &
        choice_help)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: name1
        character(len=*), intent(in), optional :: name2, name3, name4
        class(ActionType), intent(in), optional :: action
        class(*), intent(in), optional :: nargs
        character(len=*), intent(in), optional :: data_type
        class(*), intent(in), optional :: default(..)
        class(*), intent(in), optional :: const
        class(*), intent(in), optional :: choices(:)
        class(ValidatorType), intent(in), optional :: validator
        logical, intent(in), optional :: required
        character(len=*), intent(in), optional :: help, metavar, dest
        logical, intent(in), optional :: visible
        character(len=*), intent(in), optional :: deprecated_msg, removed_msg
        logical, intent(in), optional :: print_default, print_choices
        type(GroupHandle), intent(in), optional :: group
        logical, intent(in), optional :: case_insensitive_choices
        !> Environment variable used when the option is not given
        character(len=*), intent(in), optional :: env
        !> Accept the option after a subcommand as well
        logical, intent(in), optional :: global
        !> Split each value token at this character, e.g. `--grid 64,64,128`
        character(len=*), intent(in), optional :: delimiter
        !> One help line per entry of `choices`, listed below the help
        character(len=*), intent(in), optional :: choice_help(:)

        character(len=:), allocatable :: names(:), given(:), message, subject
        type(Argument) :: candidate
        logical :: optional_names
        integer :: code, group_index, index

        if (.not. allocated(self%arguments)) allocate(self%arguments(0))
        call check_names(self%arguments, action, name1, name2, name3, name4, &
            names, given, optional_names, code, message)
        if (code /= FCLAP_OK) then
            call self%reject(code, message, name1, join_names(names))
            return
        end if

        group_index = 0
        if (present(group)) then
            if (.not. group%belongs_to(self%group_owner)) then
                call self%reject(ERR_INVALID_GROUP, &
                    "the group handle does not belong to this parser", name1)
                return
            end if
            group_index = group%group_index()
            if (group_index < 1 .or. group_index > self%group_count()) then
                call self%reject(ERR_INVALID_GROUP, &
                    "the group handle is not valid", name1)
                return
            end if
            if (.not. allocated(self%groups(group_index)%item)) then
                call self%reject(ERR_INVALID_GROUP, &
                    "the group handle is not valid", name1)
                return
            end if
        end if

        ! An unallocated subparser destination is passed as an absent argument.
        call build_argument(self%arguments, names, given, optional_names, &
            self%subparser_dest, self%subparsers_enabled, action, nargs, &
            data_type, default, const, choices, validator, required, help, &
            metavar, dest, visible, deprecated_msg, removed_msg, print_default, &
            print_choices, case_insensitive_choices, candidate, code, message, &
            subject)
        if (code /= FCLAP_OK) then
            call self%reject(code, message, subject, join_names(names))
            return
        end if
        if (present(env)) then
            call check_env_source(candidate, env, code, message)
            if (code /= FCLAP_OK) then
                call self%reject(code, message, candidate%dest, join_names(names))
                return
            end if
            candidate%env = trim(env)
        end if
        if (present(delimiter)) then
            call check_delimiter(candidate, delimiter, code, message)
            if (code /= FCLAP_OK) then
                call self%reject(code, message, candidate%dest, join_names(names))
                return
            end if
            candidate%delimiter = delimiter
        end if
        if (present(choice_help)) then
            if (.not. candidate%has_choices) then
                call self%reject(ERR_INVALID_DEFAULT, &
                    "choice_help needs choices", candidate%dest, &
                    join_names(names))
                return
            end if
            if (size(choice_help) /= choice_count(candidate%choices)) then
                call self%reject(ERR_INVALID_DEFAULT, "choice_help needs " // &
                    "exactly one entry per choice", candidate%dest, &
                    join_names(names))
                return
            end if
            candidate%choice_help = choice_help
        end if
        if (present(global)) candidate%global = global
        if (candidate%global) then
            call check_global(candidate, group_index > 0 .and. &
                self%is_mutex_group(group_index), code, message)
            if (code == FCLAP_OK) then
                do index = 1, self%subparser_count()
                    if (.not. self%subparsers(index)%has_global_conflict( &
                        candidate)) cycle
                    code = ERR_DUPLICATE_OPTION
                    message = "the global option is already defined by " // &
                        "subcommand '" // trim(self%subparser_names(index)) // "'"
                    exit
                end do
            end if
            if (code /= FCLAP_OK) then
                call self%reject(code, message, candidate%dest, join_names(names))
                return
            end if
        end if

        if (group_index > 0) then
            if (.not. self%groups(group_index)%item%accepts_member( &
                candidate%required, candidate%is_positional(), &
                candidate%nargs%min_count(), message)) then
                call self%reject(ERR_INVALID_GROUP, message, candidate%dest, &
                    join_names(names))
                return
            end if
        end if

        call self%append_argument(candidate)
        if (candidate%global) then
            do index = 1, self%subparser_count()
                call self%subparsers(index)%inherit_global(candidate)
            end do
        end if
        if (group_index > 0) then
            call self%groups(group_index)%item%append_member( &
                self%argument_count())
        end if
    end subroutine parser_add_argument

    function parser_add_argument_group(self, title, description) result(handle)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: title
        character(len=*), intent(in), optional :: description
        type(GroupHandle) :: handle
        type(ArgumentGroup) :: definition

        if (len_trim(title) == 0) then
            call self%reject(ERR_INVALID_GROUP, &
                "an argument group title must not be empty", "group")
            return
        end if
        definition%title = trim(title)
        if (present(description)) definition%description = trim(description)
        call self%append_group(definition)
        handle = new_group_handle(self%group_owner, self%group_count())
    end function parser_add_argument_group

    function parser_add_mutex_group(self, required, title, description) &
        result(handle)
        class(ArgumentParser), intent(inout) :: self
        logical, intent(in), optional :: required
        character(len=*), intent(in), optional :: title, description
        type(GroupHandle) :: handle
        type(MutexGroup) :: definition

        if (present(required)) definition%required = required
        if (present(title)) definition%title = trim(title)
        if (present(description)) definition%description = trim(description)
        call self%append_group(definition)
        handle = new_group_handle(self%group_owner, self%group_count())
    end function parser_add_mutex_group

    !> Require other arguments whenever `subject` is given on the command line.
    !>
    !> Arguments are named by destination or by any option spelling and must
    !> already be registered, e.g. `call parser%add_requires("--restart-file",
    !> "--restart")`.
    subroutine parser_add_requires_one(self, subject, needed)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: subject
        character(len=*), intent(in) :: needed

        call self%add_rule(RULE_REQUIRES, subject, [needed])
    end subroutine parser_add_requires_one

    subroutine parser_add_requires_many(self, subject, needed)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: subject
        character(len=*), intent(in) :: needed(:)

        call self%add_rule(RULE_REQUIRES, subject, needed)
    end subroutine parser_add_requires_many

    !> Reject the command line when `subject` and any of `others` are given
    !> together. A conflict is symmetric, so one call covers both directions.
    subroutine parser_add_conflict_one(self, subject, other)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: subject
        character(len=*), intent(in) :: other

        call self%add_rule(RULE_CONFLICTS, subject, [other])
    end subroutine parser_add_conflict_one

    subroutine parser_add_conflict_many(self, subject, others)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: subject
        character(len=*), intent(in) :: others(:)

        call self%add_rule(RULE_CONFLICTS, subject, others)
    end subroutine parser_add_conflict_many

    !> Require `subject` unless at least one of `alternatives` is given.
    subroutine parser_add_required_unless_one(self, subject, alternative)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: subject
        character(len=*), intent(in) :: alternative

        call self%add_rule(RULE_REQUIRED_UNLESS, subject, [alternative])
    end subroutine parser_add_required_unless_one

    subroutine parser_add_required_unless_many(self, subject, alternatives)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: subject
        character(len=*), intent(in) :: alternatives(:)

        call self%add_rule(RULE_REQUIRED_UNLESS, subject, alternatives)
    end subroutine parser_add_required_unless_many

    !> Resolve and check every argument a rule refers to, then store it.
    !>
    !> A rule that could never be satisfied, or whose subject is always
    !> present anyway, is a configuration error rather than a parse error.
    subroutine parser_add_rule(self, kind, subject, others)
        class(ArgumentParser), intent(inout) :: self
        integer, intent(in) :: kind
        character(len=*), intent(in) :: subject
        character(len=*), intent(in) :: others(:)
        type(ArgumentRule) :: rule
        character(len=:), allocatable :: label, spelling
        integer :: index

        label = "the " // rule_kind_name(kind) // " rule"
        rule%kind = kind
        rule%subject = self%find_reference(subject)
        if (rule%subject == 0) then
            call self%reject(ERR_INVALID_RULE, label // &
                " names an unknown argument", trim(subject), trim(subject))
            return
        end if
        spelling = self%arguments(rule%subject)%display_label()
        if (self%arguments(rule%subject)%global) then
            call self%reject(ERR_INVALID_RULE, label // &
                " cannot involve a global option", &
                self%arguments(rule%subject)%dest, spelling)
            return
        end if
        if (size(others) == 0) then
            call self%reject(ERR_INVALID_RULE, label // &
                " needs at least one other argument", &
                self%arguments(rule%subject)%dest, spelling)
            return
        end if

        allocate(rule%others(size(others)))
        do index = 1, size(others)
            rule%others(index) = self%find_reference(others(index))
            if (rule%others(index) == 0) then
                call self%reject(ERR_INVALID_RULE, label // &
                    " names an unknown argument '" // trim(others(index)) // &
                    "'", self%arguments(rule%subject)%dest, spelling)
                return
            end if
            if (self%arguments(rule%others(index))%global) then
                call self%reject(ERR_INVALID_RULE, label // &
                    " cannot involve a global option", &
                    self%arguments(rule%subject)%dest, spelling)
                return
            end if
            if (rule%others(index) == rule%subject) then
                call self%reject(ERR_INVALID_RULE, label // &
                    " must not refer to its own subject", &
                    self%arguments(rule%subject)%dest, spelling)
                return
            end if
        end do

        select case (kind)
        case (RULE_REQUIRED_UNLESS)
            if (any_always_present(self%arguments, [rule%subject])) then
                call self%reject(ERR_INVALID_RULE, label // &
                    " needs a subject that is not already required", &
                    self%arguments(rule%subject)%dest, spelling)
                return
            end if
        case (RULE_CONFLICTS)
            if (any_always_present(self%arguments, &
                [rule%subject, rule%others])) then
                call self%reject(ERR_INVALID_RULE, &
                    "conflicting arguments must not be required", &
                    self%arguments(rule%subject)%dest, spelling)
                return
            end if
        end select
        call self%append_rule(rule)
    end subroutine parser_add_rule

    !> Index of the argument with this destination or option spelling.
    pure integer function parser_find_reference(self, name) result(index)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: name
        integer :: current

        index = 0
        if (len_trim(name) == 0) return
        do current = 1, self%argument_count()
            if (name(1:1) == '-') then
                if (.not. self%arguments(current)%is_optional) cycle
                if (.not. any(self%arguments(current)%names == trim(name))) &
                    cycle
            else
                if (self%arguments(current)%dest /= trim(name)) cycle
            end if
            index = current
            return
        end do
    end function parser_find_reference

    subroutine parser_add_subparsers(self, title, description, dest, required)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in), optional :: title, description, dest
        logical, intent(in), optional :: required
        character(len=:), allocatable :: selected_dest
        integer :: index

        if (self%subparsers_enabled) then
            call self%reject(ERR_INCOMPATIBLE_ACTION, &
                "a parser can own only one subparser collection", "subparsers")
            return
        end if

        selected_dest = "command"
        if (present(dest)) selected_dest = trim(dest)
        if (len(selected_dest) == 0) then
            call self%reject(ERR_INVALID_ARGUMENT_NAME, &
                "the subparser destination must not be empty", "subparsers")
            return
        end if
        if (self%has_dest(selected_dest)) then
            call self%reject(ERR_DUPLICATE_DEST, &
                "the subparser destination is already registered", &
                selected_dest)
            return
        end if
        do index = 1, self%argument_count()
            if (.not. self%arguments(index)%is_positional()) cycle
            if (self%arguments(index)%nargs_value() == NARGS_REMAINDER) then
                call self%reject(ERR_INVALID_NARGS, &
                    "remainder nargs cannot precede a subparser collection", &
                    selected_dest)
                return
            end if
        end do

        self%subparsers_enabled = .true.
        self%subparsers_required = .false.
        if (present(required)) self%subparsers_required = required
        self%subparser_dest = selected_dest
        self%subparser_title = "commands"
        if (present(title)) self%subparser_title = trim(title)
        if (present(description)) &
            self%subparser_description = trim(description)
    end subroutine parser_add_subparsers

    subroutine parser_add_parser(self, name, subparser, help_text, aliases)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ArgumentParser), intent(in), optional :: subparser
        character(len=*), intent(in), optional :: help_text
        !> Alternative names accepted on the command line, e.g. `["r"]`
        character(len=*), intent(in), optional :: aliases(:)
        type(ArgumentParser) :: definition
        character(len=:), allocatable :: summary
        integer :: index

        if (.not. self%subparsers_enabled) then
            call self%reject(ERR_INCOMPATIBLE_ACTION, &
                "add_subparsers must be called before add_parser", trim(name))
            return
        end if
        if (.not. valid_subcommand_name(name)) then
            call self%reject(ERR_INVALID_ARGUMENT_NAME, &
                "a subcommand name must be a nonempty token without a leading '-'", &
                trim(name))
            return
        end if
        if (self%find_subparser(name) > 0) then
            call self%reject(ERR_DUPLICATE_OPTION, &
                "the subcommand name is already registered", trim(name))
            return
        end if
        ! Check every alias first so a rejected command registers nothing.
        if (present(aliases)) then
            do index = 1, size(aliases)
                if (.not. valid_subcommand_name(aliases(index))) then
                    call self%reject(ERR_INVALID_ARGUMENT_NAME, &
                        "a subcommand alias must be a nonempty token " // &
                        "without a leading '-'", trim(name))
                    return
                end if
                if (self%find_subparser(aliases(index)) > 0 .or. &
                    trim(aliases(index)) == trim(name) .or. &
                    any(aliases(:index - 1) == aliases(index))) then
                    call self%reject(ERR_DUPLICATE_OPTION, &
                        "the subcommand alias '" // trim(aliases(index)) // &
                        "' is already registered", trim(name))
                    return
                end if
            end do
        end if

        if (present(subparser)) then
            call definition%clone_from(subparser)
        else
            call definition%init(add_help=.true.)
        end if
        do index = 1, self%argument_count()
            if (.not. self%arguments(index)%global) cycle
            if (definition%has_global_conflict(self%arguments(index))) then
                call self%reject(ERR_DUPLICATE_OPTION, "the subcommand " // &
                    "already defines the global option " // &
                    self%arguments(index)%display_label(), trim(name))
                return
            end if
        end do
        do index = 1, self%argument_count()
            if (self%arguments(index)%global) &
                call definition%inherit_global(self%arguments(index))
        end do
        if (allocated(self%prog)) then
            call definition%rebase_program(trim(self%prog) // " " // trim(name))
        else
            call definition%rebase_program("program " // trim(name))
        end if
        call definition%refresh_owner_ids()
        if (definition%config_errors%has_errors()) then
            call self%config_errors%merge(definition%config_errors)
        end if

        summary = ""
        if (present(help_text)) summary = trim(help_text)
        call self%append_subparser(trim(name), summary, definition)
        if (present(aliases)) then
            do index = 1, size(aliases)
                call self%append_alias(trim(aliases(index)), &
                    self%subparser_count())
            end do
        end if
    end subroutine parser_add_parser

    pure integer function parser_argument_count(self) result(number)
        class(ArgumentParser), intent(in) :: self

        if (allocated(self%arguments)) then
            number = size(self%arguments)
        else
            number = 0
        end if
    end function parser_argument_count

    pure integer function parser_group_count(self) result(number)
        class(ArgumentParser), intent(in) :: self

        if (allocated(self%groups)) then
            number = size(self%groups)
        else
            number = 0
        end if
    end function parser_group_count

    pure integer function parser_rule_count(self) result(number)
        class(ArgumentParser), intent(in) :: self

        if (allocated(self%rules)) then
            number = size(self%rules)
        else
            number = 0
        end if
    end function parser_rule_count

    pure integer function parser_subparser_count(self) result(number)
        class(ArgumentParser), intent(in) :: self

        if (allocated(self%subparsers)) then
            number = size(self%subparsers)
        else
            number = 0
        end if
    end function parser_subparser_count

    function parser_get_argument(self, index) result(definition)
        class(ArgumentParser), intent(in) :: self
        integer, intent(in) :: index
        type(Argument) :: definition

        if (index >= 1 .and. index <= self%argument_count()) then
            definition = self%arguments(index)
        end if
    end function parser_get_argument

    pure logical function parser_has_option(self, name) result(found)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: name
        integer :: index

        found = .false.
        do index = 1, self%argument_count()
            if (self%arguments(index)%matches_name(name)) then
                found = .true.
                return
            end if
        end do
    end function parser_has_option

    pure logical function parser_has_dest(self, dest) result(found)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: dest
        integer :: index

        found = .false.
        do index = 1, self%argument_count()
            if (self%arguments(index)%dest == trim(dest)) then
                found = .true.
                return
            end if
        end do
        if (self%subparsers_enabled .and. allocated(self%subparser_dest)) then
            found = self%subparser_dest == trim(dest)
        end if
    end function parser_has_dest

    pure logical function parser_is_valid(self) result(valid)
        class(ArgumentParser), intent(in) :: self

        valid = .not. self%config_errors%has_fatal_errors()
    end function parser_is_valid

    function parser_get_config_errors(self) result(errors)
        class(ArgumentParser), intent(in) :: self
        type(ErrorStack) :: errors

        errors = self%config_errors
    end function parser_get_config_errors

    function parser_get_help_model(self) result(model)
        class(ArgumentParser), intent(in) :: self
        type(HelpModel) :: model
        integer :: index

        if (allocated(self%prog)) then
            model%prog = self%prog
        else
            model%prog = ""
        end if
        if (allocated(self%usage)) model%usage = self%usage
        if (allocated(self%description)) model%description = self%description
        if (allocated(self%epilog)) model%epilog = self%epilog

        allocate(model%arguments(self%argument_count()))
        do index = 1, self%argument_count()
            if (allocated(self%arguments(index)%names)) then
                model%arguments(index)%names = self%arguments(index)%names
            end if
            if (allocated(self%arguments(index)%dest)) then
                model%arguments(index)%dest = self%arguments(index)%dest
            end if
            if (self%arguments(index)%has_choices .and. &
                .not. allocated(self%arguments(index)%metavar)) then
                model%arguments(index)%metavar = &
                    choices_metavar(self%arguments(index)%choices)
            else
                model%arguments(index)%metavar = &
                    self%arguments(index)%effective_metavar()
            end if
            if (allocated(self%arguments(index)%help)) then
                model%arguments(index)%help = self%arguments(index)%help
            end if
            if (self%arguments(index)%has_default) then
                model%arguments(index)%default_text = &
                    self%arguments(index)%default_value%to_string()
            end if
            if (self%arguments(index)%has_choices) then
                model%arguments(index)%choices_text = &
                    self%arguments(index)%choices%to_string()
            end if
            if (allocated(self%arguments(index)%deprecated_msg)) then
                model%arguments(index)%deprecated_msg = &
                    self%arguments(index)%deprecated_msg
            end if
            if (allocated(self%arguments(index)%removed_msg)) then
                model%arguments(index)%removed_msg = &
                    self%arguments(index)%removed_msg
            end if
            if (allocated(self%arguments(index)%env)) then
                model%arguments(index)%env = self%arguments(index)%env
            end if
            if (allocated(self%arguments(index)%delimiter)) then
                model%arguments(index)%delimiter = &
                    self%arguments(index)%delimiter
            end if
            if (allocated(self%arguments(index)%choice_help)) then
                call choice_spellings(self%arguments(index)%choices, &
                    model%arguments(index)%choice_names)
                model%arguments(index)%choice_help = &
                    self%arguments(index)%choice_help
            end if
            model%arguments(index)%nargs = &
                self%arguments(index)%nargs_value()
            model%arguments(index)%is_optional = &
                self%arguments(index)%is_optional
            model%arguments(index)%required = self%arguments(index)%required
            model%arguments(index)%visible = self%arguments(index)%visible
            model%arguments(index)%has_default = &
                self%arguments(index)%has_default
            model%arguments(index)%implicit_default = &
                self%arguments(index)%default_is_implicit
            model%arguments(index)%has_choices = &
                self%arguments(index)%has_choices
            model%arguments(index)%print_default = &
                self%arguments(index)%print_default
            model%arguments(index)%print_choices = &
                self%arguments(index)%print_choices
        end do

        allocate(model%groups(self%group_count()))
        do index = 1, self%group_count()
            if (.not. allocated(self%groups(index)%item)) cycle
            model%groups(index) = &
                self%groups(index)%item%help_snapshot()
        end do

        if (self%subparsers_enabled) then
            if (allocated(self%subparser_title)) &
                model%subcommand_title = self%subparser_title
            if (allocated(self%subparser_description)) &
                model%subcommand_description = self%subparser_description
            model%subcommand_required = self%subparsers_required
            allocate(model%commands(self%subparser_count()))
            do index = 1, self%subparser_count()
                model%commands(index)%name = trim(self%subparser_names(index))
                if (allocated(self%alias_names)) then
                    model%commands(index)%aliases = &
                        pack(self%alias_names, self%alias_targets == index)
                end if
                if (len_trim(self%subparser_helps(index)) > 0) then
                    model%commands(index)%help = &
                        trim(self%subparser_helps(index))
                end if
            end do
        end if
    end function parser_get_help_model

    function parser_format_usage(self) result(text)
        class(ArgumentParser), intent(in) :: self
        character(len=:), allocatable :: text
        type(HelpModel) :: model
        type(StandardFormatter) :: fallback

        model = self%get_help_model()
        if (allocated(self%formatter)) then
            text = self%formatter%format_usage(model)
        else
            text = fallback%format_usage(model)
        end if
    end function parser_format_usage

    function parser_format_help(self) result(text)
        class(ArgumentParser), intent(in) :: self
        character(len=:), allocatable :: text
        type(HelpModel) :: model
        type(StandardFormatter) :: fallback

        model = self%get_help_model()
        if (allocated(self%formatter)) then
            text = self%formatter%format_help(model)
        else
            text = fallback%format_help(model)
        end if
    end function parser_format_help

    subroutine parser_print_usage(self, unit)
        class(ArgumentParser), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: selected_unit

        selected_unit = output_unit
        if (present(unit)) selected_unit = unit
        write(selected_unit, '(a)') self%format_usage()
    end subroutine parser_print_usage

    subroutine parser_print_help(self, unit)
        class(ArgumentParser), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: selected_unit

        selected_unit = output_unit
        if (present(unit)) selected_unit = unit
        write(selected_unit, '(a)') self%format_help()
    end subroutine parser_print_help

    !> Parse an explicit token array without terminating the program.
    !>
    !> `environment` holds `NAME=value` entries that replace the process
    !> environment for options declared with `env=`, e.g. in tests.
    function parser_parse_tokens(self, tokens, environment) result(parsed)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: tokens(:)
        character(len=*), intent(in), optional :: environment(:)
        type(ParseResult) :: parsed

        parsed = self%parse_level(tokens, .false., environment)
    end function parser_parse_tokens

    !> Like `parse_tokens`, but unknown options and surplus positionals are
    !> returned in `extra_tokens` instead of failing, e.g. to leave options
    !> for MPI or PETSc. A separate value of an unknown option (`-k v`) is
    !> not recognized as such; write it attached (`-k=v`).
    function parser_parse_known_tokens(self, tokens, environment) &
        result(parsed)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: tokens(:)
        character(len=*), intent(in), optional :: environment(:)
        type(ParseResult) :: parsed

        parsed = self%parse_level(tokens, .true., environment)
    end function parser_parse_known_tokens

    !> Parse this parser's part of the tokens and dispatch to the selected
    !> subcommand, which parses the rest.
    recursive function parser_parse_level(self, tokens, collect_extras, &
        environment) result(parsed)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: tokens(:)
        logical, intent(in) :: collect_extras
        character(len=*), intent(in), optional :: environment(:)
        type(ParseResult) :: parsed
        type(ParseResult) :: child_result
        type(Argument), allocatable :: definitions(:)
        type(GroupBox), allocatable :: group_definitions(:)
        character(len=:), allocatable :: command_names(:), command
        integer :: child_index, next_token_index

        if (self%help_on_empty .and. size(tokens) == 0 .and. &
            .not. self%config_errors%has_fatal_errors()) then
            parsed%outcome = PARSE_FAILURE
            call parsed%errors%add("no arguments were given", &
                code=ERR_NO_ARGUMENTS)
            parsed%text = self%format_help()
            return
        end if

        if (allocated(self%arguments)) then
            definitions = self%arguments
        else
            allocate(definitions(0))
        end if
        if (allocated(self%groups)) then
            group_definitions = self%groups
        else
            allocate(group_definitions(0))
        end if
        call self%selectable_names(command_names)

        if (allocated(self%version)) then
            parsed = parse_definitions(definitions, self%config_errors, tokens, &
                group_definitions, version_text=self%version, &
                subcommand_names=command_names, &
                has_subcommands=self%subparsers_enabled, &
                subcommand_required=self%subparsers_required, &
                next_token_index=next_token_index, rules=self%rules, &
                environment=environment, collect_extras=collect_extras)
        else
            parsed = parse_definitions(definitions, self%config_errors, tokens, &
                group_definitions, subcommand_names=command_names, &
                has_subcommands=self%subparsers_enabled, &
                subcommand_required=self%subparsers_required, &
                next_token_index=next_token_index, rules=self%rules, &
                environment=environment, collect_extras=collect_extras)
        end if

        if (parsed%outcome == PARSE_HELP) parsed%text = self%format_help()
        if (parsed%outcome /= PARSE_SUCCESS) return
        if (.not. allocated(parsed%selected_command_path)) return

        command = trim(parsed%selected_command_path(1))
        child_index = self%find_subparser(command)
        if (child_index == 0) return
        ! An alias selects the command under its registered name.
        command = trim(self%subparser_names(child_index))
        deallocate(parsed%selected_command_path)
        allocate(character(len=len(command)) :: parsed%selected_command_path(1))
        parsed%selected_command_path(1) = command

        call parsed%namespace%set_string(self%subparser_dest, command)
        call parsed%set_source(self%subparser_dest, SOURCE_COMMAND_LINE)
        child_result = self%subparsers(child_index)%parse_level( &
            tokens(next_token_index:), collect_extras, environment)
        call parsed%merge_child(child_result)
        if (allocated(child_result%extra_tokens)) &
            call append_tokens(parsed%extra_tokens, child_result%extra_tokens)
        ! The child saw a slice of the line; report absolute token positions.
        call child_result%errors%offset_token_indices(next_token_index - 1)
        call parsed%errors%merge(child_result%errors)
        parsed%outcome = child_result%outcome
        call extend_command_path(parsed%selected_command_path, &
            child_result%selected_command_path)
        if (allocated(child_result%text)) then
            parsed%text = child_result%text
        else if (child_result%outcome == PARSE_FAILURE) then
            parsed%text = self%subparsers(child_index)%format_usage()
        end if
    end function parser_parse_level

    function parser_try_parse_args(self) result(parsed)
        class(ArgumentParser), intent(in) :: self
        type(ParseResult) :: parsed
        character(len=:), allocatable :: tokens(:)

        call process_tokens(tokens)
        parsed = self%parse_tokens(tokens)
    end function parser_try_parse_args

    function parser_try_parse_known_args(self) result(parsed)
        class(ArgumentParser), intent(in) :: self
        type(ParseResult) :: parsed
        character(len=:), allocatable :: tokens(:)

        call process_tokens(tokens)
        parsed = self%parse_known_tokens(tokens)
    end function parser_try_parse_known_args

    function parser_parse_args(self) result(args)
        class(ArgumentParser), intent(in) :: self
        type(Namespace) :: args

        if (.not. self%is_valid()) call self%abort_invalid_definition()
        args = self%finish_process_parse(self%try_parse_args())
    end function parser_parse_args

    !> Like `parse_args`, but unknown options and surplus positionals are
    !> returned in `extras` instead of being reported as errors.
    function parser_parse_known_args(self, extras) result(args)
        class(ArgumentParser), intent(in) :: self
        character(len=:), allocatable, intent(out) :: extras(:)
        type(Namespace) :: args
        type(ParseResult) :: parsed

        if (.not. self%is_valid()) call self%abort_invalid_definition()
        parsed = self%try_parse_known_args()
        if (allocated(parsed%extra_tokens)) then
            extras = parsed%extra_tokens
        else
            allocate(character(len=1) :: extras(0))
        end if
        args = self%finish_process_parse(parsed)
    end function parser_parse_known_args

    !> Apply the process-level policy to a parse of the command line: return
    !> the namespace on success, otherwise print and stop.
    function parser_finish_process_parse(self, parsed) result(args)
        class(ArgumentParser), intent(in) :: self
        type(ParseResult), intent(in) :: parsed
        type(Namespace) :: args

        ! Normal termination with an explicit status keeps compiler runtimes
        ! from reporting a user's command-line mistake as a program crash.
        select case (parsed%outcome)
        case (PARSE_SUCCESS)
            args = parsed%namespace
            call self%print_diagnostics(parsed, error_unit)
        case (PARSE_HELP, PARSE_VERSION)
            if (allocated(parsed%text)) write(output_unit, '(a)') parsed%text
            flush(output_unit)
            stop 0, quiet=.true.
        case (PARSE_FAILURE)
            if (allocated(parsed%text)) then
                write(error_unit, '(a)') parsed%text
            else
                write(error_unit, '(a)') self%format_usage()
            end if
            ! Help shown for an empty command line needs no error message.
            if (.not. is_empty_line_help(parsed)) &
                call self%print_diagnostics(parsed, error_unit)
            flush(error_unit)
            stop 2, quiet=.true.
        case default
            write(error_unit, '(a)') "fclap: invalid parse outcome"
            flush(error_unit)
            stop 2, quiet=.true.
        end select
    end function parser_finish_process_parse

    !> Report a user-level error after parsing and exit with status 2.
    !>
    !> Use this for checks argparse-style programs make themselves, such as
    !> "input file does not exist", so they look like every other usage error.
    subroutine parser_error(self, message)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: message

        write(error_unit, '(a)') self%format_usage()
        write(error_unit, '(a)') self%program_name() // ": error: " // &
            trim(message)
        flush(error_unit)
        stop 2, quiet=.true.
    end subroutine parser_error

    !> Stop for a broken parser definition. This is a programming error, so
    !> it is reported distinctly from user errors with status 70 (EX_SOFTWARE).
    subroutine parser_abort_invalid_definition(self)
        class(ArgumentParser), intent(in) :: self
        integer :: index

        do index = 1, self%config_errors%count()
            write(error_unit, '(a)') &
                self%format_diagnostic(self%config_errors%get(index))
        end do
        flush(error_unit)
        stop 70, quiet=.true.
    end subroutine parser_abort_invalid_definition

    function parser_program_name(self) result(prog)
        class(ArgumentParser), intent(in) :: self
        character(len=:), allocatable :: prog

        if (allocated(self%prog)) then
            prog = self%prog
        else
            prog = "program"
        end if
    end function parser_program_name

    !> Format one diagnostic in argparse style for command-line users.
    !>
    !> Structured errors name destinations; this maps them back to the option
    !> spellings a user typed, e.g. `prog: error: argument -j/--threads: ...`.
    function parser_format_diagnostic(self, entry) result(text)
        class(ArgumentParser), intent(in) :: self
        type(ErrorEntry), intent(in) :: entry
        character(len=:), allocatable :: text
        character(len=:), allocatable :: label, message, severity, prog

        if (entry%severity == ERROR_WARNING) then
            severity = "warning"
        else
            severity = "error"
        end if
        prog = self%program_name()
        message = ""
        if (allocated(entry%message)) message = trim(entry%message)

        ! Configuration codes describe the parser definition, not the input.
        if (entry%code >= 1000 .and. entry%code < 2000) then
            label = ""
            if (allocated(entry%token)) then
                label = trim(entry%token)
            else if (allocated(entry%arg_name)) then
                label = trim(entry%arg_name)
            end if
            if (len(label) > 0) message = "argument " // label // ": " // message
            text = prog // ": invalid parser definition: " // message
            return
        end if

        select case (entry%code)
        case (ERR_UNKNOWN_ARGUMENT, ERR_EXTRA_POSITIONAL, ERR_UNKNOWN_SUBCOMMAND)
            if (allocated(entry%token)) &
                message = message // " '" // trim(entry%token) // "'"
        case (ERR_MUTEX_CONFLICT, ERR_MUTEX_REQUIRED, ERR_MISSING_SUBCOMMAND)
            continue
        case default
            label = ""
            if (allocated(entry%arg_name)) &
                label = self%display_label(entry%arg_name)
            if (len(label) > 0) message = "argument " // label // ": " // message
            select case (entry%code)
            case (ERR_INVALID_VALUE, ERR_INVALID_CHOICE, ERR_VALIDATION_FAILED)
                if (allocated(entry%token)) &
                    message = message // " (got '" // trim(entry%token) // "')"
            end select
        end select
        if (allocated(entry%suggestion)) &
            message = message // " (did you mean '" // entry%suggestion // "'?)"
        text = prog // ": " // severity // ": " // message
    end function parser_format_diagnostic

    !> Write every diagnostic of a parse result in command-line form.
    subroutine parser_print_diagnostics(self, parsed, unit)
        class(ArgumentParser), intent(in) :: self
        type(ParseResult), intent(in) :: parsed
        integer, intent(in) :: unit
        integer :: index

        do index = 1, parsed%errors%count()
            write(unit, '(a)') self%format_diagnostic(parsed%errors%get(index))
        end do
    end subroutine parser_print_diagnostics

    !> Map a destination to the spelling shown to users, searching children.
    recursive function parser_display_label(self, dest) result(label)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: dest
        character(len=:), allocatable :: label
        integer :: index

        label = ""
        do index = 1, self%argument_count()
            if (self%arguments(index)%dest /= trim(dest)) cycle
            label = self%arguments(index)%display_label()
            return
        end do
        do index = 1, self%subparser_count()
            label = self%subparsers(index)%display_label(dest)
            if (len(label) > 0) return
        end do
    end function parser_display_label

    subroutine parser_append_argument(self, definition)
        class(ArgumentParser), intent(inout) :: self
        type(Argument), intent(in) :: definition
        type(Argument), allocatable :: temporary(:)
        integer :: old_size

        old_size = self%argument_count()
        allocate(temporary(old_size + 1))
        if (old_size > 0) temporary(:old_size) = self%arguments
        temporary(old_size + 1) = definition
        call move_alloc(temporary, self%arguments)
    end subroutine parser_append_argument

    subroutine parser_append_group(self, definition)
        class(ArgumentParser), intent(inout) :: self
        class(GroupType), intent(in) :: definition
        type(GroupBox), allocatable :: temporary(:)
        integer :: old_size

        old_size = self%group_count()
        allocate(temporary(old_size + 1))
        if (old_size > 0) temporary(:old_size) = self%groups
        allocate(temporary(old_size + 1)%item, source=definition)
        call move_alloc(temporary, self%groups)
    end subroutine parser_append_group

    subroutine parser_append_rule(self, rule)
        class(ArgumentParser), intent(inout) :: self
        type(ArgumentRule), intent(in) :: rule
        type(ArgumentRule), allocatable :: temporary(:)
        integer :: old_size

        old_size = self%rule_count()
        allocate(temporary(old_size + 1))
        if (old_size > 0) temporary(:old_size) = self%rules
        temporary(old_size + 1) = rule
        call move_alloc(temporary, self%rules)
    end subroutine parser_append_rule

    subroutine parser_append_subparser(self, name, help_text, definition)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: name, help_text
        type(ArgumentParser), intent(in) :: definition
        type(ArgumentParser), allocatable :: parser_values(:)
        character(len=:), allocatable :: name_values(:), help_values(:)
        integer :: old_size, name_length, help_length, index

        old_size = self%subparser_count()
        allocate(parser_values(old_size + 1))
        do index = 1, old_size
            call parser_values(index)%clone_from(self%subparsers(index))
        end do
        call parser_values(old_size + 1)%clone_from(definition)

        name_length = max(1, len_trim(name))
        help_length = max(1, len_trim(help_text))
        if (allocated(self%subparser_names)) &
            name_length = max(name_length, len(self%subparser_names))
        if (allocated(self%subparser_helps)) &
            help_length = max(help_length, len(self%subparser_helps))
        allocate(character(len=name_length) :: name_values(old_size + 1))
        allocate(character(len=help_length) :: help_values(old_size + 1))
        if (old_size > 0) then
            name_values(:old_size) = self%subparser_names
            help_values(:old_size) = self%subparser_helps
        end if
        name_values(old_size + 1) = trim(name)
        help_values(old_size + 1) = trim(help_text)

        call move_alloc(parser_values, self%subparsers)
        call move_alloc(name_values, self%subparser_names)
        call move_alloc(help_values, self%subparser_helps)
    end subroutine parser_append_subparser

    pure integer function parser_find_subparser(self, name) result(index)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: name
        integer :: current

        index = 0
        if (.not. allocated(self%subparser_names)) return
        do current = 1, size(self%subparser_names)
            if (trim(self%subparser_names(current)) == trim(name)) then
                index = current
                return
            end if
        end do
        if (.not. allocated(self%alias_names)) return
        do current = 1, size(self%alias_names)
            if (trim(self%alias_names(current)) == trim(name)) then
                index = self%alias_targets(current)
                return
            end if
        end do
    end function parser_find_subparser

    !> Whether this parser or any subcommand below it already defines a name
    !> or destination of the global option `definition`.
    recursive logical function parser_has_global_conflict(self, definition) &
        result(conflict)
        class(ArgumentParser), intent(in) :: self
        type(Argument), intent(in) :: definition
        integer :: index

        conflict = any_registered_name(self%arguments, definition%names)
        if (.not. conflict) conflict = self%has_dest(definition%dest)
        if (conflict) return
        do index = 1, self%subparser_count()
            conflict = self%subparsers(index)%has_global_conflict(definition)
            if (conflict) return
        end do
    end function parser_has_global_conflict

    !> Add a copy of a global option to this parser and every subcommand.
    recursive subroutine parser_inherit_global(self, definition)
        class(ArgumentParser), intent(inout) :: self
        type(Argument), intent(in) :: definition
        integer :: index

        call self%append_argument(definition)
        do index = 1, self%subparser_count()
            call self%subparsers(index)%inherit_global(definition)
        end do
    end subroutine parser_inherit_global

    logical function parser_is_mutex_group(self, group_index) result(mutex)
        class(ArgumentParser), intent(in) :: self
        integer, intent(in) :: group_index

        mutex = .false.
        if (group_index < 1 .or. group_index > self%group_count()) return
        if (.not. allocated(self%groups(group_index)%item)) return
        select type (group => self%groups(group_index)%item)
        type is (MutexGroup)
            mutex = .true.
        end select
    end function parser_is_mutex_group

    !> Every spelling that selects a subcommand: names, then aliases.
    subroutine parser_selectable_names(self, names)
        class(ArgumentParser), intent(in) :: self
        character(len=:), allocatable, intent(out) :: names(:)
        integer :: width, count

        if (.not. allocated(self%subparser_names)) then
            allocate(character(len=1) :: names(0))
            return
        end if
        if (.not. allocated(self%alias_names)) then
            names = self%subparser_names
            return
        end if
        width = max(len(self%subparser_names), len(self%alias_names))
        count = size(self%subparser_names)
        allocate(character(len=width) :: &
            names(count + size(self%alias_names)))
        names(:count) = self%subparser_names
        names(count + 1:) = self%alias_names
    end subroutine parser_selectable_names

    subroutine parser_append_alias(self, name, target)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer, intent(in) :: target
        character(len=:), allocatable :: names(:)
        integer, allocatable :: targets(:)
        integer :: old_size, width

        old_size = 0
        width = len(name)
        if (allocated(self%alias_names)) then
            old_size = size(self%alias_names)
            width = max(width, len(self%alias_names))
        end if
        allocate(character(len=width) :: names(old_size + 1))
        allocate(targets(old_size + 1))
        if (old_size > 0) then
            names(:old_size) = self%alias_names
            targets(:old_size) = self%alias_targets
        end if
        names(old_size + 1) = name
        targets(old_size + 1) = target
        call move_alloc(names, self%alias_names)
        call move_alloc(targets, self%alias_targets)
    end subroutine parser_append_alias

    subroutine parser_copy_parent(self, parent)
        class(ArgumentParser), intent(inout) :: self
        type(ArgumentParser), intent(in) :: parent
        type(GroupBox) :: copied_group
        type(ArgumentRule) :: copied_rule
        integer, allocatable :: index_map(:)
        integer :: argument_index, group_index, member_index, old_index
        integer :: rule_index
        logical :: kept

        call self%config_errors%merge(parent%config_errors)
        if (.not. allocated(self%arguments)) allocate(self%arguments(0))
        allocate(index_map(parent%argument_count()), source=0)
        do argument_index = 1, parent%argument_count()
            if (is_special_argument(parent%arguments(argument_index))) cycle
            if (any_registered_name(self%arguments, &
                parent%arguments(argument_index)%names)) then
                call self%reject(ERR_DUPLICATE_OPTION, &
                    "a parent option or positional name is already registered", &
                    parent%arguments(argument_index)%primary_name())
                cycle
            end if
            if (self%has_dest(parent%arguments(argument_index)%dest)) then
                call self%reject(ERR_DUPLICATE_DEST, &
                    "a parent destination is already registered", &
                    parent%arguments(argument_index)%dest)
                cycle
            end if
            call self%append_argument(parent%arguments(argument_index))
            index_map(argument_index) = self%argument_count()
        end do

        do group_index = 1, parent%group_count()
            if (.not. allocated(parent%groups(group_index)%item)) cycle
            allocate(copied_group%item, &
                source=parent%groups(group_index)%item)
            if (allocated(copied_group%item%members)) &
                deallocate(copied_group%item%members)
            if (allocated(parent%groups(group_index)%item%members)) then
                do member_index = 1, &
                    size(parent%groups(group_index)%item%members)
                    old_index = parent%groups(group_index)%item%members( &
                        member_index)
                    if (old_index < 1 .or. old_index > size(index_map)) cycle
                    if (index_map(old_index) == 0) cycle
                    call copied_group%item%append_member(index_map(old_index))
                end do
            end if
            call self%append_group(copied_group%item)
            deallocate(copied_group%item)
        end do

        do rule_index = 1, parent%rule_count()
            copied_rule = parent%rules(rule_index)
            call copied_rule%remap(index_map, kept)
            if (kept) call self%append_rule(copied_rule)
        end do
    end subroutine parser_copy_parent

    recursive subroutine parser_clone_from(self, source)
        class(ArgumentParser), intent(inout) :: self
        type(ArgumentParser), intent(in) :: source
        integer :: index

        call self%clear()
        if (allocated(source%prog)) self%prog = source%prog
        if (allocated(source%usage)) self%usage = source%usage
        if (allocated(source%description)) self%description = source%description
        if (allocated(source%epilog)) self%epilog = source%epilog
        if (allocated(source%version)) self%version = source%version
        if (allocated(source%formatter)) &
            allocate(self%formatter, source=source%formatter)
        self%add_help = source%add_help
        self%help_on_empty = source%help_on_empty
        self%group_owner = new_group_owner_id()
        if (allocated(source%arguments)) self%arguments = source%arguments
        if (allocated(source%groups)) self%groups = source%groups
        if (allocated(source%rules)) self%rules = source%rules
        self%config_errors = source%config_errors

        self%subparsers_enabled = source%subparsers_enabled
        self%subparsers_required = source%subparsers_required
        if (allocated(source%subparser_dest)) &
            self%subparser_dest = source%subparser_dest
        if (allocated(source%subparser_title)) &
            self%subparser_title = source%subparser_title
        if (allocated(source%subparser_description)) &
            self%subparser_description = source%subparser_description
        if (allocated(source%subparser_names)) &
            self%subparser_names = source%subparser_names
        if (allocated(source%subparser_helps)) &
            self%subparser_helps = source%subparser_helps
        if (allocated(source%alias_names)) &
            self%alias_names = source%alias_names
        if (allocated(source%alias_targets)) &
            self%alias_targets = source%alias_targets
        if (allocated(source%subparsers)) then
            allocate(self%subparsers(size(source%subparsers)))
            do index = 1, size(source%subparsers)
                call self%subparsers(index)%clone_from( &
                    source%subparsers(index))
            end do
        end if
    end subroutine parser_clone_from

    recursive subroutine parser_refresh_owner_ids(self)
        class(ArgumentParser), intent(inout) :: self
        integer :: index

        self%group_owner = new_group_owner_id()
        do index = 1, self%subparser_count()
            call self%subparsers(index)%refresh_owner_ids()
        end do
    end subroutine parser_refresh_owner_ids

    recursive subroutine parser_rebase_program(self, program_name)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: program_name
        integer :: index

        self%prog = trim(program_name)
        do index = 1, self%subparser_count()
            call self%subparsers(index)%rebase_program( &
                trim(program_name) // " " // trim(self%subparser_names(index)))
        end do
    end subroutine parser_rebase_program

    !> Record a configuration error. `spelling` names the definition as the
    !> programmer wrote it (e.g. `-j/--threads`) and is stored as the token.
    subroutine parser_reject(self, code, message, arg_name, spelling)
        class(ArgumentParser), intent(inout) :: self
        integer, intent(in) :: code
        character(len=*), intent(in) :: message, arg_name
        character(len=*), intent(in), optional :: spelling

        call self%config_errors%add(message, code=code, arg_name=arg_name, &
            token=spelling)
    end subroutine parser_reject

    subroutine parser_clear(self)
        class(ArgumentParser), intent(inout) :: self

        if (allocated(self%prog)) deallocate(self%prog)
        if (allocated(self%usage)) deallocate(self%usage)
        if (allocated(self%description)) deallocate(self%description)
        if (allocated(self%epilog)) deallocate(self%epilog)
        if (allocated(self%version)) deallocate(self%version)
        if (allocated(self%formatter)) deallocate(self%formatter)
        ! Keep an allocated (possibly empty) array so it can always be passed
        ! to procedures expecting the existing definitions.
        if (allocated(self%arguments)) deallocate(self%arguments)
        allocate(self%arguments(0))
        if (allocated(self%groups)) deallocate(self%groups)
        if (allocated(self%rules)) deallocate(self%rules)
        if (allocated(self%subparser_dest)) deallocate(self%subparser_dest)
        if (allocated(self%subparser_title)) deallocate(self%subparser_title)
        if (allocated(self%subparser_description)) &
            deallocate(self%subparser_description)
        if (allocated(self%subparser_names)) deallocate(self%subparser_names)
        if (allocated(self%alias_names)) deallocate(self%alias_names)
        if (allocated(self%alias_targets)) deallocate(self%alias_targets)
        if (allocated(self%subparser_helps)) deallocate(self%subparser_helps)
        if (allocated(self%subparsers)) deallocate(self%subparsers)
        self%subparsers_enabled = .false.
        self%subparsers_required = .false.
        self%help_on_empty = .false.
        self%group_owner = 0
        call self%config_errors%clear()
    end subroutine parser_clear

    logical function valid_subcommand_name(name) result(valid)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: candidate

        candidate = trim(name)
        valid = len(candidate) > 0
        if (.not. valid) return
        valid = candidate(1:1) /= '-'
        if (.not. valid) return
        valid = scan(candidate, " " // achar(9) // new_line('a')) == 0
    end function valid_subcommand_name

    !> Whether any of the indexed arguments is present on every valid command
    !> line: a required option, or a positional that needs at least one value.
    logical function any_always_present(arguments, indices) result(found)
        type(Argument), intent(in) :: arguments(:)
        integer, intent(in) :: indices(:)
        integer :: index

        found = .false.
        do index = 1, size(indices)
            associate (definition => arguments(indices(index)))
                if (definition%required) found = .true.
                if (definition%is_positional() .and. &
                    definition%nargs%min_count() > 0) found = .true.
            end associate
            if (found) return
        end do
    end function any_always_present

    logical function is_special_argument(definition) result(special)
        type(Argument), intent(in) :: definition

        special = .false.
        if (.not. allocated(definition%action)) return
        select type (action => definition%action)
        type is (HelpAction)
            special = .true.
        type is (VersionAction)
            special = .true.
        class default
            special = .false.
        end select
    end function is_special_argument

    logical function is_empty_line_help(parsed) result(empty_line)
        type(ParseResult), intent(in) :: parsed
        type(ErrorEntry) :: entry

        empty_line = .false.
        if (parsed%errors%count() /= 1) return
        entry = parsed%errors%get(1)
        empty_line = entry%code == ERR_NO_ARGUMENTS
    end function is_empty_line_help

    !> Read the process command line into a token array.
    subroutine process_tokens(tokens)
        character(len=:), allocatable, intent(out) :: tokens(:)
        integer :: count, index, argument_length, maximum_length

        count = command_argument_count()
        maximum_length = 1
        do index = 1, count
            call get_command_argument(index, length=argument_length)
            maximum_length = max(maximum_length, argument_length)
        end do
        allocate(character(len=maximum_length) :: tokens(count))
        do index = 1, count
            call get_command_argument(index, value=tokens(index))
        end do
    end subroutine process_tokens

    !> Append tokens to an allocated token array, widening it if needed.
    subroutine append_tokens(tokens, more)
        character(len=:), allocatable, intent(inout) :: tokens(:)
        character(len=*), intent(in) :: more(:)
        character(len=:), allocatable :: joined(:)
        integer :: count

        count = 0
        if (allocated(tokens)) count = size(tokens)
        if (count == 0) then
            tokens = more
            return
        end if
        allocate(character(len=max(len(tokens), len(more))) :: &
            joined(count + size(more)))
        joined(:count) = tokens
        joined(count + 1:) = more
        call move_alloc(joined, tokens)
    end subroutine append_tokens

    subroutine extend_command_path(path, suffix)
        character(len=:), allocatable, intent(inout) :: path(:)
        character(len=:), allocatable, intent(in) :: suffix(:)
        character(len=:), allocatable :: combined(:)
        integer :: path_size, suffix_size, element_length

        if (.not. allocated(suffix)) return
        if (size(suffix) == 0) return
        path_size = 0
        element_length = len(suffix)
        if (allocated(path)) then
            path_size = size(path)
            element_length = max(element_length, len(path))
        end if
        suffix_size = size(suffix)
        allocate(character(len=max(1, element_length)) :: &
            combined(path_size + suffix_size))
        if (path_size > 0) combined(:path_size) = path
        combined(path_size + 1:) = suffix
        call move_alloc(combined, path)
    end subroutine extend_command_path

    !> Show choices as the metavar, as argparse does: `{fast,slow}`.
    function choices_metavar(choices) result(metavar)
        type(ValueBox), intent(in) :: choices
        character(len=:), allocatable :: metavar
        integer :: index

        metavar = "{"
        select type (list => choices%item)
        type is (ListValue)
            do index = 1, list%size()
                if (index > 1) metavar = metavar // ","
                select type (item => list%items(index)%item)
                type is (StringValue)
                    metavar = metavar // item%value
                class default
                    metavar = metavar // list%items(index)%to_string()
                end select
            end do
        end select
        metavar = metavar // "}"
    end function choices_metavar

    integer function choice_count(choices) result(count)
        type(ValueBox), intent(in) :: choices

        count = 0
        if (.not. allocated(choices%item)) return
        select type (list => choices%item)
        type is (ListValue)
            count = list%size()
        end select
    end function choice_count

    !> Spell each choice as a user types it: strings unquoted.
    subroutine choice_spellings(choices, names)
        type(ValueBox), intent(in) :: choices
        character(len=:), allocatable, intent(out) :: names(:)
        character(len=:), allocatable :: text
        integer :: index, width, pass

        width = 1
        do pass = 1, 2
            if (pass == 2) allocate(character(len=width) :: &
                names(choice_count(choices)))
            select type (list => choices%item)
            type is (ListValue)
                do index = 1, list%size()
                    select type (item => list%items(index)%item)
                    type is (StringValue)
                        text = item%value
                    class default
                        text = list%items(index)%to_string()
                    end select
                    if (pass == 1) width = max(width, len(text))
                    if (pass == 2) names(index) = text
                end do
            end select
        end do
    end subroutine choice_spellings

    !> Spell a definition's names the way diagnostics show them: `-j/--threads`.
    function join_names(names) result(text)
        character(len=*), intent(in) :: names(:)
        character(len=:), allocatable :: text
        integer :: index

        text = ""
        do index = 1, size(names)
            if (index > 1) text = text // "/"
            text = text // trim(names(index))
        end do
    end function join_names

    function get_prog_name() result(prog_name)
        character(len=:), allocatable :: prog_name
        character(len=:), allocatable :: arg0
        integer :: length, status, separator

        call get_command_argument(0, length=length, status=status)
        if (status /= 0 .or. length <= 0) then
            prog_name = "program"
            return
        end if

        allocate(character(len=length) :: arg0)
        call get_command_argument(0, value=arg0, status=status)
        if (status /= 0) then
            prog_name = "program"
            return
        end if
        separator = max(scan(arg0, '/', back=.true.), &
            scan(arg0, achar(92), back=.true.))
        if (separator > 0) then
            prog_name = arg0(separator + 1:)
        else
            prog_name = trim(arg0)
        end if
    end function get_prog_name

end module fclap_argparser
