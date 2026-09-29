!> Parser definition facade and transactional argument registration.
module fclap_argparser
    use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
    use fclap_actions_abstract, only : ActionType
    use fclap_actions_builtin, only : StoreAction, StoreConstAction, &
        HelpAction, VersionAction, help_action, version_action
    use fclap_argument, only : Argument, derive_dest_from_names
    use fclap_error_codes, only : ERR_INVALID_ARGUMENT_NAME, &
        ERR_DUPLICATE_OPTION, ERR_DUPLICATE_DEST, ERR_INVALID_NARGS, &
        ERR_INCOMPATIBLE_ACTION, ERR_INVALID_DEFAULT, &
        ERR_INVALID_TYPE_NAME, ERR_INVALID_LIFECYCLE, ERR_INVALID_GROUP
    use fclap_error_stack, only : ErrorStack
    use fclap_formatter_abstract, only : FormatterType
    use fclap_formatter_model, only : HelpModel
    use fclap_formatter_standard, only : StandardFormatter
    use fclap_groups_abstract, only : GroupBox, GroupHandle, GroupType, &
        new_group_handle, new_group_owner_id
    use fclap_groups_helppage, only : ArgumentGroup
    use fclap_groups_mutex, only : MutexGroup
    use fclap_namespace, only : Namespace
    use fclap_nargs, only : NargsSpec, normalize_nargs, NARGS_SUCCESS, &
        NARGS_OPTIONAL, NARGS_REMAINDER
    use fclap_parse_engine, only : parse_definitions
    use fclap_parse_result, only : ParseResult, PARSE_SUCCESS, PARSE_FAILURE, &
        PARSE_HELP, PARSE_VERSION
    use fclap_validators_abstract, only : ValidatorType
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : ListValue
    use fclap_value_convert, only : CONVERT_SUCCESS, convert_value, &
        normalize_type_name, value_in_choices, value_is_list, value_matches_type
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
        integer :: group_owner = 0
        type(Argument), allocatable :: arguments(:)
        type(GroupBox), allocatable :: groups(:)
        type(ErrorStack) :: config_errors
        logical :: subparsers_enabled = .false.
        logical :: subparsers_required = .false.
        character(len=:), allocatable :: subparser_dest
        character(len=:), allocatable :: subparser_title
        character(len=:), allocatable :: subparser_description
        character(len=:), allocatable :: subparser_names(:)
        character(len=:), allocatable :: subparser_helps(:)
        type(ArgumentParser), allocatable :: subparsers(:)
    contains
        procedure :: init => parser_init
        procedure :: init_with_parents => parser_init_with_parents
        procedure :: add_argument => parser_add_argument
        procedure :: add_argument_group => parser_add_argument_group
        procedure :: add_mutually_exclusive_group => parser_add_mutex_group
        procedure :: add_subparsers => parser_add_subparsers
        procedure :: add_parser => parser_add_parser
        procedure :: argument_count => parser_argument_count
        procedure :: group_count => parser_group_count
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
        procedure :: try_parse_args => parser_try_parse_args
        procedure :: parse_args => parser_parse_args
        procedure, private :: append_argument => parser_append_argument
        procedure, private :: append_group => parser_append_group
        procedure, private :: append_subparser => parser_append_subparser
        procedure, private :: find_subparser => parser_find_subparser
        procedure, private :: copy_parent => parser_copy_parent
        procedure, private :: clone_from => parser_clone_from
        procedure, private :: refresh_owner_ids => parser_refresh_owner_ids
        procedure, private :: rebase_program => parser_rebase_program
        procedure, private :: reject => parser_reject
        procedure, private :: clear => parser_clear
    end type ArgumentParser

contains

    subroutine parser_init(self, prog, usage, description, epilog, version, &
        formatter, add_help)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in), optional :: prog, usage, description
        character(len=*), intent(in), optional :: epilog, version
        class(FormatterType), intent(in), optional :: formatter
        logical, intent(in), optional :: add_help

        call self%clear()
        self%group_owner = new_group_owner_id()
        self%add_help = .true.
        if (present(add_help)) self%add_help = add_help

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
        description, epilog, version, formatter, add_help)
        class(ArgumentParser), intent(inout) :: self
        type(ArgumentParser), intent(in) :: parents(:)
        character(len=*), intent(in), optional :: prog, usage, description
        character(len=*), intent(in), optional :: epilog, version
        class(FormatterType), intent(in), optional :: formatter
        logical, intent(in), optional :: add_help
        integer :: index

        call self%init(prog, usage, description, epilog, version, formatter, &
            add_help)
        do index = 1, size(parents)
            call self%copy_parent(parents(index))
        end do
    end subroutine parser_init_with_parents

    subroutine parser_add_argument(self, name1, name2, name3, name4, action, &
        nargs, data_type, default, const, choices, validator, required, help, &
        metavar, dest, visible, deprecated_msg, removed_msg, print_default, &
        print_choices, group)
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

        character(len=:), allocatable :: names(:), canonical_type, action_type
        character(len=:), allocatable :: candidate_dest, validation_message
        class(ActionType), allocatable :: selected_action
        type(NargsSpec) :: nargs_spec
        type(ValueBox) :: default_value, const_value, choice_values
        type(Argument) :: candidate
        logical :: optional_names, required_value, has_default, has_const
        logical :: has_choices, valid
        integer :: stat, group_index

        call collect_names(name1, name2, name3, name4, names)
        if (.not. validate_names(names, optional_names, validation_message)) then
            call self%reject(ERR_INVALID_ARGUMENT_NAME, validation_message, name1)
            return
        end if
        if (has_duplicate_name(names)) then
            call self%reject(ERR_DUPLICATE_OPTION, &
                "an alias is repeated in the same definition", name1)
            return
        end if
        if (any_registered_name(self, names)) then
            call self%reject(ERR_DUPLICATE_OPTION, &
                "an option or positional name is already registered", name1)
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

        if (present(dest)) then
            candidate_dest = trim(dest)
        else
            candidate_dest = derive_dest_from_names(names)
        end if
        if (len(candidate_dest) == 0) then
            call self%reject(ERR_INVALID_ARGUMENT_NAME, &
                "the destination must not be empty", name1)
            return
        end if
        if (self%has_dest(candidate_dest)) then
            call self%reject(ERR_DUPLICATE_DEST, &
                "the destination is already registered", candidate_dest)
            return
        end if

        if (present(action)) then
            allocate(selected_action, source=action)
        else
            allocate(StoreAction :: selected_action)
        end if

        if (present(nargs)) then
            select type (nargs)
            type is (integer)
                call normalize_nargs(nargs, nargs_spec, stat)
            type is (character(len=*))
                call normalize_nargs(nargs, nargs_spec, stat)
            class default
                stat = -1
            end select
        else
            call normalize_nargs(selected_action%default_nargs(), nargs_spec, stat)
        end if
        if (stat /= NARGS_SUCCESS) then
            call self%reject(ERR_INVALID_NARGS, "invalid nargs value", candidate_dest)
            return
        end if
        if (.not. selected_action%accepts_nargs(nargs_spec)) then
            call self%reject(ERR_INCOMPATIBLE_ACTION, &
                "the action does not accept this nargs value", candidate_dest)
            return
        end if
        if (optional_names .and. nargs_spec%value() == NARGS_REMAINDER) then
            call self%reject(ERR_INVALID_NARGS, &
                "remainder nargs is only valid for a positional argument", candidate_dest)
            return
        end if
        if (self%subparsers_enabled .and. .not. optional_names .and. &
            nargs_spec%value() == NARGS_REMAINDER) then
            call self%reject(ERR_INVALID_NARGS, &
                "remainder nargs cannot precede a subparser collection", &
                candidate_dest)
            return
        end if
        if (.not. positional_layout_is_valid(self, optional_names, nargs_spec)) then
            call self%reject(ERR_INVALID_NARGS, &
                "ambiguous or misplaced variable positional argument", candidate_dest)
            return
        end if

        action_type = selected_action%value_type()
        if (present(data_type)) then
            call normalize_type_name(data_type, canonical_type, stat)
        else if (len(action_type) > 0) then
            call normalize_type_name(action_type, canonical_type, stat)
        else
            canonical_type = "string"
            stat = CONVERT_SUCCESS
        end if
        if (stat /= CONVERT_SUCCESS) then
            if (present(data_type)) then
                call self%reject(ERR_INVALID_TYPE_NAME, &
                    "unknown argument data type", candidate_dest)
            else
                call self%reject(ERR_INCOMPATIBLE_ACTION, &
                    "the action does not provide a supported value type", candidate_dest)
            end if
            return
        end if
        if (len(action_type) > 0 .and. action_type /= canonical_type) then
            call self%reject(ERR_INCOMPATIBLE_ACTION, &
                "the action requires data type " // action_type, candidate_dest)
            return
        end if

        if (present(deprecated_msg) .and. present(removed_msg)) then
            call self%reject(ERR_INVALID_LIFECYCLE, &
                "deprecated and removed messages are mutually exclusive", candidate_dest)
            return
        end if

        if (optional_names) then
            required_value = .false.
            if (present(required)) required_value = required
        else
            required_value = nargs_spec%min_count() >= 1
            if (present(required)) then
                if (required .and. nargs_spec%min_count() == 0) then
                    call self%reject(ERR_INCOMPATIBLE_ACTION, &
                        "a zero-minimum positional cannot be required", candidate_dest)
                    return
                end if
            end if
        end if

        has_choices = present(choices)
        if (has_choices) then
            if (nargs_spec%max_count() == 0) then
                call self%reject(ERR_INCOMPATIBLE_ACTION, &
                    "a zero-value action cannot define choices", candidate_dest)
                return
            end if
            call convert_value(choices, canonical_type, choice_values, stat)
            if (stat /= CONVERT_SUCCESS) then
                call self%reject(ERR_INVALID_DEFAULT, &
                    "choices are incompatible with the declared type", candidate_dest)
                return
            end if
        end if
        if (present(validator) .and. nargs_spec%max_count() == 0) then
            call self%reject(ERR_INCOMPATIBLE_ACTION, &
                "a zero-value action cannot define a validator", candidate_dest)
            return
        end if

        has_default = present(default)
        if (has_default) then
            call convert_value(default, canonical_type, default_value, stat)
            if (stat /= CONVERT_SUCCESS) then
                call self%reject(ERR_INVALID_DEFAULT, &
                    "default value is incompatible with the declared type", candidate_dest)
                return
            end if
        else
            call selected_action%implicit_default(default_value, has_default)
        end if
        if (has_default) then
            if (.not. value_matches_type(default_value, canonical_type)) then
                call self%reject(ERR_INVALID_DEFAULT, &
                    "default value has the wrong declared type", candidate_dest)
                return
            end if
            if (value_is_list(default_value) .neqv. &
                selected_action%produces_list(nargs_spec)) then
                call self%reject(ERR_INVALID_DEFAULT, &
                    "default value has the wrong scalar/list shape", candidate_dest)
                return
            end if
        end if

        has_const = .false.
        select type (concrete_action => selected_action)
        type is (StoreConstAction)
            const_value = concrete_action%constant%clone()
            has_const = .true.
            if (const_value%type_name() /= canonical_type) then
                call self%reject(ERR_INCOMPATIBLE_ACTION, &
                    "store_const value does not match the declared type", candidate_dest)
                return
            end if
        end select

        if (present(const)) then
            if (.not. optional_names .or. nargs_spec%value() /= NARGS_OPTIONAL) then
                call self%reject(ERR_INCOMPATIBLE_ACTION, &
                    "const is only valid for an option with nargs='?'", candidate_dest)
                return
            end if
            call convert_value(const, canonical_type, const_value, stat)
            if (stat /= CONVERT_SUCCESS .or. value_is_list(const_value)) then
                call self%reject(ERR_INVALID_DEFAULT, &
                    "const value is incompatible with the declared type", candidate_dest)
                return
            end if
            has_const = .true.
        else if (optional_names .and. nargs_spec%value() == NARGS_OPTIONAL) then
            call self%reject(ERR_INCOMPATIBLE_ACTION, &
                "nargs='?' requires a const value", candidate_dest)
            return
        end if

        if (has_default .and. has_choices) then
            if (.not. value_in_choices(default_value, choice_values)) then
                call self%reject(ERR_INVALID_DEFAULT, &
                    "default value is not one of the allowed choices", candidate_dest)
                return
            end if
        end if
        if (has_const .and. has_choices) then
            if (.not. value_in_choices(const_value, choice_values)) then
                call self%reject(ERR_INVALID_DEFAULT, &
                    "const value is not one of the allowed choices", candidate_dest)
                return
            end if
        end if
        if (present(validator)) then
            if (has_default) then
                call validate_value(validator, default_value, valid, validation_message)
                if (.not. valid) then
                    call self%reject(ERR_INVALID_DEFAULT, &
                        default_validation_message(validation_message), candidate_dest)
                    return
                end if
            end if
            if (has_const) then
                call validate_value(validator, const_value, valid, validation_message)
                if (.not. valid) then
                    call self%reject(ERR_INVALID_DEFAULT, &
                        const_validation_message(validation_message), candidate_dest)
                    return
                end if
            end if
        end if

        call candidate%init(names, nargs_spec, canonical_type, selected_action, &
            dest=candidate_dest, default_value=default_value, &
            const_value=const_value, choices=choice_values, &
            has_default=has_default, has_const=has_const, &
            has_choices=has_choices, validator=validator, required=required_value, &
            help=help, metavar=metavar, visible=visible, &
            deprecated_msg=deprecated_msg, removed_msg=removed_msg, &
            print_default=print_default, print_choices=print_choices)
        call self%append_argument(candidate)
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

    subroutine parser_add_parser(self, name, subparser, help_text)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(ArgumentParser), intent(in), optional :: subparser
        character(len=*), intent(in), optional :: help_text
        type(ArgumentParser) :: definition
        character(len=:), allocatable :: summary

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

        if (present(subparser)) then
            call definition%clone_from(subparser)
        else
            call definition%init(add_help=.true.)
        end if
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
            model%arguments(index)%metavar = &
                self%arguments(index)%effective_metavar()
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
            model%arguments(index)%nargs = &
                self%arguments(index)%nargs_value()
            model%arguments(index)%is_optional = &
                self%arguments(index)%is_optional
            model%arguments(index)%required = self%arguments(index)%required
            model%arguments(index)%visible = self%arguments(index)%visible
            model%arguments(index)%has_default = &
                self%arguments(index)%has_default
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

    recursive function parser_parse_tokens(self, tokens) result(parsed)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: tokens(:)
        type(ParseResult) :: parsed
        type(ParseResult) :: child_result
        type(Argument), allocatable :: definitions(:)
        type(GroupBox), allocatable :: group_definitions(:)
        character(len=:), allocatable :: command_names(:), command
        integer :: child_index, next_token_index

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
        if (allocated(self%subparser_names)) then
            command_names = self%subparser_names
        else
            allocate(character(len=1) :: command_names(0))
        end if

        if (allocated(self%version)) then
            parsed = parse_definitions(definitions, self%config_errors, tokens, &
                group_definitions, version_text=self%version, &
                subcommand_names=command_names, &
                has_subcommands=self%subparsers_enabled, &
                subcommand_required=self%subparsers_required, &
                next_token_index=next_token_index)
        else
            parsed = parse_definitions(definitions, self%config_errors, tokens, &
                group_definitions, subcommand_names=command_names, &
                has_subcommands=self%subparsers_enabled, &
                subcommand_required=self%subparsers_required, &
                next_token_index=next_token_index)
        end if

        if (parsed%outcome == PARSE_HELP) parsed%text = self%format_help()
        if (parsed%outcome /= PARSE_SUCCESS) return
        if (.not. allocated(parsed%selected_command_path)) return

        command = trim(parsed%selected_command_path(1))
        child_index = self%find_subparser(command)
        if (child_index == 0) return

        call parsed%namespace%set_string(self%subparser_dest, command)
        child_result = self%subparsers(child_index)%parse_tokens( &
            tokens(next_token_index:))
        call parsed%namespace%merge(child_result%namespace)
        call parsed%errors%merge(child_result%errors)
        parsed%outcome = child_result%outcome
        call extend_command_path(parsed%selected_command_path, &
            child_result%selected_command_path)
        if (allocated(child_result%text)) then
            parsed%text = child_result%text
        else if (child_result%outcome == PARSE_FAILURE) then
            parsed%text = self%subparsers(child_index)%format_usage()
        end if
    end function parser_parse_tokens

    function parser_try_parse_args(self) result(parsed)
        class(ArgumentParser), intent(in) :: self
        type(ParseResult) :: parsed
        character(len=:), allocatable :: tokens(:)
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
        parsed = self%parse_tokens(tokens)
    end function parser_try_parse_args

    function parser_parse_args(self) result(args)
        class(ArgumentParser), intent(in) :: self
        type(Namespace) :: args
        type(ParseResult) :: parsed

        parsed = self%try_parse_args()
        select case (parsed%outcome)
        case (PARSE_SUCCESS)
            args = parsed%namespace
            if (parsed%errors%has_warnings()) then
                call parsed%errors%print_all(error_unit)
            end if
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
            call parsed%errors%print_all(error_unit)
            flush(error_unit)
            error stop 2, quiet=.true.
        case default
            write(error_unit, '(a)') "fclap: invalid parse outcome"
            flush(error_unit)
            error stop 2, quiet=.true.
        end select
    end function parser_parse_args

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
    end function parser_find_subparser

    subroutine parser_copy_parent(self, parent)
        class(ArgumentParser), intent(inout) :: self
        type(ArgumentParser), intent(in) :: parent
        type(GroupBox) :: copied_group
        integer, allocatable :: index_map(:)
        integer :: argument_index, group_index, member_index, old_index

        call self%config_errors%merge(parent%config_errors)
        allocate(index_map(parent%argument_count()), source=0)
        do argument_index = 1, parent%argument_count()
            if (is_special_argument(parent%arguments(argument_index))) cycle
            if (any_registered_name(self, &
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
        self%group_owner = new_group_owner_id()
        if (allocated(source%arguments)) self%arguments = source%arguments
        if (allocated(source%groups)) self%groups = source%groups
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

    subroutine parser_reject(self, code, message, arg_name)
        class(ArgumentParser), intent(inout) :: self
        integer, intent(in) :: code
        character(len=*), intent(in) :: message, arg_name

        call self%config_errors%add(message, code=code, arg_name=arg_name)
    end subroutine parser_reject

    subroutine parser_clear(self)
        class(ArgumentParser), intent(inout) :: self

        if (allocated(self%prog)) deallocate(self%prog)
        if (allocated(self%usage)) deallocate(self%usage)
        if (allocated(self%description)) deallocate(self%description)
        if (allocated(self%epilog)) deallocate(self%epilog)
        if (allocated(self%version)) deallocate(self%version)
        if (allocated(self%formatter)) deallocate(self%formatter)
        if (allocated(self%arguments)) deallocate(self%arguments)
        if (allocated(self%groups)) deallocate(self%groups)
        if (allocated(self%subparser_dest)) deallocate(self%subparser_dest)
        if (allocated(self%subparser_title)) deallocate(self%subparser_title)
        if (allocated(self%subparser_description)) &
            deallocate(self%subparser_description)
        if (allocated(self%subparser_names)) deallocate(self%subparser_names)
        if (allocated(self%subparser_helps)) deallocate(self%subparser_helps)
        if (allocated(self%subparsers)) deallocate(self%subparsers)
        self%subparsers_enabled = .false.
        self%subparsers_required = .false.
        self%group_owner = 0
        call self%config_errors%clear()
    end subroutine parser_clear

    logical function any_registered_name(self, names) result(found)
        class(ArgumentParser), intent(in) :: self
        character(len=*), intent(in) :: names(:)
        integer :: index

        found = .false.
        do index = 1, size(names)
            if (self%has_option(names(index))) then
                found = .true.
                return
            end if
        end do
    end function any_registered_name

    logical function positional_layout_is_valid(self, optional_names, nargs) &
        result(valid)
        class(ArgumentParser), intent(in) :: self
        logical, intent(in) :: optional_names
        type(NargsSpec), intent(in) :: nargs
        integer :: index

        valid = .true.
        if (optional_names) return
        do index = 1, self%argument_count()
            if (.not. self%arguments(index)%is_positional()) cycle
            if (self%arguments(index)%nargs_value() == NARGS_REMAINDER) then
                valid = .false.
                return
            end if
            if (nargs%is_variable() .and. self%arguments(index)%nargs%is_variable()) then
                valid = .false.
                return
            end if
        end do
    end function positional_layout_is_valid

    subroutine validate_value(validator, value, valid, message)
        class(ValidatorType), intent(in) :: validator
        type(ValueBox), intent(in) :: value
        logical, intent(out) :: valid
        character(len=:), allocatable, intent(out) :: message
        integer :: index

        select type (list => value%item)
        type is (ListValue)
            valid = .true.
            message = ""
            do index = 1, list%size()
                call validator%validate(list%items(index), valid, message)
                if (.not. valid) return
            end do
        class default
            call validator%validate(value, valid, message)
        end select
    end subroutine validate_value

    function default_validation_message(detail) result(message)
        character(len=*), intent(in) :: detail
        character(len=:), allocatable :: message

        message = "default value failed validation"
        if (len_trim(detail) > 0) message = message // ": " // trim(detail)
    end function default_validation_message

    function const_validation_message(detail) result(message)
        character(len=*), intent(in) :: detail
        character(len=:), allocatable :: message

        message = "const value failed validation"
        if (len_trim(detail) > 0) message = message // ": " // trim(detail)
    end function const_validation_message

    subroutine collect_names(name1, name2, name3, name4, names)
        character(len=*), intent(in) :: name1
        character(len=*), intent(in), optional :: name2, name3, name4
        character(len=:), allocatable, intent(out) :: names(:)
        integer :: number, max_length

        number = 1
        max_length = len_trim(name1)
        if (present(name2)) then
            number = number + 1
            max_length = max(max_length, len_trim(name2))
        end if
        if (present(name3)) then
            number = number + 1
            max_length = max(max_length, len_trim(name3))
        end if
        if (present(name4)) then
            number = number + 1
            max_length = max(max_length, len_trim(name4))
        end if

        allocate(character(len=max_length) :: names(number))
        names(1) = trim(name1)
        number = 1
        if (present(name2)) then
            number = number + 1
            names(number) = trim(name2)
        end if
        if (present(name3)) then
            number = number + 1
            names(number) = trim(name3)
        end if
        if (present(name4)) then
            number = number + 1
            names(number) = trim(name4)
        end if
    end subroutine collect_names

    logical function validate_names(names, optional_names, message) result(valid)
        character(len=*), intent(in) :: names(:)
        logical, intent(out) :: optional_names
        character(len=:), allocatable, intent(out) :: message
        logical :: current_optional
        integer :: index

        valid = .false.
        message = ""
        optional_names = .false.
        if (size(names) == 0) then
            message = "at least one argument name is required"
            return
        end if
        if (len_trim(names(1)) == 0) then
            message = "argument names must not be empty"
            return
        end if
        optional_names = names(1)(1:1) == '-'
        if (.not. optional_names .and. size(names) /= 1) then
            message = "a positional argument must have exactly one name"
            return
        end if

        do index = 1, size(names)
            if (len_trim(names(index)) == 0) then
                message = "argument names must not be empty"
                return
            end if
            if (trim(names(index)) == "-" .or. trim(names(index)) == "--") then
                message = "'-' and '--' are reserved tokens"
                return
            end if
            current_optional = names(index)(1:1) == '-'
            if (current_optional .neqv. optional_names) then
                message = "positional and optional names cannot be mixed"
                return
            end if
        end do
        valid = .true.
    end function validate_names

    pure logical function has_duplicate_name(names) result(duplicate)
        character(len=*), intent(in) :: names(:)
        integer :: left, right

        duplicate = .false.
        do left = 1, size(names) - 1
            do right = left + 1, size(names)
                if (trim(names(left)) == trim(names(right))) then
                    duplicate = .true.
                    return
                end if
            end do
        end do
    end function has_duplicate_name

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
