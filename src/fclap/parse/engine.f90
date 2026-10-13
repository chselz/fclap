!> Side-effect-free token parser for one flat argument definition array.
module fclap_parse_engine
    use fclap_actions_abstract, only : ACTION_CONTINUE, &
        ACTION_HELP_REQUESTED, ACTION_VERSION_REQUESTED
    use fclap_actions_boolean, only : BooleanOptionalAction
    use fclap_argument, only : Argument
    use fclap_error_entry, only : ErrorEntry
    use fclap_namespace, only : Namespace
    use fclap_error_codes, only : ERROR_WARNING, ERR_DEPRECATED_ARGUMENT, &
        ERR_EXTRA_POSITIONAL, ERR_INVALID_CHOICE, ERR_INVALID_VALUE, &
        ERR_MISSING_REQUIRED, ERR_MISSING_VALUE, ERR_REMOVED_ARGUMENT, &
        ERR_UNKNOWN_ARGUMENT, ERR_VALIDATION_FAILED, ERR_UNKNOWN_SUBCOMMAND, &
        ERR_MISSING_SUBCOMMAND, ERR_UNEXPECTED_VALUE
    use fclap_error_stack, only : ErrorStack
    use fclap_groups_abstract, only : GroupBox
    use fclap_nargs, only : NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, &
        NARGS_ONE_OR_MORE, NARGS_REMAINDER, NARGS_ZERO
    use fclap_parse_context, only : ParseContext
    use fclap_rules, only : ArgumentRule
    use fclap_parse_result, only : ParseResult, PARSE_SUCCESS, PARSE_FAILURE, &
        PARSE_HELP, PARSE_VERSION, SOURCE_DEFAULT, SOURCE_ENVIRONMENT, &
        SOURCE_COMMAND_LINE
    use fclap_validators_choices, only : ChoicesValidator, choices_validator
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : ListValue
    use fclap_value_builtin, only : StringValue, LogicalValue
    use fclap_value_convert, only : CONVERT_SUCCESS, convert_value, &
        canonicalize_choice, lowercase
    use fclap_utils_similarity, only : closest_match
    implicit none
    private

    integer, parameter :: BOUNDARY_END = 0
    integer, parameter :: BOUNDARY_MARKER = 1
    integer, parameter :: BOUNDARY_KNOWN_OPTION = 2
    integer, parameter :: BOUNDARY_UNKNOWN_OPTION = 3
    integer, parameter :: BOUNDARY_LIMIT = 4
    integer, parameter :: BOUNDARY_SUBCOMMAND = 5

    ! How an option token names a registered option.
    integer, parameter :: OPTION_NONE = 0
    integer, parameter :: OPTION_EXACT = 1
    integer, parameter :: OPTION_ATTACHED = 2
    integer, parameter :: OPTION_CLUSTER = 3

    public :: parse_definitions

contains

    function parse_definitions(arguments, config_errors, tokens, groups, &
        version_text, subcommand_names, has_subcommands, &
        subcommand_required, next_token_index, rules, environment) &
        result(parsed)
        type(Argument), intent(in) :: arguments(:)
        type(ErrorStack), intent(in) :: config_errors
        character(len=*), intent(in) :: tokens(:)
        type(GroupBox), intent(in) :: groups(:)
        character(len=*), intent(in), optional :: version_text
        character(len=*), intent(in), optional :: subcommand_names(:)
        logical, intent(in), optional :: has_subcommands, subcommand_required
        integer, intent(out), optional :: next_token_index
        type(ArgumentRule), intent(in), optional :: rules(:)
        !> `NAME=value` entries replacing the process environment
        character(len=*), intent(in), optional :: environment(:)
        type(ParseResult) :: parsed
        type(ParseContext) :: context
        character(len=:), allocatable :: token, attached
        integer :: argument_index, action_outcome, form

        call context%init(tokens, size(arguments))
        call collect_option_names(arguments, context)
        if (present(environment)) context%environment = environment
        if (present(has_subcommands)) then
            if (has_subcommands) then
                if (present(subcommand_names)) then
                    call context%configure_subcommands(subcommand_names, &
                        subcommand_required)
                else
                    call configure_empty_subcommands(context, subcommand_required)
                end if
            end if
        else if (present(subcommand_names)) then
            call context%configure_subcommands(subcommand_names, &
                subcommand_required)
        end if
        if (config_errors%has_fatal_errors()) then
            call context%errors%merge(config_errors)
            call finish_result(parsed, context, PARSE_FAILURE, next_token_index)
            return
        end if

        call apply_defaults(arguments, context)
        action_outcome = ACTION_CONTINUE

        do while (.not. context%at_end())
            token = context%current_token()

            if (context%options_enabled) then
                if (token == "--") then
                    context%options_enabled = .false.
                    call context%advance()
                    cycle
                end if
            end if

            argument_index = 0
            form = OPTION_NONE
            if (context%options_enabled) then
                call resolve_option(arguments, token, argument_index, form, &
                    attached)
            end if
            if (form == OPTION_EXACT) then
                call consume_option(arguments, argument_index, context, &
                    action_outcome, token)
            else if (form == OPTION_ATTACHED) then
                call consume_option(arguments, argument_index, context, &
                    action_outcome, token, attached, &
                    attached_spelling(token, attached))
            else if (form == OPTION_CLUSTER) then
                call consume_cluster(arguments, context, action_outcome, token)
            else
                argument_index = next_positional(arguments, context)
                if (context%options_enabled .and. token_starts_option(token)) then
                    if (argument_index > 0) then
                        if (token_is_numeric_value( &
                            arguments(argument_index), token)) then
                            call consume_positional(arguments, argument_index, &
                                context, action_outcome)
                        else
                            call add_unknown_option(context, token, context%cursor)
                        end if
                    else
                        call add_unknown_option(context, token, context%cursor)
                    end if
                else if (argument_index > 0) then
                    call consume_positional(arguments, argument_index, context, &
                        action_outcome)
                else if (context%subcommands_enabled) then
                    if (context%is_subcommand(token)) then
                        call context%select_command(token)
                        call context%advance()
                        exit
                    end if
                    call context%errors%add("unknown subcommand", &
                        code=ERR_UNKNOWN_SUBCOMMAND, arg_name=token, &
                        token=token, token_index=context%cursor, &
                        suggestion=suggest_subcommand(context, token))
                else
                    call context%errors%add("unexpected positional argument", &
                        code=ERR_EXTRA_POSITIONAL, token=token, &
                        token_index=context%cursor)
                end if
            end if

            if (context%errors%has_fatal_errors()) exit
            if (action_outcome /= ACTION_CONTINUE) exit
        end do

        if (action_outcome == ACTION_CONTINUE .and. &
            .not. context%errors%has_fatal_errors()) then
            call materialize_empty_positionals(arguments, context, action_outcome)
            if (action_outcome == ACTION_CONTINUE .and. &
                .not. context%errors%has_fatal_errors()) &
                call apply_environment(arguments, context)
        else if (context%errors%has_fatal_errors()) then
            call find_later_exit_request(arguments, context, action_outcome)
        end if

        if (action_outcome == ACTION_HELP_REQUESTED) then
            call finish_result(parsed, context, PARSE_HELP, next_token_index)
            return
        else if (action_outcome == ACTION_VERSION_REQUESTED) then
            call finish_result(parsed, context, PARSE_VERSION, next_token_index)
            if (present(version_text)) parsed%text = trim(version_text)
            return
        end if

        call check_required(arguments, context)
        call validate_groups(arguments, groups, context)
        if (present(rules)) call validate_rules(arguments, rules, context)
        if (context%subcommands_enabled .and. context%subcommand_required .and. &
            .not. allocated(context%selected_command_path)) then
            call context%errors%add("a subcommand is required", &
                code=ERR_MISSING_SUBCOMMAND)
        end if
        if (context%errors%has_fatal_errors()) then
            call finish_result(parsed, context, PARSE_FAILURE, next_token_index)
        else
            call finish_result(parsed, context, PARSE_SUCCESS, next_token_index)
        end if
        call record_sources(arguments, context, parsed)
    end function parse_definitions

    !> Fill options not given on the command line from their environment
    !> variables. An empty variable counts as unset.
    !>
    !> A value from the environment is converted and validated like a
    !> command-line value and counts as explicitly given, so it satisfies
    !> `required` and takes part in groups and rules.
    subroutine apply_environment(arguments, context)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(inout) :: context
        character(len=:), allocatable :: text
        integer :: index, outcome
        logical :: found

        do index = 1, size(arguments)
            if (.not. allocated(arguments(index)%env)) cycle
            if (context%was_seen(index)) cycle
            call lookup_environment(context, arguments(index)%env, text, found)
            if (.not. found) cycle
            call apply_environment_value(arguments(index), index, context, &
                text, outcome)
            if (context%errors%has_fatal_errors()) return
            if (context%was_seen(index)) context%from_env(index) = .true.
        end do
    end subroutine apply_environment

    !> Value of an environment variable, from the explicit environment given
    !> to the parse when there is one, otherwise from the process.
    subroutine lookup_environment(context, name, text, found)
        type(ParseContext), intent(in) :: context
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        integer :: current, length, stat

        text = ""
        found = .false.
        if (allocated(context%environment)) then
            ! The last entry for a name wins, as when a shell exports twice.
            do current = 1, size(context%environment)
                associate (entry => context%environment(current))
                    if (index(entry, "=") /= len(name) + 1) cycle
                    if (entry(:len(name)) /= name) cycle
                    text = trim(entry(len(name) + 2:))
                end associate
            end do
        else
            call get_environment_variable(name, length=length, status=stat)
            if (stat /= 0 .or. length == 0) return
            deallocate(text)
            allocate(character(len=length) :: text)
            call get_environment_variable(name, value=text, status=stat)
            if (stat /= 0) text = ""
        end if
        found = len_trim(text) > 0
    end subroutine lookup_environment

    !> Apply one environment value as if the option had been given with it.
    !>
    !> A flag reads the value as a logical: true acts like the flag, false
    !> leaves the option unset, except that a `--x/--no-x` flag stores false.
    !> An option taking several values splits the text at blanks.
    subroutine apply_environment_value(definition, index, context, text, &
        outcome)
        type(Argument), intent(in) :: definition
        integer, intent(in) :: index
        type(ParseContext), intent(inout) :: context
        character(len=*), intent(in) :: text
        integer, intent(out) :: outcome
        character(len=:), allocatable :: origin, words(:)
        type(ValueBox) :: value, item
        type(ListValue) :: list
        integer :: stat, word
        logical :: enabled

        outcome = ACTION_CONTINUE
        origin = "environment variable " // definition%env // ": "

        if (definition%nargs_value() == NARGS_ZERO) then
            call convert_value(text, "logical", value, stat)
            enabled = .false.
            if (stat == CONVERT_SUCCESS) then
                select type (flag => value%item)
                type is (LogicalValue)
                    enabled = flag%value
                end select
            else
                call context%errors%add(origin // "invalid logical value", &
                    code=ERR_INVALID_VALUE, arg_name=definition%dest, &
                    token=text)
                return
            end if
            if (enabled) then
                call execute_action(definition, index, value, context, &
                    outcome, mark_explicit=.true.)
            else
                select type (action => definition%action)
                type is (BooleanOptionalAction)
                    call context%namespace%set_logical(definition%dest, .false.)
                    call context%mark_seen(index)
                end select
            end if
            return
        end if

        if (.not. definition%nargs%produces_list()) then
            call convert_and_validate_text(definition, context, text, 0, &
                value, origin)
            if (context%errors%has_fatal_errors()) return
            call execute_action(definition, index, value, context, outcome, &
                mark_explicit=.true.)
            return
        end if

        call split_words(text, words)
        if (size(words) < definition%nargs%min_count() .or. &
            size(words) > definition%nargs%max_count()) then
            call context%errors%add(origin // "wrong number of values", &
                code=ERR_MISSING_VALUE, arg_name=definition%dest, token=text)
            return
        end if
        call list%initialize(definition%data_type)
        do word = 1, size(words)
            call convert_and_validate_text(definition, context, &
                trim(words(word)), 0, item, origin)
            if (context%errors%has_fatal_errors()) return
            call list%append(item)
        end do
        call value%set(list)
        call execute_action(definition, index, value, context, outcome, &
            mark_explicit=.true.)
    end subroutine apply_environment_value

    !> Split text at blanks and tabs into its non-empty words.
    subroutine split_words(text, words)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: words(:)
        character(len=*), parameter :: blanks = " " // achar(9)
        integer :: first, last, count, pass

        do pass = 1, 2
            count = 0
            first = 1
            do
                first = first - 1 + verify(text(first:) // " ", blanks)
                if (first > len(text)) exit
                last = scan(text(first:) // " ", blanks)
                last = first + last - 2
                count = count + 1
                if (pass == 2) words(count) = text(first:last)
                first = last + 1
                if (first > len(text)) exit
            end do
            if (pass == 1) allocate(character(len=max(1, len(text))) :: &
                words(count))
        end do
    end subroutine split_words

    !> Record where each destination's value came from.
    subroutine record_sources(arguments, context, parsed)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(in) :: context
        type(ParseResult), intent(inout) :: parsed
        integer :: index

        do index = 1, size(arguments)
            if (.not. allocated(arguments(index)%dest)) cycle
            if (context%from_env(index)) then
                call parsed%set_source(arguments(index)%dest, SOURCE_ENVIRONMENT)
            else if (context%was_seen(index)) then
                call parsed%set_source(arguments(index)%dest, &
                    SOURCE_COMMAND_LINE)
            else if (parsed%namespace%contains(arguments(index)%dest)) then
                call parsed%set_source(arguments(index)%dest, SOURCE_DEFAULT)
            end if
        end do
    end subroutine record_sources

    !> Let `-h` or `--version` later on the line win over an unrecognized
    !> token, as argparse does; conversion and validation errors still win.
    !>
    !> When an exit request is found, fatal diagnostics are dropped so help
    !> and version outcomes never carry errors.
    subroutine find_later_exit_request(arguments, context, action_outcome)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(inout) :: context
        integer, intent(inout) :: action_outcome
        type(ErrorEntry) :: last
        type(ErrorStack) :: kept, scratch_errors
        type(Namespace) :: scratch
        type(ValueBox) :: no_value
        character(len=:), allocatable :: token
        integer :: position, argument_index, outcome, index

        last = context%errors%get(context%errors%count())
        if (last%code /= ERR_UNKNOWN_ARGUMENT .and. &
            last%code /= ERR_EXTRA_POSITIONAL) return

        do position = context%cursor, size(context%tokens)
            token = trim(context%tokens(position))
            if (token == "--") return
            argument_index = find_option(arguments, token)
            if (argument_index == 0) cycle
            if (arguments(argument_index)%nargs_value() /= NARGS_ZERO) cycle
            call arguments(argument_index)%action%apply( &
                arguments(argument_index)%dest, no_value, scratch, outcome, &
                scratch_errors)
            if (outcome == ACTION_CONTINUE) cycle

            action_outcome = outcome
            do index = 1, context%errors%count()
                last = context%errors%get(index)
                if (last%severity == ERROR_WARNING) call kept%append(last)
            end do
            context%errors = kept
            return
        end do
    end subroutine find_later_exit_request

    subroutine configure_empty_subcommands(context, required)
        type(ParseContext), intent(inout) :: context
        logical, intent(in), optional :: required
        character(len=1) :: names(0)

        call context%configure_subcommands(names, required)
    end subroutine configure_empty_subcommands

    subroutine apply_defaults(arguments, context)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(inout) :: context
        integer :: index

        do index = 1, size(arguments)
            if (.not. arguments(index)%has_default) cycle
            call context%namespace%set_value(arguments(index)%dest, &
                arguments(index)%default_value)
        end do
    end subroutine apply_defaults

    !> Consume one option occurrence starting at the current token.
    !>
    !> `option_token` is the spelling used in diagnostics. When `attached` is
    !> present the option was written as `--name=value` or `-nVALUE`, and that
    !> text is the only value of this occurrence.
    subroutine consume_option(arguments, index, context, action_outcome, &
        option_token, attached, option_name)
        type(Argument), intent(in) :: arguments(:)
        integer, intent(in) :: index
        type(ParseContext), intent(inout) :: context
        integer, intent(out) :: action_outcome
        character(len=*), intent(in) :: option_token
        character(len=*), intent(in), optional :: attached
        !> Registered spelling that matched; defaults to `option_token`
        character(len=*), intent(in), optional :: option_name
        type(ValueBox) :: value
        character(len=:), allocatable :: name
        integer :: option_token_index, available, boundary, count
        integer :: minimum, maximum
        logical :: continue_parsing

        action_outcome = ACTION_CONTINUE
        name = option_token
        if (present(option_name)) name = option_name
        option_token_index = context%cursor
        call record_lifecycle(arguments(index), option_token, option_token_index, &
            context, continue_parsing)
        if (.not. continue_parsing) return
        call context%advance()

        if (present(attached)) then
            call consume_attached_value(arguments(index), index, context, &
                action_outcome, option_token, option_token_index, attached, name)
            return
        end if

        if (arguments(index)%nargs_value() == NARGS_ZERO) then
            call execute_action(arguments(index), index, value, context, &
                action_outcome, mark_explicit=.true., option_name=name)
            return
        end if

        minimum = arguments(index)%nargs%min_count()
        maximum = arguments(index)%nargs%max_count()
        call scan_available_values(arguments, arguments(index), context, &
            maximum, .false., available, boundary)
        count = min(available, maximum)

        if (count < minimum) then
            if (boundary == BOUNDARY_UNKNOWN_OPTION) then
                call add_unknown_at(context, context%cursor + available)
            else
                call add_missing_value(context, arguments(index), option_token, &
                    option_token_index)
            end if
            return
        end if

        if (arguments(index)%nargs_value() == NARGS_OPTIONAL .and. count == 0) then
            value = arguments(index)%const_value%clone()
        else
            call convert_token_range(arguments(index), context, context%cursor, &
                count, value)
            if (context%errors%has_fatal_errors()) return
        end if

        call execute_action(arguments(index), index, value, context, &
            action_outcome, mark_explicit=.true., option_name=name)
        if (context%errors%has_fatal_errors()) return
        call advance_values(context, count)
    end subroutine consume_option

    !> Apply the single value written inside an option token.
    subroutine consume_attached_value(definition, index, context, &
        action_outcome, option_token, option_token_index, attached, option_name)
        type(Argument), intent(in) :: definition
        integer, intent(in) :: index
        type(ParseContext), intent(inout) :: context
        integer, intent(out) :: action_outcome
        character(len=*), intent(in) :: option_token, attached, option_name
        integer, intent(in) :: option_token_index
        type(ValueBox) :: value, item
        type(ListValue) :: list

        action_outcome = ACTION_CONTINUE
        if (definition%nargs%max_count() == 0) then
            call context%errors%add("option does not take a value", &
                code=ERR_UNEXPECTED_VALUE, arg_name=definition%dest, &
                token=option_token, token_index=option_token_index)
            return
        end if
        if (definition%nargs%min_count() > 1) then
            call add_missing_value(context, definition, option_token, &
                option_token_index)
            return
        end if

        if (definition%nargs%produces_list()) then
            call list%initialize(definition%data_type)
            call convert_and_validate_text(definition, context, attached, &
                option_token_index, item)
            if (context%errors%has_fatal_errors()) return
            call list%append(item)
            call value%set(list)
        else
            call convert_and_validate_text(definition, context, attached, &
                option_token_index, value)
            if (context%errors%has_fatal_errors()) return
        end if
        call execute_action(definition, index, value, context, &
            action_outcome, mark_explicit=.true., option_name=option_name)
    end subroutine consume_attached_value

    !> Consume combined single-letter options such as `-vvx` or `-vj4`.
    !>
    !> Every letter must name a registered one-letter option. Flags are applied
    !> in order; the first letter whose option takes values ends the cluster
    !> and receives the remaining text, or the following tokens when the
    !> remainder is empty.
    subroutine consume_cluster(arguments, context, action_outcome, token)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(inout) :: context
        integer, intent(out) :: action_outcome
        character(len=*), intent(in) :: token
        type(ValueBox) :: value
        integer :: argument_index, position
        logical :: continue_parsing

        action_outcome = ACTION_CONTINUE
        do position = 2, len(token)
            argument_index = find_option(arguments, "-" // token(position:position))
            if (argument_index == 0) then
                call add_unknown_option(context, token, context%cursor)
                return
            end if

            if (arguments(argument_index)%nargs_value() /= NARGS_ZERO) then
                if (position < len(token)) then
                    call consume_option(arguments, argument_index, context, &
                        action_outcome, token, token(position + 1:), &
                        "-" // token(position:position))
                else
                    call consume_option(arguments, argument_index, context, &
                        action_outcome, token, &
                        option_name="-" // token(position:position))
                end if
                return
            end if

            call record_lifecycle(arguments(argument_index), token, &
                context%cursor, context, continue_parsing)
            if (.not. continue_parsing) return
            call execute_action(arguments(argument_index), argument_index, &
                value, context, action_outcome, mark_explicit=.true., &
                option_name="-" // token(position:position))
            if (context%errors%has_fatal_errors()) return
            if (action_outcome /= ACTION_CONTINUE) exit
        end do
        call context%advance()
    end subroutine consume_cluster

    !> Classify a token as a registered option, an option with an attached
    !> value, or a cluster of one-letter flags.
    !>
    !> An exact registered spelling always wins. Otherwise `--name=value` and
    !> `-n=value` split at the first `=`, and `-nVALUE` attaches everything
    !> after a one-letter option that takes values. A one-letter flag followed
    !> by more letters forms a cluster.
    subroutine resolve_option(arguments, token, argument_index, form, attached)
        type(Argument), intent(in) :: arguments(:)
        character(len=*), intent(in) :: token
        integer, intent(out) :: argument_index, form
        character(len=:), allocatable, intent(out) :: attached
        integer :: separator

        form = OPTION_NONE
        attached = ""
        argument_index = find_option(arguments, token)
        if (argument_index > 0) then
            form = OPTION_EXACT
            return
        end if
        if (.not. token_starts_option(token)) return

        separator = index(token, "=")
        if (separator > 2) then
            argument_index = find_option(arguments, token(:separator - 1))
            if (argument_index > 0) then
                form = OPTION_ATTACHED
                attached = token(separator + 1:)
                return
            end if
        end if

        if (len(token) > 2 .and. token(2:2) /= "-") then
            argument_index = find_option(arguments, token(1:2))
            if (argument_index > 0) then
                if (arguments(argument_index)%nargs_value() == NARGS_ZERO) then
                    form = OPTION_CLUSTER
                else
                    form = OPTION_ATTACHED
                    attached = token(3:)
                end if
                return
            end if
        end if
        argument_index = 0
    end subroutine resolve_option

    subroutine consume_positional(arguments, index, context, action_outcome)
        type(Argument), intent(in) :: arguments(:)
        integer, intent(in) :: index
        type(ParseContext), intent(inout) :: context
        integer, intent(out) :: action_outcome
        type(ValueBox) :: value
        character(len=:), allocatable :: first_token
        integer :: available, boundary, count, minimum, maximum, reserve
        integer :: scan_maximum
        integer :: nargs_value
        logical :: continue_parsing

        action_outcome = ACTION_CONTINUE
        nargs_value = arguments(index)%nargs_value()
        minimum = arguments(index)%nargs%min_count()
        maximum = arguments(index)%nargs%max_count()

        scan_maximum = maximum
        if (nargs_value == NARGS_OPTIONAL) scan_maximum = huge(0)
        call scan_available_values(arguments, arguments(index), context, &
            scan_maximum, nargs_value == NARGS_REMAINDER, available, boundary)

        reserve = 0
        select case (nargs_value)
        case (NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, NARGS_ONE_OR_MORE)
            reserve = later_positional_minimum(arguments, index)
            count = min(maximum, max(0, available - reserve))
        case default
            count = min(maximum, available)
        end select

        if (count < minimum) then
            if (boundary == BOUNDARY_UNKNOWN_OPTION .and. available < minimum) then
                call add_unknown_at(context, context%cursor + available)
            else
                call add_missing_positional_value(context, arguments(index))
            end if
            return
        end if

        if (count == 0) then
            if ((nargs_value == NARGS_ZERO_OR_MORE .or. &
                nargs_value == NARGS_REMAINDER) .and. &
                .not. arguments(index)%has_default) then
                call make_empty_list(arguments(index)%data_type, value)
                call execute_action(arguments(index), index, value, context, &
                    action_outcome, mark_explicit=.false.)
            end if
            call context%mark_completed(index)
            context%positional_cursor = index + 1
            return
        end if

        first_token = trim(context%tokens(context%cursor))
        call record_lifecycle(arguments(index), first_token, context%cursor, &
            context, continue_parsing)
        if (.not. continue_parsing) return

        call convert_token_range(arguments(index), context, context%cursor, &
            count, value)
        if (context%errors%has_fatal_errors()) return
        call execute_action(arguments(index), index, value, context, &
            action_outcome, mark_explicit=.true.)
        if (context%errors%has_fatal_errors()) return

        call advance_values(context, count)
        call context%mark_completed(index)
        context%positional_cursor = index + 1
    end subroutine consume_positional

    subroutine scan_available_values(arguments, definition, context, maximum, &
        remainder, count, boundary)
        type(Argument), intent(in) :: arguments(:)
        type(Argument), intent(in) :: definition
        type(ParseContext), intent(in) :: context
        integer, intent(in) :: maximum
        logical, intent(in) :: remainder
        integer, intent(out) :: count, boundary
        character(len=:), allocatable :: token
        integer :: position

        count = 0
        boundary = BOUNDARY_END
        do while (count < maximum)
            position = context%cursor + count
            if (position > size(context%tokens)) return

            token = trim(context%tokens(position))
            if (definition%nargs%is_variable() .and. &
                context%is_subcommand(token)) then
                boundary = BOUNDARY_SUBCOMMAND
                return
            end if
            if (.not. remainder .and. context%options_enabled) then
                if (token == "--") then
                    boundary = BOUNDARY_MARKER
                    return
                end if
                if (names_option(arguments, token)) then
                    boundary = BOUNDARY_KNOWN_OPTION
                    return
                end if
                if (token_starts_option(token)) then
                    if (.not. token_is_numeric_value(definition, token)) then
                        boundary = BOUNDARY_UNKNOWN_OPTION
                        return
                    end if
                end if
            end if
            count = count + 1
        end do
        boundary = BOUNDARY_LIMIT
    end subroutine scan_available_values

    subroutine convert_token_range(definition, context, first, count, value)
        type(Argument), intent(in) :: definition
        type(ParseContext), intent(inout) :: context
        integer, intent(in) :: first, count
        type(ValueBox), intent(out) :: value
        type(ValueBox) :: item
        type(ListValue) :: list
        integer :: offset

        if (definition%nargs%produces_list()) then
            call list%initialize(definition%data_type)
            do offset = 0, count - 1
                call convert_and_validate_token(definition, context, &
                    first + offset, item)
                if (context%errors%has_fatal_errors()) return
                call list%append(item)
            end do
            call value%set(list)
        else
            call convert_and_validate_token(definition, context, first, value)
        end if
    end subroutine convert_token_range

    subroutine convert_and_validate_token(definition, context, token_index, value)
        type(Argument), intent(in) :: definition
        type(ParseContext), intent(inout) :: context
        integer, intent(in) :: token_index
        type(ValueBox), intent(out) :: value

        call convert_and_validate_text(definition, context, &
            trim(context%tokens(token_index)), token_index, value)
    end subroutine convert_and_validate_token

    subroutine convert_and_validate_text(definition, context, token, &
        token_index, value, origin)
        type(Argument), intent(in) :: definition
        type(ParseContext), intent(inout) :: context
        character(len=*), intent(in) :: token
        integer, intent(in) :: token_index
        type(ValueBox), intent(out) :: value
        !> Prefix naming where the text came from, e.g. an environment variable
        character(len=*), intent(in), optional :: origin
        type(ChoicesValidator) :: choice_check
        character(len=:), allocatable :: message, prefix
        integer :: stat
        logical :: valid

        prefix = ""
        if (present(origin)) prefix = origin

        call convert_value(token, definition%data_type, value, stat)
        if (stat /= CONVERT_SUCCESS) then
            call context%errors%add(prefix // "invalid " // &
                definition%data_type // " value", code=ERR_INVALID_VALUE, &
                arg_name=definition%dest, token=token, token_index=token_index)
            return
        end if

        if (definition%has_choices) then
            choice_check = choices_validator(definition%choices, &
                definition%case_insensitive_choices)
            call choice_check%validate(value, valid, message)
            if (.not. valid) then
                call context%errors%add(prefix // message, &
                    code=ERR_INVALID_CHOICE, arg_name=definition%dest, &
                    token=token, &
                    token_index=token_index, &
                    suggestion=suggest_choice(definition, token))
                return
            end if
            if (definition%case_insensitive_choices) &
                call canonicalize_choice(value, definition%choices, .true.)
        end if

        if (allocated(definition%validator)) then
            call definition%validator%validate(value, valid, message)
            if (.not. valid) then
                if (.not. allocated(message)) message = ""
                if (len_trim(message) == 0) message = &
                    "argument value failed validation"
                call context%errors%add(prefix // message, &
                    code=ERR_VALIDATION_FAILED, arg_name=definition%dest, &
                    token=token, token_index=token_index)
            end if
        end if
    end subroutine convert_and_validate_text

    !> Run an argument's action and record explicit presence.
    !>
    !> Options pass the matched spelling so actions such as `--no-name` can
    !> react to it. An accumulating action that replaces its default starts
    !> from an empty value on the first explicit occurrence.
    subroutine execute_action(definition, index, value, context, action_outcome, &
        mark_explicit, option_name)
        type(Argument), intent(in) :: definition
        integer, intent(in) :: index
        type(ValueBox), intent(in) :: value
        type(ParseContext), intent(inout) :: context
        integer, intent(out) :: action_outcome
        logical, intent(in) :: mark_explicit
        character(len=*), intent(in), optional :: option_name

        action_outcome = ACTION_CONTINUE
        if (mark_explicit .and. definition%has_default) then
            if (.not. context%was_seen(index) .and. &
                definition%action%replaces_default()) then
                call context%namespace%remove(definition%dest)
            end if
        end if
        if (present(option_name)) then
            call definition%action%apply_option(option_name, definition%dest, &
                value, context%namespace, action_outcome, context%errors)
        else
            call definition%action%apply(definition%dest, value, &
                context%namespace, action_outcome, context%errors)
        end if
        if (context%errors%has_fatal_errors()) return
        if (mark_explicit) call context%mark_seen(index)
    end subroutine execute_action

    !> Registered spelling of an attached-value option token: the text before
    !> the attached value, without a separating `=`.
    function attached_spelling(token, attached) result(name)
        character(len=*), intent(in) :: token, attached
        character(len=:), allocatable :: name

        name = token(1:len(token) - len(attached))
        if (len(name) > 0) then
            if (name(len(name):len(name)) == "=") name = name(1:len(name) - 1)
        end if
    end function attached_spelling

    subroutine materialize_empty_positionals(arguments, context, action_outcome)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(inout) :: context
        integer, intent(inout) :: action_outcome
        type(ValueBox) :: value
        integer :: index, nargs_value

        do index = 1, size(arguments)
            if (.not. arguments(index)%is_positional()) cycle
            if (context%was_completed(index)) cycle
            nargs_value = arguments(index)%nargs_value()
            if ((nargs_value == NARGS_ZERO_OR_MORE .or. &
                nargs_value == NARGS_REMAINDER) .and. &
                .not. arguments(index)%has_default) then
                call make_empty_list(arguments(index)%data_type, value)
                call execute_action(arguments(index), index, value, context, &
                    action_outcome, mark_explicit=.false.)
                if (context%errors%has_fatal_errors()) return
                if (action_outcome /= ACTION_CONTINUE) return
            end if
            if (nargs_value == NARGS_OPTIONAL .or. &
                nargs_value == NARGS_ZERO_OR_MORE .or. &
                nargs_value == NARGS_REMAINDER) then
                call context%mark_completed(index)
            end if
        end do
    end subroutine materialize_empty_positionals

    subroutine make_empty_list(type_name, value)
        character(len=*), intent(in) :: type_name
        type(ValueBox), intent(out) :: value
        type(ListValue) :: list

        call list%initialize(type_name)
        call value%set(list)
    end subroutine make_empty_list

    subroutine record_lifecycle(definition, token, token_index, context, &
        continue_parsing)
        type(Argument), intent(in) :: definition
        character(len=*), intent(in) :: token
        integer, intent(in) :: token_index
        type(ParseContext), intent(inout) :: context
        logical, intent(out) :: continue_parsing

        continue_parsing = .true.
        if (allocated(definition%removed_msg)) then
            call context%errors%add(definition%removed_msg, &
                code=ERR_REMOVED_ARGUMENT, arg_name=definition%dest, &
                token=token, token_index=token_index)
            continue_parsing = .false.
            return
        end if
        if (allocated(definition%deprecated_msg)) then
            call context%errors%add(definition%deprecated_msg, &
                code=ERR_DEPRECATED_ARGUMENT, severity=ERROR_WARNING, &
                arg_name=definition%dest, token=token, token_index=token_index)
        end if
    end subroutine record_lifecycle

    subroutine check_required(arguments, context)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(inout) :: context
        integer :: index

        do index = 1, size(arguments)
            if (.not. arguments(index)%required) cycle
            if (context%was_seen(index)) cycle
            call context%errors%add("required argument was not supplied", &
                code=ERR_MISSING_REQUIRED, arg_name=arguments(index)%dest)
        end do
    end subroutine check_required

    subroutine validate_groups(arguments, groups, context)
        type(Argument), intent(in) :: arguments(:)
        type(GroupBox), intent(in) :: groups(:)
        type(ParseContext), intent(inout) :: context
        character(len=:), allocatable :: labels(:)
        integer :: group_index

        if (size(groups) == 0) return
        call argument_labels(arguments, labels)
        do group_index = 1, size(groups)
            if (.not. allocated(groups(group_index)%item)) cycle
            call groups(group_index)%item%validate_presence( &
                context%seen, labels, group_index, context%errors)
        end do
    end subroutine validate_groups

    subroutine validate_rules(arguments, rules, context)
        type(Argument), intent(in) :: arguments(:)
        type(ArgumentRule), intent(in) :: rules(:)
        type(ParseContext), intent(inout) :: context
        integer :: rule_index

        do rule_index = 1, size(rules)
            call rules(rule_index)%validate_presence(arguments, context%seen, &
                context%errors)
        end do
    end subroutine validate_rules

    integer function find_option(arguments, token) result(index)
        type(Argument), intent(in) :: arguments(:)
        character(len=*), intent(in) :: token
        integer :: current

        index = 0
        do current = 1, size(arguments)
            if (.not. arguments(current)%is_optional) cycle
            if (arguments(current)%matches_name(token)) then
                index = current
                return
            end if
        end do
    end function find_option

    !> Collect the user-facing spelling of every argument for diagnostics.
    subroutine argument_labels(arguments, labels)
        type(Argument), intent(in) :: arguments(:)
        character(len=:), allocatable, intent(out) :: labels(:)
        integer :: index, width

        width = 1
        do index = 1, size(arguments)
            width = max(width, len(arguments(index)%display_label()))
        end do
        allocate(character(len=width) :: labels(size(arguments)))
        do index = 1, size(arguments)
            labels(index) = arguments(index)%display_label()
        end do
    end subroutine argument_labels

    !> Report whether a token names an option in any supported spelling.
    logical function names_option(arguments, token) result(found)
        type(Argument), intent(in) :: arguments(:)
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: attached
        integer :: argument_index, form

        call resolve_option(arguments, token, argument_index, form, attached)
        found = form /= OPTION_NONE
    end function names_option

    integer function next_positional(arguments, context) result(index)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(inout) :: context

        index = max(1, context%positional_cursor)
        do while (index <= size(arguments))
            if (arguments(index)%is_positional() .and. &
                .not. context%was_completed(index)) then
                context%positional_cursor = index
                return
            end if
            index = index + 1
        end do
        context%positional_cursor = size(arguments) + 1
        index = 0
    end function next_positional

    integer function later_positional_minimum(arguments, index) result(minimum)
        type(Argument), intent(in) :: arguments(:)
        integer, intent(in) :: index
        integer :: current

        minimum = 0
        do current = index + 1, size(arguments)
            if (.not. arguments(current)%is_positional()) cycle
            minimum = minimum + arguments(current)%nargs%min_count()
        end do
    end function later_positional_minimum

    logical function token_is_numeric_value(definition, token) result(is_value)
        type(Argument), intent(in) :: definition
        character(len=*), intent(in) :: token
        type(ValueBox) :: value
        integer :: stat

        is_value = .false.
        if (definition%data_type /= "integer" .and. &
            definition%data_type /= "real") return
        call convert_value(token, definition%data_type, value, stat)
        is_value = stat == CONVERT_SUCCESS
    end function token_is_numeric_value

    !> A lone `-` is an ordinary value, conventionally standard input/output.
    pure logical function token_starts_option(token) result(starts_option)
        character(len=*), intent(in) :: token

        starts_option = len_trim(token) > 1
        if (starts_option) starts_option = token(1:1) == '-'
    end function token_starts_option

    subroutine advance_values(context, count)
        type(ParseContext), intent(inout) :: context
        integer, intent(in) :: count
        integer :: current

        do current = 1, count
            call context%advance()
        end do
    end subroutine advance_values

    subroutine add_unknown_at(context, token_index)
        type(ParseContext), intent(inout) :: context
        integer, intent(in) :: token_index
        character(len=:), allocatable :: token

        token = trim(context%tokens(token_index))
        call add_unknown_option(context, token, token_index)
    end subroutine add_unknown_at

    subroutine add_unknown_option(context, token, token_index)
        type(ParseContext), intent(inout) :: context
        character(len=*), intent(in) :: token
        integer, intent(in) :: token_index

        call context%errors%add("unknown option", code=ERR_UNKNOWN_ARGUMENT, &
            arg_name=token, token=token, token_index=token_index, &
            suggestion=suggest_option(context, token))
    end subroutine add_unknown_option

    !> Record the spellings of every option a user may be told about.
    !>
    !> Hidden and removed options are left out, so a suggestion never points
    !> at an option the help does not document or the parser rejects.
    subroutine collect_option_names(arguments, context)
        type(Argument), intent(in) :: arguments(:)
        type(ParseContext), intent(inout) :: context
        integer :: index, name, count, width

        count = 0
        width = 1
        do index = 1, size(arguments)
            if (.not. is_suggestable(arguments(index))) cycle
            count = count + arguments(index)%name_count()
            width = max(width, len(arguments(index)%names))
        end do
        allocate(character(len=width) :: context%option_names(count))
        count = 0
        do index = 1, size(arguments)
            if (.not. is_suggestable(arguments(index))) cycle
            do name = 1, arguments(index)%name_count()
                count = count + 1
                context%option_names(count) = arguments(index)%names(name)
            end do
        end do
    end subroutine collect_option_names

    logical function is_suggestable(definition) result(suggestable)
        type(Argument), intent(in) :: definition

        suggestable = definition%is_optional .and. definition%visible .and. &
            .not. allocated(definition%removed_msg)
    end function is_suggestable

    !> Closest known option to an unknown option token, or an empty string.
    !>
    !> Leading hyphens are compared separately from the name: they would
    !> otherwise make every pair of short names look alike, e.g. `--a` and
    !> `--b`. Names of a single character are never suggested.
    function suggest_option(context, token) result(suggestion)
        type(ParseContext), intent(in) :: context
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: suggestion
        character(len=:), allocatable :: typed, bare(:)
        integer :: current, separator, match

        suggestion = ""
        if (.not. allocated(context%option_names)) return
        if (size(context%option_names) == 0) return
        typed = trim(token)
        separator = index(typed, "=")
        if (separator > 0) typed = typed(:separator - 1)
        typed = strip_hyphens(typed)
        if (len(typed) < 2) return

        allocate(character(len=len(context%option_names)) :: &
            bare(size(context%option_names)))
        do current = 1, size(context%option_names)
            bare(current) = strip_hyphens(trim(context%option_names(current)))
            if (len_trim(bare(current)) < 2) bare(current) = ""
        end do
        match = closest_match(typed, bare)
        if (match > 0) suggestion = trim(context%option_names(match))
    end function suggest_option

    !> Closest subcommand name to an unknown command token, or an empty string.
    function suggest_subcommand(context, token) result(suggestion)
        type(ParseContext), intent(in) :: context
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: suggestion
        integer :: match

        suggestion = ""
        if (.not. allocated(context%subcommand_names)) return
        match = closest_match(trim(token), context%subcommand_names)
        if (match > 0) suggestion = trim(context%subcommand_names(match))
    end function suggest_subcommand

    !> Closest declared string choice to a rejected value, or an empty string.
    function suggest_choice(definition, token) result(suggestion)
        type(Argument), intent(in) :: definition
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: suggestion
        character(len=:), allocatable :: names(:)
        integer :: index, count, width, match

        suggestion = ""
        if (.not. allocated(definition%choices%item)) return
        select type (allowed => definition%choices%item)
        type is (ListValue)
            count = allowed%size()
            width = 1
            do index = 1, count
                select type (choice => allowed%items(index)%item)
                type is (StringValue)
                    width = max(width, len(choice%value))
                class default
                    return
                end select
            end do
            allocate(character(len=width) :: names(count))
            do index = 1, count
                select type (choice => allowed%items(index)%item)
                type is (StringValue)
                    names(index) = choice%value
                end select
            end do
        class default
            return
        end select

        if (definition%case_insensitive_choices) then
            match = closest_match(lowercase(trim(token)), lowercase(names))
        else
            match = closest_match(trim(token), names)
        end if
        if (match > 0) suggestion = trim(names(match))
    end function suggest_choice

    pure function strip_hyphens(name) result(bare)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: bare
        integer :: first

        first = verify(name, "-")
        if (first == 0) then
            bare = ""
        else
            bare = name(first:)
        end if
    end function strip_hyphens

    subroutine add_missing_value(context, definition, token, token_index)
        type(ParseContext), intent(inout) :: context
        type(Argument), intent(in) :: definition
        character(len=*), intent(in) :: token
        integer, intent(in) :: token_index

        call context%errors%add("option requires more values", &
            code=ERR_MISSING_VALUE, arg_name=definition%dest, token=token, &
            token_index=token_index)
    end subroutine add_missing_value

    subroutine add_missing_positional_value(context, definition)
        type(ParseContext), intent(inout) :: context
        type(Argument), intent(in) :: definition
        character(len=:), allocatable :: token

        if (context%at_end()) then
            call context%errors%add("positional argument requires more values", &
                code=ERR_MISSING_VALUE, arg_name=definition%dest)
        else
            token = context%current_token()
            call context%errors%add("positional argument requires more values", &
                code=ERR_MISSING_VALUE, arg_name=definition%dest, token=token, &
                token_index=context%cursor)
        end if
    end subroutine add_missing_positional_value

    subroutine finish_result(parsed, context, outcome, next_token_index)
        type(ParseResult), intent(out) :: parsed
        type(ParseContext), intent(in) :: context
        integer, intent(in) :: outcome
        integer, intent(out), optional :: next_token_index

        parsed%namespace = context%namespace
        parsed%errors = context%errors
        parsed%outcome = outcome
        if (present(next_token_index)) next_token_index = context%cursor
        if (allocated(context%selected_command_path)) then
            parsed%selected_command_path = context%selected_command_path
        end if
    end subroutine finish_result

end module fclap_parse_engine
