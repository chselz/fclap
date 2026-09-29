!> Side-effect-free token parser for one flat argument definition array.
module fclap_parse_engine
    use fclap_actions_abstract, only : ACTION_CONTINUE, &
        ACTION_HELP_REQUESTED, ACTION_VERSION_REQUESTED
    use fclap_argument, only : Argument
    use fclap_error_codes, only : ERROR_WARNING, ERR_DEPRECATED_ARGUMENT, &
        ERR_EXTRA_POSITIONAL, ERR_INVALID_CHOICE, ERR_INVALID_VALUE, &
        ERR_MISSING_REQUIRED, ERR_MISSING_VALUE, ERR_REMOVED_ARGUMENT, &
        ERR_UNKNOWN_ARGUMENT, ERR_VALIDATION_FAILED, ERR_UNKNOWN_SUBCOMMAND, &
        ERR_MISSING_SUBCOMMAND
    use fclap_error_stack, only : ErrorStack
    use fclap_groups_abstract, only : GroupBox
    use fclap_nargs, only : NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, &
        NARGS_ONE_OR_MORE, NARGS_REMAINDER, NARGS_ZERO
    use fclap_parse_context, only : ParseContext
    use fclap_parse_result, only : ParseResult, PARSE_SUCCESS, PARSE_FAILURE, &
        PARSE_HELP, PARSE_VERSION
    use fclap_validators_choices, only : ChoicesValidator, choices_validator
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : ListValue
    use fclap_value_convert, only : CONVERT_SUCCESS, convert_value
    implicit none
    private

    integer, parameter :: BOUNDARY_END = 0
    integer, parameter :: BOUNDARY_MARKER = 1
    integer, parameter :: BOUNDARY_KNOWN_OPTION = 2
    integer, parameter :: BOUNDARY_UNKNOWN_OPTION = 3
    integer, parameter :: BOUNDARY_LIMIT = 4
    integer, parameter :: BOUNDARY_SUBCOMMAND = 5

    public :: parse_definitions

contains

    function parse_definitions(arguments, config_errors, tokens, groups, &
        version_text, subcommand_names, has_subcommands, &
        subcommand_required, next_token_index) result(parsed)
        type(Argument), intent(in) :: arguments(:)
        type(ErrorStack), intent(in) :: config_errors
        character(len=*), intent(in) :: tokens(:)
        type(GroupBox), intent(in) :: groups(:)
        character(len=*), intent(in), optional :: version_text
        character(len=*), intent(in), optional :: subcommand_names(:)
        logical, intent(in), optional :: has_subcommands, subcommand_required
        integer, intent(out), optional :: next_token_index
        type(ParseResult) :: parsed
        type(ParseContext) :: context
        character(len=:), allocatable :: token
        integer :: argument_index, action_outcome

        call context%init(tokens, size(arguments))
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
            if (context%options_enabled) then
                argument_index = find_option(arguments, token)
            end if
            if (argument_index > 0) then
                call consume_option(arguments, argument_index, context, &
                    action_outcome)
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
                        token=token, token_index=context%cursor)
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
        call validate_groups(groups, context)
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
    end function parse_definitions

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

    subroutine consume_option(arguments, index, context, action_outcome)
        type(Argument), intent(in) :: arguments(:)
        integer, intent(in) :: index
        type(ParseContext), intent(inout) :: context
        integer, intent(out) :: action_outcome
        type(ValueBox) :: value
        character(len=:), allocatable :: option_token
        integer :: option_token_index, available, boundary, count
        integer :: minimum, maximum
        logical :: continue_parsing

        action_outcome = ACTION_CONTINUE
        option_token = context%current_token()
        option_token_index = context%cursor
        call record_lifecycle(arguments(index), option_token, option_token_index, &
            context, continue_parsing)
        if (.not. continue_parsing) return
        call context%advance()

        if (arguments(index)%nargs_value() == NARGS_ZERO) then
            call execute_action(arguments(index), index, value, context, &
                action_outcome, mark_explicit=.true.)
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
            action_outcome, mark_explicit=.true.)
        if (context%errors%has_fatal_errors()) return
        call advance_values(context, count)
    end subroutine consume_option

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
                if (find_option(arguments, token) > 0) then
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
        type(ChoicesValidator) :: choice_check
        character(len=:), allocatable :: token, message
        integer :: stat
        logical :: valid

        token = trim(context%tokens(token_index))
        call convert_value(token, definition%data_type, value, stat)
        if (stat /= CONVERT_SUCCESS) then
            call context%errors%add("argument value could not be converted", &
                code=ERR_INVALID_VALUE, arg_name=definition%dest, token=token, &
                token_index=token_index)
            return
        end if

        if (definition%has_choices) then
            choice_check = choices_validator(definition%choices)
            call choice_check%validate(value, valid, message)
            if (.not. valid) then
                call context%errors%add(message, code=ERR_INVALID_CHOICE, &
                    arg_name=definition%dest, token=token, token_index=token_index)
                return
            end if
        end if

        if (allocated(definition%validator)) then
            call definition%validator%validate(value, valid, message)
            if (.not. valid) then
                if (.not. allocated(message)) message = ""
                if (len_trim(message) == 0) message = &
                    "argument value failed validation"
                call context%errors%add(message, code=ERR_VALIDATION_FAILED, &
                    arg_name=definition%dest, token=token, token_index=token_index)
            end if
        end if
    end subroutine convert_and_validate_token

    subroutine execute_action(definition, index, value, context, action_outcome, &
        mark_explicit)
        type(Argument), intent(in) :: definition
        integer, intent(in) :: index
        type(ValueBox), intent(in) :: value
        type(ParseContext), intent(inout) :: context
        integer, intent(out) :: action_outcome
        logical, intent(in) :: mark_explicit

        action_outcome = ACTION_CONTINUE
        call definition%action%apply(definition%dest, value, context%namespace, &
            action_outcome, context%errors)
        if (context%errors%has_fatal_errors()) return
        if (mark_explicit) call context%mark_seen(index)
    end subroutine execute_action

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

    subroutine validate_groups(groups, context)
        type(GroupBox), intent(in) :: groups(:)
        type(ParseContext), intent(inout) :: context
        integer :: group_index

        do group_index = 1, size(groups)
            if (.not. allocated(groups(group_index)%item)) cycle
            call groups(group_index)%item%validate_presence( &
                context%seen, group_index, context%errors)
        end do
    end subroutine validate_groups

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

    pure logical function token_starts_option(token) result(starts_option)
        character(len=*), intent(in) :: token

        starts_option = len_trim(token) > 0
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
            arg_name=token, token=token, token_index=token_index)
    end subroutine add_unknown_option

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
