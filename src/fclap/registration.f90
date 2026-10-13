!> Validation and normalization of one argument definition.
!>
!> The parser facade owns its state; this module turns the keyword arguments
!> of `add_argument` into a ready `Argument`, or into one configuration error,
!> without modifying anything. Checks run in a fixed order so a definition
!> with several faults always reports the same first one.
module fclap_registration
    use fclap_actions_abstract, only : ActionType
    use fclap_actions_builtin, only : StoreAction, StoreConstAction
    use fclap_argument, only : Argument, derive_dest_from_names
    use fclap_error_codes, only : FCLAP_OK, ERR_INVALID_ARGUMENT_NAME, &
        ERR_DUPLICATE_OPTION, ERR_DUPLICATE_DEST, ERR_INVALID_NARGS, &
        ERR_INCOMPATIBLE_ACTION, ERR_INVALID_DEFAULT, ERR_INVALID_TYPE_NAME, &
        ERR_INVALID_LIFECYCLE, ERR_INCOMPATIBLE_VALIDATOR
    use fclap_nargs, only : NargsSpec, normalize_nargs, NARGS_SUCCESS, &
        NARGS_OPTIONAL, NARGS_REMAINDER
    use fclap_validators_abstract, only : ValidatorType
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : ListValue
    use fclap_value_convert, only : CONVERT_SUCCESS, convert_value, &
        normalize_type_name, value_in_choices, value_is_list, &
        value_matches_type, canonicalize_choice, has_ambiguous_choices
    implicit none
    private

    public :: check_names
    public :: build_argument
    public :: any_registered_name

contains

    !> Collect, expand, and validate the names of a new definition.
    !>
    !> The action may add spellings (for example `--no-name`); every name is
    !> then checked for syntax, repetition, and collisions with `existing`.
    subroutine check_names(existing, action, name1, name2, name3, name4, &
        names, given, optional_names, code, message)
        type(Argument), intent(in) :: existing(:)
        class(ActionType), intent(in), optional :: action
        character(len=*), intent(in) :: name1
        character(len=*), intent(in), optional :: name2, name3, name4
        !> Every accepted spelling, including those the action added
        character(len=:), allocatable, intent(out) :: names(:)
        !> The spellings as written, used to derive the destination
        character(len=:), allocatable, intent(out) :: given(:)
        logical, intent(out) :: optional_names
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: message

        code = FCLAP_OK
        message = ""
        call collect_names(name1, name2, name3, name4, given)
        if (present(action)) then
            call action%option_names(given, names)
        else
            names = given
        end if

        if (.not. validate_names(names, optional_names, message)) then
            code = ERR_INVALID_ARGUMENT_NAME
        else if (has_duplicate_name(names)) then
            code = ERR_DUPLICATE_OPTION
            message = "an alias is repeated in the same definition"
        else if (any_registered_name(existing, names)) then
            code = ERR_DUPLICATE_OPTION
            message = "an option or positional name is already registered"
        end if
    end subroutine check_names

    !> Normalize and validate everything after the names.
    !>
    !> On success `code` is `FCLAP_OK` and `candidate` is ready to commit. On
    !> failure `subject` names the argument for the configuration error.
    subroutine build_argument(existing, names, given, optional_names, reserved_dest, &
        subparsers_enabled, action, nargs, data_type, default, const, choices, &
        validator, required, help, metavar, dest, visible, deprecated_msg, &
        removed_msg, print_default, print_choices, case_insensitive_choices, &
        candidate, code, message, subject)
        type(Argument), intent(in) :: existing(:)
        character(len=*), intent(in) :: names(:), given(:)
        logical, intent(in) :: optional_names
        character(len=*), intent(in), optional :: reserved_dest
        logical, intent(in) :: subparsers_enabled
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
        logical, intent(in), optional :: case_insensitive_choices
        type(Argument), intent(out) :: candidate
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: message, subject

        character(len=:), allocatable :: canonical_type, action_type
        character(len=:), allocatable :: candidate_dest, detail
        class(ActionType), allocatable :: selected_action
        type(NargsSpec) :: nargs_spec
        type(ValueBox) :: default_value, const_value, choice_values
        logical :: required_value, has_default, has_const, implicit_default
        logical :: has_choices, valid, ignore_case
        integer :: stat

        code = FCLAP_OK
        message = ""
        subject = trim(names(1))

        if (present(dest)) then
            candidate_dest = trim(dest)
        else
            candidate_dest = derive_dest_from_names(given)
        end if
        if (len(candidate_dest) == 0) then
            call fail(ERR_INVALID_ARGUMENT_NAME, &
                "the destination must not be empty")
            return
        end if
        subject = candidate_dest
        if (dest_is_taken(existing, reserved_dest, candidate_dest)) then
            call fail(ERR_DUPLICATE_DEST, "the destination is already registered")
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
            call fail(ERR_INVALID_NARGS, "invalid nargs value")
            return
        end if
        if (.not. selected_action%accepts_nargs(nargs_spec)) then
            call fail(ERR_INCOMPATIBLE_ACTION, &
                "the action does not accept this nargs value")
            return
        end if
        if (optional_names .and. nargs_spec%value() == NARGS_REMAINDER) then
            call fail(ERR_INVALID_NARGS, &
                "remainder nargs is only valid for a positional argument")
            return
        end if
        if (subparsers_enabled .and. .not. optional_names .and. &
            nargs_spec%value() == NARGS_REMAINDER) then
            call fail(ERR_INVALID_NARGS, &
                "remainder nargs cannot precede a subparser collection")
            return
        end if
        if (.not. positional_layout_is_valid(existing, optional_names, &
            nargs_spec)) then
            call fail(ERR_INVALID_NARGS, &
                "ambiguous or misplaced variable positional argument")
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
                call fail(ERR_INVALID_TYPE_NAME, "unknown argument data type")
            else
                call fail(ERR_INCOMPATIBLE_ACTION, &
                    "the action does not provide a supported value type")
            end if
            return
        end if
        if (len(action_type) > 0 .and. action_type /= canonical_type) then
            call fail(ERR_INCOMPATIBLE_ACTION, &
                "the action requires data type " // action_type)
            return
        end if

        if (present(deprecated_msg) .and. present(removed_msg)) then
            call fail(ERR_INVALID_LIFECYCLE, &
                "deprecated and removed messages are mutually exclusive")
            return
        end if

        if (optional_names) then
            required_value = .false.
            if (present(required)) required_value = required
        else
            required_value = nargs_spec%min_count() >= 1
            if (present(required)) then
                if (required .and. nargs_spec%min_count() == 0) then
                    call fail(ERR_INCOMPATIBLE_ACTION, &
                        "a zero-minimum positional cannot be required")
                    return
                end if
            end if
        end if

        has_choices = present(choices)
        if (has_choices) then
            if (nargs_spec%max_count() == 0) then
                call fail(ERR_INCOMPATIBLE_ACTION, &
                    "a zero-value action cannot define choices")
                return
            end if
            call convert_value(choices, canonical_type, choice_values, stat)
            if (stat /= CONVERT_SUCCESS) then
                call fail(ERR_INVALID_DEFAULT, &
                    "choices are incompatible with the declared type")
                return
            end if
        end if

        ignore_case = .false.
        if (present(case_insensitive_choices)) &
            ignore_case = case_insensitive_choices
        if (ignore_case) then
            if (.not. has_choices .or. canonical_type /= "string") then
                call fail(ERR_INCOMPATIBLE_ACTION, &
                    "case-insensitive matching requires string choices")
                return
            end if
            if (has_ambiguous_choices(choice_values, ignore_case)) then
                call fail(ERR_INVALID_DEFAULT, &
                    "choices differ only by letter case")
                return
            end if
        end if
        if (present(validator)) then
            if (nargs_spec%max_count() == 0) then
                call fail(ERR_INCOMPATIBLE_ACTION, &
                    "a zero-value action cannot define a validator")
                return
            end if
            if (.not. validator%accepts_type(canonical_type)) then
                call fail(ERR_INCOMPATIBLE_VALIDATOR, &
                    "the validator cannot check values of type " // &
                    canonical_type)
                return
            end if
        end if

        has_default = present(default)
        implicit_default = .false.
        if (has_default) then
            call convert_value(default, canonical_type, default_value, stat)
            if (stat /= CONVERT_SUCCESS) then
                call fail(ERR_INVALID_DEFAULT, &
                    "default value is incompatible with the declared type")
                return
            end if
        else
            call selected_action%implicit_default(default_value, has_default)
            implicit_default = has_default
        end if
        if (has_default) then
            if (.not. value_matches_type(default_value, canonical_type)) then
                call fail(ERR_INVALID_DEFAULT, &
                    "default value has the wrong declared type")
                return
            end if
            if (value_is_list(default_value) .neqv. &
                selected_action%produces_list(nargs_spec)) then
                call fail(ERR_INVALID_DEFAULT, &
                    "default value has the wrong scalar/list shape")
                return
            end if
        end if

        has_const = .false.
        select type (concrete_action => selected_action)
        type is (StoreConstAction)
            const_value = concrete_action%constant%clone()
            has_const = .true.
            if (const_value%type_name() /= canonical_type) then
                call fail(ERR_INCOMPATIBLE_ACTION, &
                    "store_const value does not match the declared type")
                return
            end if
        end select

        if (present(const)) then
            if (.not. optional_names .or. &
                nargs_spec%value() /= NARGS_OPTIONAL) then
                call fail(ERR_INCOMPATIBLE_ACTION, &
                    "const is only valid for an option with nargs='?'")
                return
            end if
            call convert_value(const, canonical_type, const_value, stat)
            if (stat /= CONVERT_SUCCESS .or. value_is_list(const_value)) then
                call fail(ERR_INVALID_DEFAULT, &
                    "const value is incompatible with the declared type")
                return
            end if
            has_const = .true.
        else if (optional_names .and. nargs_spec%value() == NARGS_OPTIONAL) then
            call fail(ERR_INCOMPATIBLE_ACTION, "nargs='?' requires a const value")
            return
        end if

        if (has_default .and. has_choices) then
            if (.not. value_in_choices(default_value, choice_values, &
                ignore_case)) then
                call fail(ERR_INVALID_DEFAULT, &
                    "default value is not one of the allowed choices")
                return
            end if
            if (ignore_case) &
                call canonicalize_choice(default_value, choice_values, ignore_case)
        end if
        if (has_const .and. has_choices) then
            if (.not. value_in_choices(const_value, choice_values, &
                ignore_case)) then
                call fail(ERR_INVALID_DEFAULT, &
                    "const value is not one of the allowed choices")
                return
            end if
            if (ignore_case) &
                call canonicalize_choice(const_value, choice_values, ignore_case)
        end if
        if (present(validator)) then
            if (has_default) then
                call validate_value(validator, default_value, valid, detail)
                if (.not. valid) then
                    call fail(ERR_INVALID_DEFAULT, &
                        detailed("default value failed validation", detail))
                    return
                end if
            end if
            if (has_const) then
                call validate_value(validator, const_value, valid, detail)
                if (.not. valid) then
                    call fail(ERR_INVALID_DEFAULT, &
                        detailed("const value failed validation", detail))
                    return
                end if
            end if
        end if

        call candidate%init(names, nargs_spec, canonical_type, selected_action, &
            dest=candidate_dest, default_value=default_value, &
            const_value=const_value, choices=choice_values, &
            has_default=has_default, has_const=has_const, &
            has_choices=has_choices, validator=validator, &
            required=required_value, help=help, metavar=metavar, &
            visible=visible, deprecated_msg=deprecated_msg, &
            removed_msg=removed_msg, print_default=print_default, &
            print_choices=print_choices, case_insensitive_choices=ignore_case, &
            implicit_default=implicit_default)

    contains

        subroutine fail(error_code, error_message)
            integer, intent(in) :: error_code
            character(len=*), intent(in) :: error_message

            code = error_code
            message = error_message
        end subroutine fail

    end subroutine build_argument

    !> Report whether any name is already used by an existing definition.
    logical function any_registered_name(existing, names) result(found)
        type(Argument), intent(in) :: existing(:)
        character(len=*), intent(in) :: names(:)
        integer :: argument_index, name_index

        found = .false.
        do argument_index = 1, size(existing)
            do name_index = 1, size(names)
                if (existing(argument_index)%matches_name(names(name_index))) then
                    found = .true.
                    return
                end if
            end do
        end do
    end function any_registered_name

    logical function dest_is_taken(existing, reserved_dest, dest) result(taken)
        type(Argument), intent(in) :: existing(:)
        character(len=*), intent(in), optional :: reserved_dest
        character(len=*), intent(in) :: dest
        integer :: index

        taken = .false.
        if (present(reserved_dest)) taken = reserved_dest == dest
        do index = 1, size(existing)
            if (taken) return
            taken = existing(index)%dest == dest
        end do
    end function dest_is_taken

    logical function positional_layout_is_valid(existing, optional_names, &
        nargs) result(valid)
        type(Argument), intent(in) :: existing(:)
        logical, intent(in) :: optional_names
        type(NargsSpec), intent(in) :: nargs
        integer :: index

        valid = .true.
        if (optional_names) return
        do index = 1, size(existing)
            if (.not. existing(index)%is_positional()) cycle
            if (existing(index)%nargs_value() == NARGS_REMAINDER) then
                valid = .false.
                return
            end if
            if (nargs%is_variable() .and. existing(index)%nargs%is_variable()) then
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

    function detailed(summary, detail) result(message)
        character(len=*), intent(in) :: summary, detail
        character(len=:), allocatable :: message

        message = summary
        if (len_trim(detail) > 0) message = message // ": " // trim(detail)
    end function detailed

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

        allocate(character(len=max(1, max_length)) :: names(number))
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

end module fclap_registration
