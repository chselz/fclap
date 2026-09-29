!> Deterministic argparse-style help formatter.
module fclap_formatter_standard
    use fclap_formatter_abstract, only : FormatterType
    use fclap_formatter_model, only : HelpArgument, HelpGroup, HelpModel
    use fclap_nargs, only : NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, &
        NARGS_ONE_OR_MORE, NARGS_REMAINDER, NARGS_ZERO
    implicit none

    private
    public :: StandardFormatter

    type, extends(FormatterType) :: StandardFormatter
        logical :: show_defaults = .true.
        logical :: show_choices = .true.
        logical :: raw_description = .false.
        logical :: raw_help_text = .false.
        integer :: help_width = 80
    contains
        procedure :: format_help => standard_format_help
        procedure :: format_usage => standard_format_usage
    end type StandardFormatter

contains

    function standard_format_help(self, model) result(res)
        class(StandardFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: res
        character(len=:), allocatable :: block
        integer :: index, width

        width = effective_width(self)
        res = self%format_usage(model)

        if (allocated(model%description)) then
            if (len_trim(model%description) > 0) then
                if (self%raw_description) then
                    block = trim(model%description)
                else
                    block = wrap_text(trim(model%description), 0, 0, width)
                end if
                call append_block(res, block)
            end if
        end if

        block = format_section(self, model, .false., &
            "positional arguments:", width)
        call append_block(res, block)

        block = format_section(self, model, .true., "options:", width)
        call append_block(res, block)

        do index = 1, model_group_count(model)
            if (model%groups(index)%is_mutex) cycle
            block = format_help_group(self, model, model%groups(index), width)
            call append_block(res, block)
        end do

        block = format_subcommands(self, model, width)
        call append_block(res, block)

        if (allocated(model%epilog)) then
            if (len_trim(model%epilog) > 0) then
                if (self%raw_description) then
                    block = trim(model%epilog)
                else
                    block = wrap_text(trim(model%epilog), 0, 0, width)
                end if
                call append_block(res, block)
            end if
        end if
    end function standard_format_help

    function standard_format_usage(self, model) result(res)
        class(StandardFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: res
        character(len=:), allocatable :: body, term
        logical, allocatable :: emitted(:)
        integer :: group_index, index, member_index, width

        width = effective_width(self)
        body = ""
        if (allocated(model%usage)) then
            body = trim(model%usage)
        else
            if (allocated(model%prog)) body = trim(model%prog)
            allocate(emitted(model_argument_count(model)), source=.false.)
            do index = 1, model_argument_count(model)
                if (emitted(index)) cycle
                if (.not. model%arguments(index)%visible) then
                    emitted(index) = .true.
                    cycle
                end if
                group_index = mutex_group_for_argument(model, index)
                if (group_index > 0) then
                    term = mutex_usage_term(model, model%groups(group_index))
                    if (allocated(model%groups(group_index)%argument_indices)) then
                        do member_index = 1, &
                            size(model%groups(group_index)%argument_indices)
                            if (model%groups(group_index)%argument_indices( &
                                member_index) < 1) cycle
                            if (model%groups(group_index)%argument_indices( &
                                member_index) > size(emitted)) cycle
                            emitted(model%groups(group_index)%argument_indices( &
                                member_index)) = .true.
                        end do
                    end if
                else
                    term = usage_term(model%arguments(index))
                    emitted(index) = .true.
                end if
                if (len(term) == 0) cycle
                if (len(body) > 0) body = body // " "
                body = body // term
            end do
            term = subcommand_usage_term(model)
            if (len(term) > 0) then
                if (len(body) > 0) body = body // " "
                body = body // term
            end if
        end if

        res = wrap_text("usage: " // body, 0, len("usage: "), width)
    end function standard_format_usage

    function format_section(self, model, optional_section, heading, width) &
        result(section)
        class(StandardFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        logical, intent(in) :: optional_section
        character(len=*), intent(in) :: heading
        integer, intent(in) :: width
        character(len=:), allocatable :: section
        character(len=:), allocatable :: row
        integer :: index

        section = ""
        do index = 1, model_argument_count(model)
            if (.not. model%arguments(index)%visible) cycle
            if (model%arguments(index)%is_optional .neqv. optional_section) cycle
            if (argument_in_help_group(model, index)) cycle
            row = format_argument(self, model%arguments(index), width)
            if (len(section) == 0) section = heading
            section = section // new_line('a') // row
        end do
    end function format_section

    function format_help_group(self, model, group, width) result(section)
        class(StandardFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        type(HelpGroup), intent(in) :: group
        integer, intent(in) :: width
        character(len=:), allocatable :: section
        character(len=:), allocatable :: description, row
        integer :: argument_index, member_index
        logical :: has_rows

        section = ""
        if (allocated(group%title)) section = trim(group%title) // ":"
        if (len(section) == 0) section = "arguments:"

        if (allocated(group%description)) then
            if (len_trim(group%description) > 0) then
                if (self%raw_description) then
                    description = indent_lines(trim(group%description), 2)
                else
                    description = wrap_text(trim(group%description), 2, 2, width)
                end if
                section = section // new_line('a') // description
            end if
        end if

        has_rows = .false.
        if (.not. allocated(group%argument_indices)) return
        do member_index = 1, size(group%argument_indices)
            argument_index = group%argument_indices(member_index)
            if (argument_index < 1 .or. &
                argument_index > model_argument_count(model)) cycle
            if (.not. model%arguments(argument_index)%visible) cycle
            row = format_argument(self, model%arguments(argument_index), width)
            if (.not. has_rows .and. allocated(group%description)) then
                if (len_trim(group%description) > 0) then
                    section = section // new_line('a')
                end if
            end if
            section = section // new_line('a') // row
            has_rows = .true.
        end do
    end function format_help_group

    function format_subcommands(self, model, width) result(section)
        class(StandardFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        integer, intent(in) :: width
        character(len=:), allocatable :: section
        character(len=:), allocatable :: description, row, help
        integer :: help_column, index

        section = ""
        if (.not. allocated(model%commands)) return
        if (allocated(model%subcommand_title)) then
            section = trim(model%subcommand_title) // ":"
        else
            section = "commands:"
        end if

        if (allocated(model%subcommand_description)) then
            if (len_trim(model%subcommand_description) > 0) then
                if (self%raw_description) then
                    description = indent_lines( &
                        trim(model%subcommand_description), 2)
                else
                    description = wrap_text( &
                        trim(model%subcommand_description), 2, 2, width)
                end if
                section = section // new_line('a') // description
            end if
        end if

        help_column = min(24, max(12, width / 3))
        do index = 1, size(model%commands)
            if (.not. allocated(model%commands(index)%name)) cycle
            row = "  " // trim(model%commands(index)%name)
            help = ""
            if (allocated(model%commands(index)%help)) &
                help = trim(model%commands(index)%help)
            if (len(help) > 0) then
                if (len(row) >= help_column) then
                    row = row // new_line('a') // &
                        wrap_text(help, help_column, help_column, width)
                else
                    row = row // repeat(" ", help_column - len(row)) // &
                        wrap_text(help, 0, help_column, width)
                end if
            end if
            section = section // new_line('a') // row
        end do
    end function format_subcommands

    function format_argument(self, argument, width) result(row)
        class(StandardFormatter), intent(in) :: self
        type(HelpArgument), intent(in) :: argument
        integer, intent(in) :: width
        character(len=:), allocatable :: row
        character(len=:), allocatable :: label, body, wrapped
        integer :: help_column

        label = argument_label(argument)
        body = argument_help(self, argument)
        row = "  " // label
        if (len(body) == 0) return

        help_column = min(24, max(12, width / 3))
        if (len(row) >= help_column) then
            if (self%raw_help_text) then
                wrapped = indent_lines(body, help_column)
            else
                wrapped = wrap_text(body, help_column, help_column, width)
            end if
            row = row // new_line('a') // wrapped
        else
            if (self%raw_help_text) then
                wrapped = indent_continuations(body, help_column)
            else
                wrapped = wrap_text(body, 0, help_column, width)
            end if
            row = row // repeat(" ", help_column - len(row)) // wrapped
        end if
    end function format_argument

    function argument_help(self, argument) result(text)
        class(StandardFormatter), intent(in) :: self
        type(HelpArgument), intent(in) :: argument
        character(len=:), allocatable :: text

        text = ""
        if (allocated(argument%help)) text = trim(argument%help)
        if (self%show_defaults .and. argument%has_default .and. &
            argument%print_default) then
            call append_note(text, &
                "default: " // allocated_text(argument%default_text))
        end if
        if (self%show_choices .and. argument%has_choices .and. &
            argument%print_choices) then
            call append_note(text, &
                "choices: " // allocated_text(argument%choices_text))
        end if
        if (allocated(argument%deprecated_msg)) then
            call append_note(text, &
                "deprecated: " // trim(argument%deprecated_msg))
        end if
        if (allocated(argument%removed_msg)) then
            call append_note(text, "removed: " // trim(argument%removed_msg))
        end if
    end function argument_help

    function argument_label(argument) result(label)
        type(HelpArgument), intent(in) :: argument
        character(len=:), allocatable :: label
        character(len=:), allocatable :: suffix
        integer :: index

        if (.not. argument%is_optional) then
            label = argument%metavar
            return
        end if

        suffix = value_pattern(argument%metavar, argument%nargs)
        label = ""
        if (.not. allocated(argument%names)) return
        do index = 1, size(argument%names)
            if (index > 1) label = label // ", "
            label = label // trim(argument%names(index))
            if (len(suffix) > 0) label = label // " " // suffix
        end do
    end function argument_label

    function usage_term(argument) result(term)
        type(HelpArgument), intent(in) :: argument
        character(len=:), allocatable :: term
        character(len=:), allocatable :: suffix

        suffix = value_pattern(argument%metavar, argument%nargs)
        if (argument%is_optional) then
            if (.not. allocated(argument%names)) then
                term = ""
                return
            end if
            term = trim(argument%names(1))
            if (len(suffix) > 0) term = term // " " // suffix
            if (.not. argument%required) term = "[" // term // "]"
        else
            term = suffix
        end if
    end function usage_term

    function bare_usage_term(argument) result(term)
        type(HelpArgument), intent(in) :: argument
        character(len=:), allocatable :: term
        character(len=:), allocatable :: suffix

        suffix = value_pattern(argument%metavar, argument%nargs)
        if (argument%is_optional) then
            if (.not. allocated(argument%names)) then
                term = ""
                return
            end if
            term = trim(argument%names(1))
            if (len(suffix) > 0) term = term // " " // suffix
        else
            term = suffix
        end if
    end function bare_usage_term

    function mutex_usage_term(model, group) result(term)
        type(HelpModel), intent(in) :: model
        type(HelpGroup), intent(in) :: group
        character(len=:), allocatable :: term
        character(len=:), allocatable :: member_term
        integer :: argument_index, member_index

        term = ""
        if (.not. allocated(group%argument_indices)) return
        do member_index = 1, size(group%argument_indices)
            argument_index = group%argument_indices(member_index)
            if (argument_index < 1 .or. &
                argument_index > model_argument_count(model)) cycle
            if (.not. model%arguments(argument_index)%visible) cycle
            member_term = bare_usage_term(model%arguments(argument_index))
            if (len(member_term) == 0) cycle
            if (len(term) > 0) term = term // " | "
            term = term // member_term
        end do
        if (len(term) == 0) return
        if (group%required) then
            term = "(" // term // ")"
        else
            term = "[" // term // "]"
        end if
    end function mutex_usage_term

    function subcommand_usage_term(model) result(term)
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: term
        integer :: index

        term = ""
        if (.not. allocated(model%commands)) return
        do index = 1, size(model%commands)
            if (.not. allocated(model%commands(index)%name)) cycle
            if (len(term) > 0) term = term // ","
            term = term // trim(model%commands(index)%name)
        end do
        if (len(term) == 0) return
        term = "{" // term // "}"
        if (.not. model%subcommand_required) term = "[" // term // "]"
    end function subcommand_usage_term

    integer function mutex_group_for_argument(model, argument_index) &
        result(group_index)
        type(HelpModel), intent(in) :: model
        integer, intent(in) :: argument_index
        integer :: index

        group_index = 0
        do index = 1, model_group_count(model)
            if (.not. model%groups(index)%is_mutex) cycle
            if (.not. allocated(model%groups(index)%argument_indices)) cycle
            if (any(model%groups(index)%argument_indices == argument_index)) then
                group_index = index
                return
            end if
        end do
    end function mutex_group_for_argument

    logical function argument_in_help_group(model, argument_index) result(found)
        type(HelpModel), intent(in) :: model
        integer, intent(in) :: argument_index
        integer :: index

        found = .false.
        do index = 1, model_group_count(model)
            if (model%groups(index)%is_mutex) cycle
            if (.not. allocated(model%groups(index)%argument_indices)) cycle
            if (any(model%groups(index)%argument_indices == argument_index)) then
                found = .true.
                return
            end if
        end do
    end function argument_in_help_group

    function value_pattern(metavar, nargs) result(pattern)
        character(len=*), intent(in) :: metavar
        integer, intent(in) :: nargs
        character(len=:), allocatable :: pattern

        select case (nargs)
        case (NARGS_ZERO)
            pattern = ""
        case (NARGS_OPTIONAL)
            pattern = "[" // trim(metavar) // "]"
        case (NARGS_ZERO_OR_MORE, NARGS_REMAINDER)
            pattern = "[" // trim(metavar) // " ...]"
        case (NARGS_ONE_OR_MORE)
            pattern = trim(metavar) // " [" // trim(metavar) // " ...]"
        case (1:)
            pattern = repeated_metavar(trim(metavar), nargs)
        case default
            pattern = trim(metavar)
        end select
    end function value_pattern

    function repeated_metavar(metavar, count) result(text)
        character(len=*), intent(in) :: metavar
        integer, intent(in) :: count
        character(len=:), allocatable :: text
        integer :: index

        text = ""
        do index = 1, count
            if (index > 1) text = text // " "
            text = text // metavar
        end do
    end function repeated_metavar

    function wrap_text(input, first_indent, continuation_indent, width) &
        result(output)
        character(len=*), intent(in) :: input
        integer, intent(in) :: first_indent, continuation_indent, width
        character(len=:), allocatable :: output
        character(len=:), allocatable :: word
        integer :: first, last, input_length, line_length, required

        output = repeat(" ", max(0, first_indent))
        line_length = max(0, first_indent)
        input_length = len_trim(input)
        first = 1
        do while (first <= input_length)
            do while (first <= input_length)
                if (input(first:first) /= " " .and. &
                    input(first:first) /= new_line('a') .and. &
                    input(first:first) /= achar(9)) exit
                first = first + 1
            end do
            if (first > input_length) exit

            last = first
            do while (last <= input_length)
                if (input(last:last) == " " .or. &
                    input(last:last) == new_line('a') .or. &
                    input(last:last) == achar(9)) exit
                last = last + 1
            end do
            word = input(first:last - 1)
            required = len(word)
            if (line_length > first_indent .or. &
                (line_length > 0 .and. first_indent == 0)) then
                required = required + 1
            end if

            if (line_length + required > width .and. &
                line_length > first_indent) then
                output = output // new_line('a') // &
                    repeat(" ", max(0, continuation_indent)) // word
                line_length = max(0, continuation_indent) + len(word)
            else
                if (line_length > first_indent .or. &
                    (line_length > 0 .and. first_indent == 0)) then
                    output = output // " "
                    line_length = line_length + 1
                end if
                output = output // word
                line_length = line_length + len(word)
            end if
            first = last + 1
        end do
    end function wrap_text

    function indent_lines(input, indentation) result(output)
        character(len=*), intent(in) :: input
        integer, intent(in) :: indentation
        character(len=:), allocatable :: output

        output = repeat(" ", max(0, indentation)) // &
            indent_continuations(input, indentation)
    end function indent_lines

    function indent_continuations(input, indentation) result(output)
        character(len=*), intent(in) :: input
        integer, intent(in) :: indentation
        character(len=:), allocatable :: output
        integer :: index

        output = ""
        do index = 1, len_trim(input)
            output = output // input(index:index)
            if (input(index:index) == new_line('a') .and. &
                index < len_trim(input)) then
                output = output // repeat(" ", max(0, indentation))
            end if
        end do
    end function indent_continuations

    subroutine append_note(text, note)
        character(len=:), allocatable, intent(inout) :: text
        character(len=*), intent(in) :: note

        if (len(text) > 0) text = text // " "
        text = text // "(" // trim(note) // ")"
    end subroutine append_note

    subroutine append_block(text, block)
        character(len=:), allocatable, intent(inout) :: text
        character(len=*), intent(in) :: block

        if (len(block) == 0) return
        if (len(text) > 0) text = text // new_line('a') // new_line('a')
        text = text // block
    end subroutine append_block

    pure integer function model_argument_count(model) result(number)
        type(HelpModel), intent(in) :: model

        if (allocated(model%arguments)) then
            number = size(model%arguments)
        else
            number = 0
        end if
    end function model_argument_count

    pure integer function model_group_count(model) result(number)
        type(HelpModel), intent(in) :: model

        if (allocated(model%groups)) then
            number = size(model%groups)
        else
            number = 0
        end if
    end function model_group_count

    pure integer function effective_width(self) result(width)
        class(StandardFormatter), intent(in) :: self

        width = self%help_width
        if (width < 20) width = 80
    end function effective_width

    function allocated_text(text) result(value)
        character(len=:), allocatable, intent(in) :: text
        character(len=:), allocatable :: value

        if (allocated(text)) then
            value = text
        else
            value = ""
        end if
    end function allocated_text

end module fclap_formatter_standard
