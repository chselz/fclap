!> Deterministic argparse-style help formatter.
module fclap_formatter_standard
    use fclap_formatter_abstract, only : FormatterType
    use fclap_formatter_model, only : HelpArgument, HelpGroup, HelpModel
    use fclap_nargs, only : NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, &
        NARGS_ONE_OR_MORE, NARGS_REMAINDER, NARGS_ZERO
    implicit none

    private
    public :: StandardFormatter

    ! Joins the words of one usage item so wrapping never splits the item.
    character(len=*), parameter :: USAGE_JOINER = achar(31)

    type, extends(FormatterType) :: StandardFormatter
        logical :: show_defaults = .true.
        logical :: show_choices = .true.
        logical :: raw_description = .false.
        logical :: raw_help_text = .false.
        integer :: help_width = 80
        !> Take the width from the `COLUMNS` environment variable when it
        !> holds a usable width, falling back to `help_width`
        logical :: width_from_env = .false.
    contains
        procedure :: format_help => standard_format_help
        procedure :: format_usage => standard_format_usage
    end type StandardFormatter

contains

    !> format the help text appropriately
    !> important this is not a printing function only the formated text gets
    !> returned to the caller of this function.
    function standard_format_help(self, model) result(res)
        class(StandardFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: res
        character(len=:), allocatable :: section
        integer :: index, width

        ! get the width of the output
        width = effective_width(self)
        ! when the help page is printed also the information on the usage
        ! is printed before the explanation of the flags.
        res = self%format_usage(model)

        if (allocated(model%description)) then
            if (len_trim(model%description) > 0) then
                if (self%raw_description) then
                    section = trim(model%description)
                else
                    section = wrap_text(trim(model%description), 0, 0, width)
                end if
                call append_section(res, section)
            end if
        end if

        ! format the positional arguments
        section = format_section(self, model, .false., &
            "positional arguments:", width)
        call append_section(res, section)

        ! format the optional arguments
        section = format_section(self, model, .true., "options:", width)
        call append_section(res, section)

        ! format the argument groups
        do index = 1, model_group_count(model)
            if (model%groups(index)%is_mutex) cycle
            section = format_help_group(self, model, model%groups(index), width)
            call append_section(res, section)
        end do

        ! format the subcommands
        section = format_subcommands(self, model, width)
        call append_section(res, section)

        ! format the epilog
        if (allocated(model%epilog)) then
            if (len_trim(model%epilog) > 0) then
                if (self%raw_description) then
                    section = trim(model%epilog)
                else
                    section = wrap_text(trim(model%epilog), 0, 0, width)
                end if
                call append_section(res, section)
            end if
        end if
    end function standard_format_help

    !> format the usage text appropriately
    !> important this is not a printing function only the formated text gets
    !> returned to the caller of this function.
    function standard_format_usage(self, model) result(res)
        class(StandardFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: res
        character(len=:), allocatable :: body, term
        logical, allocatable :: emitted(:)
        integer :: group_index, index, member_index, width, pass

        width = effective_width(self)
        body = ""
        if (allocated(model%usage)) then
            body = trim(model%usage)
        else
            if (allocated(model%prog)) body = atomic_term(trim(model%prog))
            allocate(emitted(model_argument_count(model)), source=.false.)
            ! Options first, then positionals, as argparse lists them.
            do pass = 1, 2
            do index = 1, model_argument_count(model)
                if (emitted(index)) cycle
                if (.not. model%arguments(index)%visible) then
                    emitted(index) = .true.
                    cycle
                end if
                if (model%arguments(index)%is_optional .neqv. (pass == 1)) cycle
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
                body = body // atomic_term(term)
            end do
            end do
            term = subcommand_usage_term(model)
            if (len(term) > 0) then
                if (len(body) > 0) body = body // " "
                body = body // atomic_term(term)
            end if
        end if

        res = wrap_text("usage: " // body, 0, len("usage: "), width)
        res = replace_joiners(res)
    end function standard_format_usage

    !> Protect the blanks inside one usage item from line wrapping.
    pure function atomic_term(term) result(joined)
        character(len=*), intent(in) :: term
        character(len=len(term)) :: joined
        integer :: position

        joined = term
        do position = 1, len(joined)
            if (joined(position:position) == " ") then
                joined(position:position) = USAGE_JOINER
            end if
        end do
    end function atomic_term

    pure function replace_joiners(text) result(plain)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: plain
        integer :: position

        plain = text
        do position = 1, len(plain)
            if (plain(position:position) == USAGE_JOINER) then
                plain(position:position) = " "
            end if
        end do
    end function replace_joiners

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
            section = section // new_line("a") // row
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
                section = section // new_line("a") // description
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
                    section = section // new_line("a")
                end if
            end if
            section = section // new_line("a") // row
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
                section = section // new_line("a") // description
            end if
        end if

        help_column = min(24, max(12, width / 3))
        do index = 1, size(model%commands)
            if (.not. allocated(model%commands(index)%name)) cycle
            row = "  " // trim(model%commands(index)%name)
            if (allocated(model%commands(index)%aliases)) then
                if (size(model%commands(index)%aliases) > 0) then
                    row = row // " (" // join_trimmed( &
                        model%commands(index)%aliases, ", ") // ")"
                end if
            end if
            help = ""
            if (allocated(model%commands(index)%help)) then
                help = trim(model%commands(index)%help)
            end if
            if (len(help) > 0) then
                if (len(row) >= help_column) then
                    row = row // new_line("a") // &
                        wrap_text(help, help_column, help_column, width)
                else
                    row = row // repeat(" ", help_column - len(row)) // &
                        wrap_from_column(help, help_column, width)
                end if
            end if
            section = section // new_line("a") // row
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
        help_column = min(24, max(12, width / 3))
        if (len(body) == 0) then
            ! Choice lines go below the label even without help text.
            if (allocated(argument%choice_help)) row = row // new_line("a") // &
                choice_lines(argument, help_column + 2, width)
            return
        end if

        if (len(row) >= help_column) then
            if (self%raw_help_text) then
                wrapped = indent_lines(body, help_column)
            else
                wrapped = wrap_text(body, help_column, help_column, width)
            end if
            row = row // new_line("a") // wrapped
        else
            if (self%raw_help_text) then
                wrapped = indent_continuations(body, help_column)
            else
                wrapped = wrap_from_column(body, help_column, width)
            end if
            row = row // repeat(" ", help_column - len(row)) // wrapped
        end if
        if (allocated(argument%choice_help)) row = row // new_line("a") // &
            choice_lines(argument, help_column + 2, width)
    end function format_argument

    !> One line per described choice: the choice, then its aligned help.
    function choice_lines(argument, indent, width) result(lines)
        type(HelpArgument), intent(in) :: argument
        integer, intent(in) :: indent, width
        character(len=:), allocatable :: lines
        integer :: index, name_width

        lines = ""
        name_width = maxval(len_trim(argument%choice_names))
        do index = 1, size(argument%choice_names)
            if (index > 1) lines = lines // new_line("a")
            lines = lines // repeat(" ", indent) // &
                argument%choice_names(index)(:name_width) // "  " // &
                wrap_from_column(trim(argument%choice_help(index)), &
                indent + name_width + 2, width)
        end do
    end function choice_lines

    function argument_help(self, argument) result(text)
        class(StandardFormatter), intent(in) :: self
        type(HelpArgument), intent(in) :: argument
        character(len=:), allocatable :: text

        text = ""
        if (allocated(argument%help)) text = trim(argument%help)
        if (allocated(argument%env)) call append_note(text, "env: " // argument%env)
        ! Implicit defaults such as `.false.` for a flag carry no information.
        if (self%show_defaults .and. argument%has_default .and. &
            argument%print_default .and. .not. argument%implicit_default) then
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

        suffix = value_pattern(argument)
        label = ""
        if (.not. allocated(argument%names)) return
        do index = 1, size(argument%names)
            if (index > 1) label = label // ", "
            label = label // trim(argument%names(index))
            if (len(suffix) > 0) label = label // " " // suffix
        end do
    end function argument_label

    !> Usage item of one argument; optional options are bracketed.
    function usage_term(argument) result(term)
        type(HelpArgument), intent(in) :: argument
        character(len=:), allocatable :: term

        term = bare_usage_term(argument)
        if (len(term) == 0) return
        if (argument%is_optional .and. .not. argument%required) then
            term = "[" // term // "]"
        end if
    end function usage_term

    function bare_usage_term(argument) result(term)
        type(HelpArgument), intent(in) :: argument
        character(len=:), allocatable :: term
        character(len=:), allocatable :: suffix

        suffix = value_pattern(argument)
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

    !> Values of one argument as shown in usage and help, e.g. `N N N`.
    !>
    !> An explicit metavar that already contains the separator of a fixed
    !> count describes every value, e.g. `EX,EY,EZ` or `X Y Z`, and is shown
    !> once; any other metavar is repeated per value.
    function value_pattern(argument) result(pattern)
        type(HelpArgument), intent(in) :: argument
        character(len=:), allocatable :: pattern
        character(len=:), allocatable :: metavar, delimiter, separator
        integer :: nargs, position

        metavar = trim(argument%metavar)
        nargs = argument%nargs
        if (allocated(argument%delimiter)) then
            delimiter = argument%delimiter
            separator = delimiter
        else
            separator = " "
        end if
        if (argument%explicit_metavar .and. nargs > 1) then
            if (index(metavar, separator) > 0) then
                pattern = metavar
                return
            end if
        end if

        if (allocated(delimiter)) then
            ! One token holding the values, e.g. `N,N,N` or `N[,N...]`.
            select case (nargs)
            case (NARGS_ZERO_OR_MORE)
                pattern = "[" // trim(metavar) // "[" // delimiter // &
                    trim(metavar) // "...]]"
            case (NARGS_ONE_OR_MORE)
                pattern = trim(metavar) // "[" // delimiter // &
                    trim(metavar) // "...]"
            case default
                pattern = trim(metavar)
                do position = 2, nargs
                    pattern = pattern // delimiter // trim(metavar)
                end do
            end select
            return
        end if

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

    !> Wrap text that continues a line already filled up to `column`: the
    !> first line is limited like every other, and later lines are indented
    !> to `column`. The result omits the first line's indentation.
    function wrap_from_column(input, column, width) result(output)
        character(len=*), intent(in) :: input
        integer, intent(in) :: column, width
        character(len=:), allocatable :: output

        output = wrap_text(input, column, column, width)
        output = output(column + 1:)
    end function wrap_from_column

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
                    input(first:first) /= new_line("a") .and. &
                    input(first:first) /= achar(9)) exit
                first = first + 1
            end do
            if (first > input_length) exit

            last = first
            do while (last <= input_length)
                if (input(last:last) == " " .or. &
                    input(last:last) == new_line("a") .or. &
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
                output = output // new_line("a") // &
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
            if (input(index:index) == new_line("a") .and. &
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

    !> Join the trimmed items of a list with a separator.
    function join_trimmed(items, separator) result(text)
        character(len=*), intent(in) :: items(:)
        character(len=*), intent(in) :: separator
        character(len=:), allocatable :: text
        integer :: index

        text = ""
        do index = 1, size(items)
            if (index > 1) text = text // separator
            text = text // trim(items(index))
        end do
    end function join_trimmed

    !> append a text section to another already existing text-holding variable
    subroutine append_section(text, section)
        character(len=:), allocatable, intent(inout) :: text
        character(len=*), intent(in) :: section

        if (len(section) == 0) return
        if (len(text) > 0) text = text // new_line("a") // new_line("a")
        text = text // section
    end subroutine append_section

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

    integer function effective_width(self) result(width)
        class(StandardFormatter), intent(in) :: self
        character(len=16) :: text
        integer :: columns, length, stat

        width = self%help_width
        if (width < 20) width = 80
        if (.not. self%width_from_env) return

        ! Shells set COLUMNS but often do not export it; absence is normal.
        call get_environment_variable("COLUMNS", text, length, stat)
        if (stat /= 0 .or. length == 0 .or. length > len(text)) return
        if (verify(trim(text), "0123456789") /= 0) return
        read(text, *, iostat=stat) columns
        if (stat == 0 .and. columns >= 20) width = columns
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
