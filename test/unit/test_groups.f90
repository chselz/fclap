module test_groups
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, GroupHandle, &
        HelpModel, ParseResult, FCLAP_OK, PARSE_SUCCESS, PARSE_FAILURE, &
        PARSE_HELP, ERR_INVALID_GROUP, ERR_MUTEX_CONFLICT, ERR_MUTEX_REQUIRED, &
        store_true
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_groups

contains

    subroutine collect_groups(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("ordinary group help snapshot", &
                test_argument_group_help), &
            new_unittest("mutex usage snapshot", test_mutex_usage), &
            new_unittest("optional mutex permits zero or one", &
                test_optional_mutex), &
            new_unittest("mutex conflict is structured failure", &
                test_mutex_conflict), &
            new_unittest("required mutex ignores defaults", &
                test_required_mutex), &
            new_unittest("help bypasses required mutex", &
                test_help_bypasses_mutex), &
            new_unittest("invalid group handles are rejected", &
                test_invalid_handles) &
        ]
    end subroutine collect_groups

    subroutine test_argument_group_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(GroupHandle) :: output_group
        type(HelpModel) :: model
        character(len=:), allocatable :: expected
        character(len=1) :: nl

        nl = new_line('a')
        call parser%init(prog="demo", add_help=.false.)
        call parser%add_argument("input", help="input file")
        output_group = parser%add_argument_group("output options", &
            description="Control generated files")
        call parser%add_argument("-o", "--output", help="output path", &
            group=output_group)
        call parser%add_argument("--force", action=store_true(), &
            help="replace existing files", print_default=.false., &
            group=output_group)
        call parser%add_argument("--verbose", action=store_true(), &
            help="verbose logging", print_default=.false.)

        expected = &
            "usage: demo INPUT [-o OUTPUT] [--force] [--verbose]" // &
            nl // nl // "positional arguments:" // nl // &
            "  INPUT                 input file" // nl // nl // &
            "options:" // nl // &
            "  --verbose             verbose logging" // nl // nl // &
            "output options:" // nl // &
            "  Control generated files" // nl // nl // &
            "  -o OUTPUT, --output OUTPUT" // nl // &
            repeat(" ", 24) // "output path" // nl // &
            "  --force               replace existing files"

        call check(error, parser%group_count(), 1)
        if (allocated(error)) return
        call check(error, parser%format_help(), expected)
        if (allocated(error)) return

        model = parser%get_help_model()
        call check(error, size(model%groups), 1)
        if (allocated(error)) return
        call check(error, model%groups(1)%title, "output options")
        if (allocated(error)) return
        call check(error, model%groups(1)%is_mutex, .false.)
        if (allocated(error)) return
        call check(error, all(model%groups(1)%argument_indices == [2, 3]), .true.)
    end subroutine test_argument_group_help

    subroutine test_mutex_usage(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(GroupHandle) :: mode_group, format_group

        call parser%init(prog="demo", add_help=.false.)
        mode_group = parser%add_mutually_exclusive_group()
        call parser%add_argument("--fast", action=store_true(), &
            print_default=.false., group=mode_group)
        call parser%add_argument("--safe", action=store_true(), &
            print_default=.false., group=mode_group)
        call parser%add_argument("--hidden", action=store_true(), &
            visible=.false., group=mode_group)
        format_group = parser%add_mutually_exclusive_group(required=.true.)
        call parser%add_argument("--json", action=store_true(), &
            print_default=.false., group=format_group)
        call parser%add_argument("--xml", action=store_true(), &
            print_default=.false., group=format_group)
        call parser%add_argument("--verbose", action=store_true(), &
            print_default=.false.)
        ! The first handle remains stable after the group array grows.
        call parser%add_argument("--auto", action=store_true(), &
            print_default=.false., group=mode_group)

        call check(error, parser%format_usage(), &
            "usage: demo [--fast | --safe | --auto] (--json | --xml) " // &
            "[--verbose]")
    end subroutine test_mutex_usage

    subroutine test_optional_mutex(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(GroupHandle) :: mode_group
        type(ParseResult) :: result
        character(len=1), allocatable :: no_tokens(:)
        logical :: fast
        integer :: stat

        call parser%init(add_help=.false.)
        mode_group = parser%add_mutually_exclusive_group(title="mode")
        call parser%add_argument("--fast", action=store_true(), group=mode_group)
        call parser%add_argument("--safe", action=store_true(), group=mode_group)
        allocate(no_tokens(0))

        result = parser%parse_tokens(no_tokens)
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("fast", fast, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, fast, .false.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=6) :: "--fast", "--fast"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%errors%count(), 0)
    end subroutine test_optional_mutex

    subroutine test_mutex_conflict(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(GroupHandle) :: mode_group
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(add_help=.false.)
        mode_group = parser%add_mutually_exclusive_group(title="mode")
        call parser%add_argument("--fast", action=store_true(), group=mode_group)
        call parser%add_argument("--safe", action=store_true(), group=mode_group)

        result = parser%parse_tokens([character(len=6) :: "--fast", "--safe"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MUTEX_CONFLICT)
        if (allocated(error)) return
        call check(error, entry%arg_name, "mode")
    end subroutine test_mutex_conflict

    subroutine test_required_mutex(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(GroupHandle) :: mode_group
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        character(len=1), allocatable :: no_tokens(:)
        logical :: fast, safe
        integer :: stat

        call parser%init(add_help=.false.)
        mode_group = parser%add_mutually_exclusive_group( &
            required=.true., title="mode")
        call parser%add_argument("--fast", action=store_true(), group=mode_group)
        call parser%add_argument("--safe", action=store_true(), group=mode_group)
        allocate(no_tokens(0))

        result = parser%parse_tokens(no_tokens)
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MUTEX_REQUIRED)
        if (allocated(error)) return
        call check(error, entry%arg_name, "mode")
        if (allocated(error)) return

        ! Both implicit false defaults exist, but neither counts as supplied.
        call result%namespace%get("fast", fast, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call result%namespace%get("safe", safe, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, fast .or. safe, .false.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=6) :: "--safe"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%errors%count(), 0)
    end subroutine test_required_mutex

    subroutine test_help_bypasses_mutex(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(GroupHandle) :: mode_group
        type(ParseResult) :: result

        call parser%init(prog="demo")
        mode_group = parser%add_mutually_exclusive_group(required=.true.)
        call parser%add_argument("--fast", action=store_true(), group=mode_group)
        call parser%add_argument("--safe", action=store_true(), group=mode_group)

        result = parser%parse_tokens([character(len=6) :: "--help"])
        call check(error, result%outcome, PARSE_HELP)
        if (allocated(error)) return
        call check(error, result%errors%count(), 0)
    end subroutine test_help_bypasses_mutex

    subroutine test_invalid_handles(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: owner, other, stale_parser, empty_title_parser
        type(GroupHandle) :: handle, stale, invalid, empty_title
        type(ErrorStack) :: errors

        call owner%init(add_help=.false.)
        call other%init(add_help=.false.)
        handle = owner%add_argument_group("owned")
        call other%add_argument("--foreign", group=handle)
        call check(error, other%argument_count(), 0)
        if (allocated(error)) return
        errors = other%get_config_errors()
        call check(error, error_code_at(errors, 1), ERR_INVALID_GROUP)
        if (allocated(error)) return

        call stale_parser%init(add_help=.false.)
        stale = stale_parser%add_argument_group("before reinit")
        call stale_parser%init(add_help=.false.)
        call stale_parser%add_argument("--stale", group=stale)
        call check(error, stale_parser%argument_count(), 0)
        if (allocated(error)) return
        errors = stale_parser%get_config_errors()
        call check(error, error_code_at(errors, 1), ERR_INVALID_GROUP)
        if (allocated(error)) return

        call owner%add_argument("--invalid", group=invalid)
        errors = owner%get_config_errors()
        call check(error, error_code_at(errors, 1), ERR_INVALID_GROUP)
        if (allocated(error)) return
        call check(error, owner%argument_count(), 0)
        if (allocated(error)) return

        call empty_title_parser%init(add_help=.false.)
        empty_title = empty_title_parser%add_argument_group("")
        call check(error, empty_title%is_valid(), .false.)
        if (allocated(error)) return
        call check(error, empty_title_parser%group_count(), 0)
        if (allocated(error)) return
        errors = empty_title_parser%get_config_errors()
        call check(error, error_code_at(errors, 1), ERR_INVALID_GROUP)
    end subroutine test_invalid_handles

    integer function error_code_at(errors, index) result(code)
        type(ErrorStack), intent(in) :: errors
        integer, intent(in) :: index
        type(ErrorEntry) :: entry

        entry = errors%get(index)
        code = entry%code
    end function error_code_at

end module test_groups
