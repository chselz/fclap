module test_subparsers
    use fclap, only : ArgumentParser, ErrorEntry, ErrorStack, FormatterType, &
        GroupHandle, HelpModel, ParseResult, FCLAP_OK, PARSE_SUCCESS, &
        PARSE_FAILURE, PARSE_HELP, ERR_DUPLICATE_DEST, ERR_MISSING_REQUIRED, &
        ERR_UNKNOWN_SUBCOMMAND, ERR_MISSING_SUBCOMMAND, &
        ERR_VALIDATION_FAILED, ip, not_less_than, store_true
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_subparsers

    type, extends(FormatterType) :: ChildFormatter
    contains
        procedure :: format_help => child_format_help
        procedure :: format_usage => child_format_usage
    end type ChildFormatter

contains

    subroutine collect_subparsers(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("one-level dispatch and merged namespace", &
                test_one_level_dispatch), &
            new_unittest("nested dispatch records full command path", &
                test_nested_dispatch), &
            new_unittest("unknown subcommand is structured failure", &
                test_unknown_subcommand), &
            new_unittest("required subcommand is structured failure", &
                test_required_subcommand), &
            new_unittest("optional subparser collection accepts no command", &
                test_optional_subcommands), &
            new_unittest("child errors and usage remain local", &
                test_child_errors), &
            new_unittest("subcommands appear in usage and help", &
                test_subcommand_help), &
            new_unittest("children own groups and polymorphic formatters", &
                test_child_independence), &
            new_unittest("parent definitions are deep copied", &
                test_parent_copy), &
            new_unittest("parent destination conflicts are explicit", &
                test_parent_conflict) &
        ]
    end subroutine collect_subparsers

    subroutine test_one_level_dispatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, run_parser
        type(ParseResult) :: result
        character(len=32) :: value
        logical :: verbose, force
        integer :: stat

        call run_parser%init(add_help=.false.)
        call run_parser%add_argument("input")
        call run_parser%add_argument("--force", action=store_true())

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("workspace")
        call parser%add_argument("--verbose", action=store_true())
        call parser%add_subparsers(dest="operation", required=.true.)
        call parser%add_parser("run", run_parser, help_text="run a job")

        result = parser%parse_tokens([character(len=9) :: &
            "project", "--verbose", "run", "input.dat", "--force"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("workspace", value, stat)
        call check(error, trim(value), "project")
        if (allocated(error)) return
        call result%namespace%get("operation", value, stat)
        call check(error, trim(value), "run")
        if (allocated(error)) return
        call result%namespace%get("input", value, stat)
        call check(error, trim(value), "input.dat")
        if (allocated(error)) return
        call result%namespace%get("verbose", verbose, stat)
        call check(error, verbose, .true.)
        if (allocated(error)) return
        call result%namespace%get("force", force, stat)
        call check(error, force, .true.)
        if (allocated(error)) return
        call check(error, size(result%selected_command_path), 1)
        if (allocated(error)) return
        call check(error, trim(result%selected_command_path(1)), "run")
    end subroutine test_one_level_dispatch

    subroutine test_nested_dispatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, remote_parser, add_parser
        type(ParseResult) :: result
        character(len=32) :: value
        integer(ip) :: priority
        integer :: stat

        call add_parser%init(add_help=.false.)
        call add_parser%add_argument("--scope", default="leaf")
        call add_parser%add_argument("--priority", data_type="integer", &
            default=1_ip)

        call remote_parser%init(add_help=.false.)
        call remote_parser%add_argument("--scope", default="remote")
        call remote_parser%add_subparsers(dest="command", required=.true.)
        call remote_parser%add_parser("add", add_parser)

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--scope", default="root")
        call parser%add_subparsers(dest="command", required=.true.)
        call parser%add_parser("remote", remote_parser)

        result = parser%parse_tokens([character(len=10) :: &
            "remote", "add", "--priority", "3"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, size(result%selected_command_path), 2)
        if (allocated(error)) return
        call check(error, trim(result%selected_command_path(1)), "remote")
        if (allocated(error)) return
        call check(error, trim(result%selected_command_path(2)), "add")
        if (allocated(error)) return

        ! Deeper namespaces overwrite parent keys; the deepest command
        ! destination consequently contains the leaf command.
        call result%namespace%get("scope", value, stat)
        call check(error, trim(value), "leaf")
        if (allocated(error)) return
        call result%namespace%get("command", value, stat)
        call check(error, trim(value), "add")
        if (allocated(error)) return
        call result%namespace%get("priority", priority, stat)
        call check(error, priority, 3_ip)
    end subroutine test_nested_dispatch

    subroutine test_unknown_subcommand(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, child
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call child%init(add_help=.false.)
        call parser%init(add_help=.false.)
        call parser%add_subparsers(required=.true.)
        call parser%add_parser("known", child)

        result = parser%parse_tokens([character(len=7) :: "unknown"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_SUBCOMMAND)
        if (allocated(error)) return
        call check(error, entry%flag, "unknown")
        if (allocated(error)) return
        call check(error, result%namespace%contains("command"), .false.)
    end subroutine test_unknown_subcommand

    subroutine test_required_subcommand(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        character(len=1), allocatable :: no_tokens(:)

        call parser%init(add_help=.false.)
        call parser%add_subparsers(required=.true.)
        allocate(no_tokens(0))

        result = parser%parse_tokens(no_tokens)
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        call check(error, result%errors%count(), 1)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_SUBCOMMAND)
    end subroutine test_required_subcommand

    subroutine test_optional_subcommands(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, child
        type(ParseResult) :: result
        character(len=1), allocatable :: no_tokens(:)
        logical :: verbose
        integer :: stat

        call child%init(add_help=.false.)
        call parser%init(add_help=.false.)
        call parser%add_argument("--verbose", action=store_true())
        call parser%add_subparsers()
        call parser%add_parser("run", child)
        allocate(no_tokens(0))

        result = parser%parse_tokens(no_tokens)
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, allocated(result%selected_command_path), .false.)
        if (allocated(error)) return
        call result%namespace%get("verbose", verbose, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, verbose, .false.)
    end subroutine test_optional_subcommands

    subroutine test_child_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, child
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        character(len=32) :: value
        integer :: stat

        call child%init(add_help=.false.)
        call child%add_argument("--name", required=.true.)
        call parser%init(prog="tool", add_help=.false.)
        call parser%add_subparsers(required=.true.)
        call parser%add_parser("run", child)
        ! Registration owns a snapshot; changing the source parser afterward
        ! cannot mutate the registered child.
        call child%init(add_help=.false.)
        call child%add_argument("--other")

        result = parser%parse_tokens([character(len=3) :: "run"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_REQUIRED)
        if (allocated(error)) return
        call check(error, result%text, "usage: tool run --name NAME")
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=6) :: &
            "run", "--name", "alice"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call check(error, result%errors%count(), 0)
        if (allocated(error)) return
        call result%namespace%get("name", value, stat)
        call check(error, trim(value), "alice")
    end subroutine test_child_errors

    subroutine test_subcommand_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, child
        character(len=:), allocatable :: expected
        character(len=1) :: nl

        nl = new_line('a')
        call child%init(add_help=.false.)
        call parser%init(prog="tool", add_help=.false.)
        call parser%add_subparsers(title="commands", &
            description="available operations", required=.true.)
        call parser%add_parser("clone", child, &
            help_text="clone a repository")
        call parser%add_parser("commit", child, &
            help_text="record changes")

        expected = "usage: tool {clone,commit}" // nl // nl // &
            "commands:" // nl // "  available operations" // nl // &
            "  clone" // repeat(" ", 17) // "clone a repository" // nl // &
            "  commit" // repeat(" ", 16) // "record changes"
        call check(error, parser%format_help(), expected)
    end subroutine test_subcommand_help

    subroutine test_child_independence(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, grouped_child, formatted_child
        type(GroupHandle) :: group
        type(ChildFormatter) :: formatter
        type(ParseResult) :: result

        call grouped_child%init(add_help=.true.)
        group = grouped_child%add_argument_group("advanced")
        call grouped_child%add_argument("--force", action=store_true(), &
            group=group)
        call formatted_child%init(formatter=formatter, add_help=.true.)

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_subparsers(required=.true.)
        call parser%add_parser("grouped", grouped_child)
        call parser%add_parser("formatted", formatted_child)
        call grouped_child%init(add_help=.false.)
        call formatted_child%init(add_help=.false.)

        result = parser%parse_tokens([character(len=7) :: &
            "grouped", "--help"])
        call check(error, result%outcome, PARSE_HELP)
        if (allocated(error)) return
        call check(error, index(result%text, "advanced:") > 0, .true.)
        if (allocated(error)) return
        call check(error, index(result%text, "usage: tool grouped") > 0, .true.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: &
            "formatted", "--help"])
        call check(error, result%outcome, PARSE_HELP)
        if (allocated(error)) return
        call check(error, result%text, "child help: tool formatted")
    end subroutine test_child_independence

    subroutine test_parent_copy(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parent, parser
        type(GroupHandle) :: shared
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        integer(ip) :: level
        logical :: verbose
        integer :: stat

        call parent%init(add_help=.true.)
        shared = parent%add_argument_group("shared options")
        call parent%add_argument("--level", data_type="integer", &
            default=1_ip, validator=not_less_than(1_ip), group=shared)
        call parent%add_argument("--verbose", action=store_true(), group=shared)

        call parser%init_with_parents([parent], prog="child", add_help=.true.)
        call parent%init(prog="reinitialized", add_help=.false.)

        ! The parent's automatic help action is not inherited, so the child
        ! owns exactly one help definition plus the two shared definitions.
        call check(error, parser%argument_count(), 3)
        if (allocated(error)) return
        call check(error, parser%group_count(), 1)
        if (allocated(error)) return
        call check(error, index(parser%format_help(), "shared options:") > 0, &
            .true.)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=7) :: "--level", "0"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_VALIDATION_FAILED)
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=9) :: &
            "--level", "2", "--verbose"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("level", level, stat)
        call check(error, level, 2_ip)
        if (allocated(error)) return
        call result%namespace%get("verbose", verbose, stat)
        call check(error, verbose, .true.)
    end subroutine test_parent_copy

    subroutine test_parent_conflict(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: first, second, parser
        type(ParseResult) :: result
        type(ErrorStack) :: errors
        type(ErrorEntry) :: entry
        character(len=1), allocatable :: no_tokens(:)

        call first%init(add_help=.false.)
        call first%add_argument("--first", dest="shared")
        call second%init(add_help=.false.)
        call second%add_argument("--second", dest="shared")
        call parser%init_with_parents([first, second], add_help=.false.)

        call check(error, parser%is_valid(), .false.)
        if (allocated(error)) return
        errors = parser%get_config_errors()
        entry = errors%get(1)
        call check(error, entry%code, ERR_DUPLICATE_DEST)
        if (allocated(error)) return
        allocate(no_tokens(0))
        result = parser%parse_tokens(no_tokens)
        call check(error, result%outcome, PARSE_FAILURE)
    end subroutine test_parent_conflict

    function child_format_usage(self, model) result(text)
        class(ChildFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: text

        text = "child usage: " // model%prog
    end function child_format_usage

    function child_format_help(self, model) result(text)
        class(ChildFormatter), intent(in) :: self
        type(HelpModel), intent(in) :: model
        character(len=:), allocatable :: text

        text = "child help: " // model%prog
    end function child_format_help

end module test_subparsers
