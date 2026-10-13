module test_known_args
    use fclap, only : ArgumentParser, ErrorEntry, ParseResult, PARSE_SUCCESS, &
        PARSE_FAILURE, ERR_MISSING_VALUE, ERR_UNKNOWN_ARGUMENT, &
        ERR_UNKNOWN_SUBCOMMAND, store_true
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_known_args

contains

    subroutine collect_known_args(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("unknown options become extra tokens", &
                test_extra_tokens), &
            new_unittest("values of unknown options are not recognized", &
                test_separate_values), &
            new_unittest("known options still need their values", &
                test_missing_values), &
            new_unittest("subcommands collect extra tokens", &
                test_subcommands) &
        ]
    end subroutine collect_known_args

    subroutine test_extra_tokens(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        character(len=:), allocatable :: input
        integer :: threads, stat
        character(len=12), parameter :: tokens(7) = [character(len=12) :: &
            "-ksp_type=cg", "input.dat", "--threads", "4", "-log_view", &
            "--", "-extra"]

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("input")
        call parser%add_argument("-j", "--threads", data_type="integer")

        result = parser%parse_known_tokens(tokens)
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("input", input, stat)
        call check(error, input, "input.dat")
        if (allocated(error)) return
        call result%namespace%get("threads", threads, stat)
        call check(error, threads, 4)
        if (allocated(error)) return
        call check(error, allocated(result%extra_tokens))
        if (allocated(error)) return
        call check(error, size(result%extra_tokens), 3)
        if (allocated(error)) return
        call check(error, trim(result%extra_tokens(1)), "-ksp_type=cg")
        if (allocated(error)) return
        call check(error, trim(result%extra_tokens(2)), "-log_view")
        if (allocated(error)) return
        ! After `--` a surplus positional is kept as well.
        call check(error, trim(result%extra_tokens(3)), "-extra")
        if (allocated(error)) return

        ! Strict parsing rejects the same line and has no extra tokens.
        result = parser%parse_tokens(tokens)
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_ARGUMENT)
        if (allocated(error)) return
        call check(error, .not. allocated(result%extra_tokens))
        if (allocated(error)) return

        ! Nothing unknown gives an empty, allocated list.
        result = parser%parse_known_tokens([character(len=4) :: "in"])
        call check(error, allocated(result%extra_tokens))
        if (allocated(error)) return
        call check(error, size(result%extra_tokens), 0)
    end subroutine test_extra_tokens

    subroutine test_separate_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        character(len=:), allocatable :: input

        integer :: stat

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("input")

        ! As in argparse, `cg` is taken as the positional.
        result = parser%parse_known_tokens([character(len=9) :: &
            "-ksp_type", "cg", "input.dat"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("input", input, stat)
        call check(error, input, "cg")
        if (allocated(error)) return
        call check(error, size(result%extra_tokens), 2)
        if (allocated(error)) return
        call check(error, trim(result%extra_tokens(2)), "input.dat")
    end subroutine test_separate_values

    subroutine test_missing_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-j", "--threads", data_type="integer")

        result = parser%parse_known_tokens([character(len=9) :: &
            "--threads", "-foo"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
    end subroutine test_missing_values

    subroutine test_subcommands(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, run_parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry
        logical :: force
        integer :: stat

        call run_parser%init(add_help=.false.)
        call run_parser%add_argument("--force", action=store_true())
        call parser%init(prog="tool", add_help=.false.)
        call parser%add_subparsers()
        call parser%add_parser("run", run_parser)

        result = parser%parse_known_tokens([character(len=7) :: &
            "-foo", "run", "-bar", "--force", "x"])
        call check(error, result%outcome, PARSE_SUCCESS)
        if (allocated(error)) return
        call result%namespace%get("force", force, stat)
        call check(error, force)
        if (allocated(error)) return
        call check(error, size(result%extra_tokens), 3)
        if (allocated(error)) return
        call check(error, trim(result%extra_tokens(1)), "-foo")
        if (allocated(error)) return
        call check(error, trim(result%extra_tokens(3)), "x")
        if (allocated(error)) return

        ! An unknown command is still an error.
        result = parser%parse_known_tokens([character(len=7) :: "nope"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_SUBCOMMAND)
    end subroutine test_subcommands

end module test_known_args
