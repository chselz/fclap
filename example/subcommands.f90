program subcommands
    use fclap, only : ArgumentParser, Namespace, store_true
    implicit none

    type(ArgumentParser) :: parser, run, clean
    type(Namespace) :: args
    character(len=:), allocatable :: command, input
    logical :: verbose, dry_run

    call parser%init(prog="fclap-subcommands", &
        description="Run or clean a calculation")
    ! A global option is also accepted after the command: `run -v in.dat`.
    call parser%add_argument("-v", "--verbose", action=store_true(), &
        help="enable verbose output", global=.true.)
    call parser%add_subparsers(title="commands", dest="command", &
        required=.true.)

    ! Children are copied when registered, so define them completely first.
    call run%init()
    call run%add_argument("input", help="input file")
    call run%add_argument("-n", "--dry-run", action=store_true(), &
        help="show what would be done")
    call parser%add_parser("run", run, help="run a calculation", &
        aliases=[character(len=1) :: "r"])

    call clean%init()
    call clean%add_argument("--all", action=store_true(), &
        help="also remove cached results")
    call parser%add_parser("clean", clean, help="remove outputs")

    args = parser%parse_args()

    call args%get("command", command)
    call args%get("verbose", verbose)

    write(*, "(a)") "command=" // command
    write(*, "(a,l1)") "verbose=", verbose
    select case (command)
    case ("run")
        ! Only the selected command's values exist in the namespace.
        call args%get("input", input)
        call args%get("dry_run", dry_run)
        write(*, "(a)") "input=" // input
        write(*, "(a,l1)") "dry_run=", dry_run
    case default
        ! "clean" has no arguments of its own.
    end select
end program subcommands
