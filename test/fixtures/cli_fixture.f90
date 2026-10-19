program cli_fixture
    use fclap, only : ArgumentParser, Namespace, StandardFormatter, store_true
    implicit none

    type(ArgumentParser) :: parser, show
    type(Namespace) :: arguments
    character(len=:), allocatable :: echo
    logical :: check
    integer :: stat

    call parser%init(prog="fixture", version="fixture 1.0", &
        formatter=StandardFormatter(width_from_env=.true.))
    call parser%add_argument("--name", default="world")
    call parser%add_argument("--legacy", deprecated_msg="use --name")
    call parser%add_argument("--check", action=store_true())
    call parser%add_argument("--jobs", data_type="integer", default=1, &
        env="FIXTURE_JOBS")
    call parser%add_argument("--echo", help="print the value in brackets")
    call show%init(help_on_empty=.true.)
    call show%add_argument("--what", help="item to show")
    call parser%add_subparsers()
    call parser%add_parser("show", show, help="show an item")
    arguments = parser%parse_args()

    ! An application-level check reported like any other usage error.
    call arguments%get("check", check, stat)
    if (check) call parser%error("check failed")
    ! Brackets show that trailing blanks of the value survive.
    if (arguments%contains("echo")) then
        call arguments%get("echo", echo)
        print "(a)", "[" // echo // "]"
    end if
end program cli_fixture
