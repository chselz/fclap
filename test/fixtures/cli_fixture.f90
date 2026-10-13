program cli_fixture
    use fclap, only : ArgumentParser, Namespace, store_true
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: arguments
    logical :: check
    integer :: stat

    call parser%init(prog="fixture", version="fixture 1.0")
    call parser%add_argument("--name", default="world")
    call parser%add_argument("--legacy", deprecated_msg="use --name")
    call parser%add_argument("--check", action=store_true())
    call parser%add_argument("--jobs", data_type="integer", default=1, &
        env="FIXTURE_JOBS")
    arguments = parser%parse_args()

    ! An application-level check reported like any other usage error.
    call arguments%get("check", check, stat)
    if (check) call parser%error("check failed")
end program cli_fixture
