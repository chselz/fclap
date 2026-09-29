program cli_fixture
    use fclap, only : ArgumentParser, Namespace
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: arguments

    call parser%init(prog="fixture", version="fixture 1.0")
    call parser%add_argument("--name", default="world")
    call parser%add_argument("--legacy", deprecated_msg="use --name")
    arguments = parser%parse_args()
end program cli_fixture
