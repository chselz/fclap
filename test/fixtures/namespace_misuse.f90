program namespace_misuse
    use fclap, only : ArgumentParser, Namespace
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: arguments
    character(len=:), allocatable :: value

    call parser%init(prog="fixture")
    call parser%add_argument("--name", default="world")
    arguments = parser%parse_args()
    ! A mistyped key without stat= must stop instead of being ignored.
    call arguments%get("nmae", value)
end program namespace_misuse
