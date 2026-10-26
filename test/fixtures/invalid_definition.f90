program invalid_definition
    use fclap, only : ArgumentParser, Namespace
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: arguments

    ! A duplicate option is a programming error, not a user error.
    call parser%init(prog="fixture")
    call parser%add_argument("-o", "--output")
    call parser%add_argument("-x", "--output")
    arguments = parser%parse_args()
end program invalid_definition
