program nargs_example
    use fclap, only: ArgumentParser, Namespace, not_bigger_than
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: args
    integer :: count_val

    call parser%init(prog="multi_file")

    ! Accept a count that must be >= 5
    call parser%add_argument("count", data_type="int", action=not_bigger_than(5), &
                             help="A count value (must be >= 5)")

    args = parser%parse_args()

    call args%get("count", count_val)

    print '(A,I0)', "Count value: ", count_val
end program nargs_example