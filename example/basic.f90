program basic
    use fclap, only : ArgumentParser, Namespace, not_less_than, store_true
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: args
    character(len=:), allocatable :: input
    integer :: threads
    logical :: verbose

    call parser%init(prog="fclap-basic", &
        description="Process one input file")
    call parser%add_argument("input", help="input file")
    call parser%add_argument("-v", "--verbose", action=store_true(), &
        help="enable verbose output")
    call parser%add_argument("-j", "--threads", data_type="integer", &
        default=1, validator=not_less_than(1), &
        help="number of worker threads")

    args = parser%parse_args()

    ! Every value here always exists (a positional, a flag, or a default), so
    ! no stat= is needed: a mistyped key would stop with a clear message.
    call args%get("input", input)
    call args%get("verbose", verbose)
    call args%get("threads", threads)

    write(*, "(a)") "input=" // input
    write(*, "(a,l1)") "verbose=", verbose
    write(*, "(a,i0)") "threads=", threads
end program basic
