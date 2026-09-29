program basic
    use fclap, only : ArgumentParser, Namespace, FCLAP_OK, ip, &
        not_less_than, store_true
    implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: args
    character(len=256) :: input
    integer(ip) :: threads
    integer :: stat
    logical :: verbose

    call parser%init(prog="fclap-basic", &
        description="Process one input file")
    call parser%add_argument("input", help="input file")
    call parser%add_argument("-v", "--verbose", action=store_true(), &
        help="enable verbose output")
    call parser%add_argument("-j", "--threads", data_type="integer", &
        default=1_ip, validator=not_less_than(1_ip), &
        help="number of worker threads")

    args = parser%parse_args()
    call args%get("input", input, stat)
    !if (stat /= FCLAP_OK) error stop "missing input"
    call args%get("verbose", verbose, stat)
   ! if (stat /= FCLAP_OK) error stop "missing verbose flag"
    call args%get("threads", threads, stat)
    !if (stat /= FCLAP_OK) error stop "missing thread count"

    write(*, '(a)') "input=" // trim(input)
    write(*, '(a,l1)') "verbose=", verbose
    write(*, '(a,i0)') "threads=", threads
end program basic
