program library_mode
    use fclap, only : ArgumentParser, ErrorEntry, ParseResult, FCLAP_OK, &
        PARSE_SUCCESS, PARSE_FAILURE, ERR_VALIDATION_FAILED, in_range
    implicit none

    type(ArgumentParser) :: parser
    type(ParseResult) :: result
    type(ErrorEntry) :: entry
    integer :: threads, stat

    call parser%init(prog="fclap-library")
    call parser%add_argument("-j", "--threads", data_type="integer", &
        default=1, validator=in_range(1, 64), &
        help="number of worker threads")

    ! parse_tokens never prints or terminates, so callers decide what to do.
    result = parser%parse_tokens([character(len=12) :: "--threads=8"])
    if (result%outcome /= PARSE_SUCCESS) error stop "valid tokens rejected"
    call result%namespace%get("threads", threads, stat)
    if (stat /= FCLAP_OK) error stop "missing thread count"
    write(*, "(a,i0)") "threads=", threads

    result = parser%parse_tokens([character(len=12) :: "-j", "128"])
    if (result%outcome /= PARSE_FAILURE) error stop "invalid tokens accepted"
    entry = result%errors%get(1)
    if (entry%code /= ERR_VALIDATION_FAILED) error stop "unexpected error code"
    write(*, "(a)") "rejected: " // parser%format_diagnostic(entry)
end program library_mode
