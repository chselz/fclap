program package_consumer
    use fclap, only : Argument, ArgumentParser, ParseResult, FCLAP_OK, &
        PARSE_SUCCESS, &
        fclap_version_string, get_fclap_version, store_true
    implicit none

    type(ArgumentParser) :: parser
    type(Argument) :: definition
    type(ParseResult) :: result
    character(len=:), allocatable :: version
    character(len=:), allocatable :: input
    integer :: count
    integer :: stat
    logical :: verbose

    call parser%init(prog="consumer", add_help=.false.)
    call parser%add_argument("input")
    call parser%add_argument("--count", data_type="integer", default=1)
    call parser%add_argument("--verbose", action=store_true())
    definition = parser%get_argument(1)
    if (definition%primary_name() /= "input") error stop "inspection failed"
    result = parser%parse_tokens([character(len=9) :: &
        "input.dat", "--count", "2", "--verbose"])

    if (result%outcome /= PARSE_SUCCESS) error stop "parse failed"
    call result%namespace%get("input", input, stat)
    if (stat /= FCLAP_OK .or. trim(input) /= "input.dat") then
        error stop "string retrieval failed"
    end if
    call result%namespace%get("count", count, stat)
    if (stat /= FCLAP_OK .or. count /= 2) then
        error stop "integer retrieval failed"
    end if
    call result%namespace%get("verbose", verbose, stat)
    if (stat /= FCLAP_OK .or. .not. verbose) then
        error stop "logical retrieval failed"
    end if

    call get_fclap_version(string=version)
    if (version /= fclap_version_string) error stop "version mismatch"
end program package_consumer
