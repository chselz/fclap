program nargs_example
    use fclap, only: ArgumentParser, Namespace, ARG_ONE_OR_MORE

    type(ArgumentParser) :: parser
    type(Namespace) :: args
    character(len=256) :: files(64)
    integer :: num_files, i

    call parser%init(prog="multi_file")

    ! Accept one or more input files
    call parser%add_argument("files", nargs=ARG_ONE_OR_MORE, &
                             help="Input files to process")

    args = parser%parse_args()

    ! Get the list of files
    call args%get("files", files, num_files)

    print '(A,I0,A)', "Processing ", num_files, " files:"
    do i = 1, num_files
        print '(A,A)', "  - ", trim(files(i))
    end do
end program nargs_example