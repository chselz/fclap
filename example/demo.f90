program demo
    use fclap, only: ArgumentParser, Namespace
        implicit none

    type(ArgumentParser) :: parser
    type(Namespace) :: argholder
    type :: args_t
        character(len=256) :: filename, output
        logical :: verbose
        integer :: count
    end type args_t
    type(args_t) :: args
    
    call parser%init(prog="processor")
    
    ! Positional argument
    call parser%add_argument("filename", help="File to process")
    
    ! Optional arguments
    call parser%add_argument("-v", "--verbose", action="store_true", &
                                help="Enable verbose output")
    call parser%add_argument("-o", "--output", default_val="out.txt", &
                                help="Output file (default: out.txt)")
    call parser%add_argument("-c", "--count", data_type="int", default_val="1", &
                                help="Number of iterations")
    
    argholder = parser%parse_args()
    
    args%filename = argholder%get_string("filename")
    args%output = argholder%get_string("output")
    args%verbose = argholder%get_logical("verbose")
    args%count = argholder%get_integer("count")
    
    if (args%verbose) print *, "Verbose mode enabled"
    print '(A,A)', "Processing: ", trim(args%filename)
    print '(A,A)', "Output to:  ", trim(args%output)
    print '(A,I0,A)', "Running ", args%count, " iterations"
end program demo
