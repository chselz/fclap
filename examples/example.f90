program example
  use fclap, only: ArgumentParser, Namespace

type(ArgumentParser) :: parser
type(Namespace) :: args

call parser%init(prog="myapp", description="My application")
call parser%add_argument("filename", help="Input file")
call parser%add_argument("-v", "--verbose", action="store_true")
args = parser%parse_args()
    
end program example
