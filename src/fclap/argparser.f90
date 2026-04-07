!> @file fclap_parser.f90
!> @brief Parser module for fclap - the core command-line parsing engine.
!>
!> This module defines the main ArgumentParser type and provides
!> all functionality for defining arguments, parsing command-line input,
!> and generating help messages.
!>
!> @details The ArgumentParser is the central class of fclap. It handles:
!> - Registration of positional and optional arguments
!> - Parsing of command-line input into a Namespace object
!> - Generation of help and usage messages
!> - Support for subparsers (subcommands)
!> - Argument groups and mutually exclusive groups
!>
!> @example
!>   use fclap, only: ArgumentParser, Namespace
!>   type(ArgumentParser) :: parser
!>   type(Namespace) :: args
!>   call parser%init(prog="myapp", description="My application")
!>   call parser%add_argument("filename", help="Input file")
!>   args = parser%parse_args()

module flcap_parser
    use fclap_error_stack, only : ErrorStack
    use flcap_formatter_abstract, only : FormatterType
    use fclap_formatter_standard, only : StandardFormatter
    use fclap_namespace, only : Namespace
    implicit none
    private

    type :: ArgumentParser
        !> @brief The name of the program (e.g., from argv(0))
        character(len=:), allocatable :: prog
        !> @brief The string describing the program usage (default: generated from arguments added to parser)
        character(len=:), allocatable :: usage
        !> @brief Description text displayed before arguments in help output
        character(len=:), allocatable :: description
        !> @brief Epilog text displayed after arguments in help output
        character(len=:), allocatable :: epilog
        !> @brief Version string for --version output
        character(len=:), allocatable :: version
        !> @brief A type to customize the help output
        class(FormatterType) :: formatter
        !> @brief Whether to automatically add -h/--help argument (default .true.)
        logical :: add_help = .true.
        !> @brief Whether to exit automatically when an error occurs (default .true.)
        logical :: exit_on_error = .true.
    contains
        !> @brief Initialize the parser with optional configuration.
        !> @param prog Optional program name (defaults to argv(0))
        !> @param description Optional description for help output
        !> @param epilog Optional text after arguments in help
        !> @param add_help Whether to add -h/--help (default: .true.)
        !> @param version Optional version string for --version
        procedure :: init => parser_init
    end type ArgumentParser
contains
    ! in der init function from argparser
    ! if(not present(formatter)) then
    !    formatter = StandardFormatter

    subroutine parser_init(self, prog, usage, description, epilog, version, help_formatter, add_help, exit_on_error, parents)
        class(ArgumentParser), intent(inout) :: self
        character(len=*), intent(in), optional :: prog
        character(len=*), intent(in), optional :: usage
        character(len=*), intent(in), optional :: description
        character(len=*), intent(in), optional :: epilog
        character(len=*), intent(in), optional :: version
        class(FormatterType), intent(in), optional :: help_formatter
        logical, intent(in), optional :: add_help
        logical, intent(in), optional :: exit_on_error
        type(ArgumentParser), intent(in), optional :: parents(:)

        if (present(prog)) then
            self%prog = trim(prog)
        else
            self%prog = get_prog_name()
        end if

        if (present(usage)) self%usage = trim(usage)
        if (present(description)) self%description = trim(description)
        if (present(epilog)) self%epilog = trim(epilog)
        if (present(version)) self%version = trim(version)
        if (present(add_help)) self%add_help = add_help

        if (present(help_formatter)) then
            self%formatter = help_formatter
        else 
            self%formatter = StandardFormatter()
        end if

        if (self%add_help) then
            call self%add_argument("-h", "--help", action="help", &
                                   help="show this help message and exit")
        end if

        if (present(version)) then
            call self%add_argument("--version", action="version", &
                                   help="show program's version number and exit")
        end if

        if (present(parents)) then
            do i = 1, size(parents)
                call self%copy_arguments_from(parents(i))
            end do
        end if
    end subroutine parser_init

    recursive function parser_parse_args(self, err_stack) result(args)
        class(ArgumentParser), intent(inout) :: self
        type(ErrorStack), intent(inout), optional :: err_stack
        type(Namespace) :: args
    end function parser_parse_args

    ! ============================================================================
    ! UTILITY FUNCTIONS
    ! ============================================================================

    !> @brief Get the program name from the command line.
    !>
    !> @details Retrieves the program name from argv(0), stripping any path
    !> components to return just the executable name. If an override is
    !> provided, it will be used instead.
    !>
    !> @param override Optional custom program name to use
    !> @return The program name string
    function get_prog_name(override) result(prog_name)
        character(len=*), intent(in), optional :: override
        character(len=:), allocatable :: prog_name
        character(len=:), allocatable :: arg0
        integer :: length, status, idx

        if (present(override)) then
            prog_name = trim(override)
            return
        end if

        call get_command_argument(0, length=length, status=status)

        if (status == 0 .and. length > 0) then
            allocate(character(len=length) :: arg0)
            call get_command_argument(0, value=arg0, status=status)
            
            idx = scan(arg0, '/', back=.true.)
            if (idx == 0) idx = scan(arg0, '\', back=.true.)
            
            if (idx > 0) then
                prog_name = arg0(idx+1:)
            else
                prog_name = trim(arg0)
            end if
        else
            prog_name = "program"
        end if
    end function get_prog_name

end module flcap_parser