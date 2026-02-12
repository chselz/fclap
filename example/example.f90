program subcommand_example
    use fclap, only: ArgumentParser, Namespace
    implicit none

    type(ArgumentParser) :: parser, clone_parser, commit_parser
    type(Namespace) :: args
    character(len=64) :: command
    character(len=256) :: url, branch, message

    call parser%init(prog="mygit", description="A simple VCS")

    ! Add a global option (available before the subcommand)
    call parser%add_argument("--foo", action="store_true", help="foo help")

    ! Enable subparsers
    call parser%add_subparsers(title="commands", dest="command")

    ! Create the 'clone' subcommand with its own arguments
    call clone_parser%init(prog="mygit clone")
    call clone_parser%add_argument("url", help="Repository URL")
    call clone_parser%add_argument("-b", "--branch", help="Branch to clone")
    call parser%add_parser("clone", clone_parser, help_text="Clone a repository")

    ! Create the 'commit' subcommand with its own arguments
    call commit_parser%init(prog="mygit commit")
    call commit_parser%add_argument("-m", "--message", required=.true., &
                                    help="Commit message")
    call parser%add_parser("commit", commit_parser, help_text="Commit changes")

    args = parser%parse_args()

    ! Check which command was used
    call args%get("command", command)

    select case(trim(command))
    case("clone")
        call args%get("url", url)
        print *, "Cloning: ", trim(url)
        if (args%has_key("branch")) then
            call args%get("branch", branch)
            print *, "  Branch: ", trim(branch)
        end if
    case("commit")
        call args%get("message", message)
        print *, "Committing: ", trim(message)
    end select
end program subcommand_example
