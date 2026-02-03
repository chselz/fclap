!> Public API for fclap
!>
!> This module provides the main entry point to fclap,
!> a Fortran Command Line Argument Parser.
!> It re-exports all public types and procedures needed
!> for command line parsing.


module fclap
  use fclap_argparser, only : ArgParser, ParsedArgs, argparse_error, &
    & NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, NARGS_ONE_OR_MORE, &
    & NARGS_PARSER, NARGS_REMAINDER, NARGS_SINGLE, SUPPRESS, &
    & STATUS_ACTIVE, STATUS_DEPRECATED, STATUS_REMOVED
  use fclap_version, only : get_fclap_version, fclap_version_compact, &
    & fclap_version_string

  implicit none
  public

  ! Create an alias: Namespace => ParsedArgs for Python-style naming
  type, extends(ParsedArgs) :: Namespace
  end type Namespace

end module fclap
