!> Public API for fclap
!>
!> This module provides the main entry point to fclap,
!> a Fortran Command Line Argument Parser.
!> It re-exports all public types and procedures needed
!> for command line parsing.


module fclap
  use fclap_argparser, only : ArgParser
  use fclap_version, only : get_fclap_version, fclap_version_compact, &
    & fclap_version_string

  implicit none
  public

end module fclap
