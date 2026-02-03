!> Utility functions for argparse
!> This module provides helper functions for command-line argument parsing.

module argparse_utils
    implicit none
    private
    
    public :: get_prog_name
    public :: strip_dashes
    public :: is_option_string
    public :: to_lower

contains
    !> Determine the program name similar to argparse's _prog_name
    !> A function rather than a subroutine was used to directly use the programname
    !> in the generation of the help message instead of needing to use a dummy variable
    !> print *, "Usage: ", get_prog_name(), " [options]"
    !>
    !> vs
    !>
    !> character(len=:), allocatable :: tmp_name
    !> call get_prog_name(tmp_name)
    !> print *, "Usage: ", tmp_name, " [options]"
    function get_prog_name(override) result(prog_name)
        character(len=*), intent(in), optional :: override
        character(len=:), allocatable :: prog_name

        character(len=:), allocatable :: arg0
        integer :: length, status, idx

        ! --- 1. Check for Explicit Override ---
        ! If the user manually provided a name, use it.
        if (present(override)) then
            prog_name = trim(override)
            return
        end if

        ! --- 2. Get arg0 (Executable Path) ---
        ! Fortran standard intrinsic to get the command line argument at index 0.
        ! We call it once to get the length, allocate the string, then call again to fill it.
        call get_command_argument(0, length=length, status=status)

        if (status == 0 .and. length > 0) then
            allocate(character(len=length) :: arg0)
            call get_command_argument(0, value=arg0, status=status)
            
            ! --- 3. Basename Logic (Simple Script equivalent) ---
            ! Find the last path separator to strip the directory path.
            ! We check for both '/' (Unix) and '\' (Windows).
            idx = scan(arg0, '/', back=.true.)
            if (idx == 0) idx = scan(arg0, '\', back=.true.)
            
            if (idx > 0) then
                ! Return the substring after the last separator
                prog_name = arg0(idx+1:)
            else
                ! No separators found, return the whole name
                prog_name = trim(arg0)
            end if
        else
            ! Fallback: If we can't determine the name (edge case)
            prog_name = "program"
        end if

    end function get_prog_name

    !> Strip leading dashes from an option string
    !> e.g., "--verbose" -> "verbose", "-v" -> "v"
    function strip_dashes(str) result(stripped)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: stripped
        integer :: i

        stripped = trim(str)
        do while (len(stripped) > 0 .and. stripped(1:1) == '-')
            stripped = stripped(2:)
        end do
    end function strip_dashes

    !> Check if a string is an option string (starts with dash)
    function is_option_string(str) result(is_option)
        character(len=*), intent(in) :: str
        logical :: is_option

        is_option = .false.
        if (len_trim(str) > 0) then
            is_option = (str(1:1) == '-')
        end if
    end function is_option_string

    !> Convert string to lowercase
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: lower_str
        integer :: i, ic

        lower_str = str
        do i = 1, len(lower_str)
            ic = ichar(lower_str(i:i))
            ! ASCII: 'A' = 65, 'Z' = 90
            if (ic >= 65 .and. ic <= 90) then
                lower_str(i:i) = char(ic + 32)
            end if
        end do
    end function to_lower

end module argparse_utils