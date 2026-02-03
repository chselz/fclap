!> Error handling module for fclap
!>
!> This module defines the error type and error handling procedures
!> used throughout the fclap library.

module fclap_errors
    implicit none
    private

    !> Error type for fclap errors
    !> Stores error information including message, related argument, and error state.
    type, public :: fclap_error
        !> The error message describing what went wrong
        character(len=:), allocatable :: message
        !> The argument name or value that caused the error
        character(len=:), allocatable :: argument
        !> Flag indicating whether an error has occurred
        logical :: has_error = .false.
    contains
        procedure :: init => error_init
        procedure :: report => error_report
        procedure :: clear => error_clear
    end type fclap_error

contains

    !> Initialize an error with message and optional argument
    subroutine error_init(self, message, argument)
        class(fclap_error), intent(inout) :: self
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: argument

        self%message = trim(message)
        self%has_error = .true.
        if (present(argument)) then
            self%argument = trim(argument)
        end if
    end subroutine error_init

    !> Report the error message to a unit (default: stderr)
    subroutine error_report(self, unit)
        class(fclap_error), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: out_unit

        out_unit = 0  ! stderr
        if (present(unit)) out_unit = unit

        if (self%has_error) then
            if (allocated(self%argument)) then
                write(*, '(A,A,A,A)') "error: ", trim(self%message), " -- ", trim(self%argument)
            else
                write(*, '(A,A)') "error: ", trim(self%message)
            end if
        end if
    end subroutine error_report

    !> Clear the error state
    subroutine error_clear(self)
        class(fclap_error), intent(inout) :: self

        self%has_error = .false.
        if (allocated(self%message)) deallocate(self%message)
        if (allocated(self%argument)) deallocate(self%argument)
    end subroutine error_clear

end module fclap_errors
