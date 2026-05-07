!> @file fclap_errors.f90
!> @brief Error handling module for fclap.
!>
!> @details This module defines the fclap_error type and error handling
!> procedures used throughout the fclap library. Errors are used to
!> communicate parsing failures and invalid argument usage.

module fclap_errors
    ! TODO use iso fortran env for stderr
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    implicit none
    private

    public :: fclap_error

    !> @brief Error type for fclap parsing and validation errors.
    !>
    !> @details Stores error information including a message, the related
    !> argument name, and a flag indicating whether an error has occurred.
    !> Used throughout fclap to report and handle errors.
    type :: fclap_error
        !> The error message describing what went wrong
        character(len=:), allocatable :: message
        !> The argument name or value that caused the error
        character(len=:), allocatable :: argument
        !> @brief Flag indicating whether an error has occurred
        logical :: has_error = .false.
    contains
        !> @brief Initialize an error with message and optional argument.
        !> @param message The error message
        !> @param argument Optional related argument name
        procedure :: init => error_init
        !> @brief Report the error message to an output unit.
        !> @param unit Optional output unit (default: stderr)
        procedure :: report => error_report
        !> @brief Clear the error state.
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
                write(stderr, '(A,A,A,A)') "error: ", trim(self%message), " -- ", trim(self%argument)
            else
                write(stderr, '(A,A)') "error: ", trim(self%message)
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
