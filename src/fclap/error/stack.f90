module fclap_error_stack
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use fclap_error_entry, only : ErrorEntry

    implicit none

    private
    public :: ErrorStack

    !> A container for one or more errors
    type :: ErrorStack
        !> list of errors that came up during parsing
        type(ErrorEntry), allocatable :: items(:)
        !> number of errors contained in the items list
        integer :: count = 0
    contains
        !> add an error of type(ErrorEntry) to the items list
        procedure :: add_error
        !> check if the error stack contains any errors
        procedure :: has_errors
        !> print all errors of the error_stack
        procedure :: print_all
        procedure :: clear
        final :: destruct_stack
    end type ErrorStack
    
contains

    !> Add an error to the stack (dynamic array resizing)
    subroutine add_error(self, message, code, severity, arg_name)
        class(ErrorStack), intent(inout) :: self
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code, severity
        character(len=*), intent(in), optional :: arg_name
        
        type(ErrorEntry), allocatable :: temp(:)
        type(ErrorEntry) :: new_err
        
        ! Set defaults
        new_err%message = trim(message)
        if (present(code)) new_err%code = code
        if (present(severity)) new_err%severity = severity
        if (present(arg_name)) new_err%arg_name = trim(arg_name)

        ! Resize logic
        if (.not. allocated(self%items)) then
            allocate(self%items(1))
        else
            call move_alloc(self%items, temp)
            allocate(self%items(self%count + 1))
            self%items(1:self%count) = temp
        end if

        self%count = self%count + 1
        self%items(self%count) = new_err
    end subroutine

    !> check if the ErrorStack contains errors
    !> returns a logical
    logical function has_errors(self)
        class(ErrorStack), intent(in) :: self
        has_errors = (self%count > 0)
    end function

    subroutine print_all(self, unit)
        class(ErrorStack), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: i
        integer :: lun = stderr

        if (present(unit)) lun = unit

        if (self%count == 0) then
            write(lun, '(A)') "No errors found."
            return
        end if

        write(lun, '(A,I0,A)') "=== Found ", self%count, " errors ==="
        do i = 1, self%count
            write(lun, '(A)') self%items(i)%to_string()
        end do
    end subroutine

    subroutine clear(self)
        class(ErrorStack), intent(inout) :: self
        
        ! Deallocate the array of error items
        if (allocated(self%items)) then
            deallocate(self%items)
        end if
        
        ! Reset the counter
        self%count = 0
    end subroutine clear

    subroutine destruct_stack(self)
        type(ErrorStack), intent(inout) :: self
        if (allocated(self%items)) deallocate(self%items)
    end subroutine
    
end module fclap_error_stack
