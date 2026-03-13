module fclap_error_stack
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use fclap_error_entry, only : error_entry

    implicit none

    private
    public :: error_stack

    !> A container for one or more errors
    type :: error_stack
        !> list of errors that came up during parsing
        type(error_entry), allocatable :: items(:)
        !> number of errors contained in the items list
        integer :: count = 0
    contains
        !> add an error of type(error_entry) to the items list
        procedure :: add_error
        !> check if the error stack contains any errors
        procedure :: has_errors
        !> print all errors of the error_stack
        procedure :: print_all
        procedure :: clear
        final :: destruct_stack
    end type
    
contains

    !> Add an error to the stack (dynamic array resizing)
    subroutine add_error(self, message, code, severity, arg_name)
        class(error_stack), intent(inout) :: self
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code, severity
        character(len=*), intent(in), optional :: arg_name
        
        type(error_entry), allocatable :: temp(:)
        type(error_entry) :: new_err
        
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

    !> check if the error_stack contains errors
    !> returns a logical
    logical function has_errors(self)
        class(error_stack), intent(in) :: self
        has_errors = (self%count > 0)
    end function

    subroutine print_all(self, unit)
        class(error_stack), intent(in) :: self
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
        class(error_stack), intent(inout) :: self
        
        ! Deallocate the array of error items
        if (allocated(self%items)) then
            deallocate(self%items)
        end if
        
        ! Reset the counter
        self%count = 0
    end subroutine clear

    subroutine destruct_stack(self)
        type(error_stack), intent(inout) :: self
        if (allocated(self%items)) deallocate(self%items)
    end subroutine
    
end module fclap_error_stack
    
    
    
    