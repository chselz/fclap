!> Ordered collection of structured fclap diagnostics.
module fclap_error_stack
    use, intrinsic :: iso_fortran_env, only : error_unit
    use fclap_error_codes, only : ERROR_FATAL, ERROR_WARNING
    use fclap_error_entry, only : ErrorEntry
    implicit none
    private

    public :: ErrorStack

    type :: ErrorStack
        private
        !> List of error entries in the stack
        type(ErrorEntry), allocatable :: items(:)
    contains
        procedure :: add       => error_stack_add
        procedure :: add_error => error_stack_add
        procedure :: append    => error_stack_append_entry
        procedure :: merge     => error_stack_merge
        procedure :: count     => error_stack_count
        procedure :: get       => error_stack_get
        procedure :: has_errors
        procedure :: has_fatal_errors
        procedure :: has_warnings
        procedure :: format_all
        procedure :: print_all
        procedure :: clear
    end type ErrorStack

contains

    subroutine error_stack_add(self, message, code, severity, arg_name, token, token_index)
        class(ErrorStack), intent(inout) :: self
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code, severity
        character(len=*), intent(in), optional :: arg_name, token
        integer, intent(in), optional :: token_index
        type(ErrorEntry) :: entry

        call entry%init(message, code, severity, arg_name, token, token_index)
        call self%append(entry)
    end subroutine error_stack_add

    subroutine error_stack_append_entry(self, entry)
        class(ErrorStack), intent(inout) :: self
        type(ErrorEntry), intent(in) :: entry
        type(ErrorEntry), allocatable :: temporary(:)
        integer :: current_size

        current_size = self%count()
        allocate(temporary(current_size + 1))
        if (current_size > 0) temporary(:current_size) = self%items
        temporary(current_size + 1) = entry
        call move_alloc(temporary, self%items)
    end subroutine error_stack_append_entry

    subroutine error_stack_merge(self, other)
        class(ErrorStack), intent(inout) :: self
        type(ErrorStack), intent(in) :: other
        integer :: index

        do index = 1, other%count()
            call self%append(other%items(index))
        end do
    end subroutine error_stack_merge

    pure integer function error_stack_count(self) result(number)
        class(ErrorStack), intent(in) :: self

        if (allocated(self%items)) then
            number = size(self%items)
        else
            number = 0
        end if
    end function error_stack_count

    function error_stack_get(self, index) result(entry)
        class(ErrorStack), intent(in) :: self
        integer, intent(in) :: index
        type(ErrorEntry) :: entry

        if (index >= 1 .and. index <= self%count()) entry = self%items(index)
    end function error_stack_get

    pure logical function has_errors(self)
        class(ErrorStack), intent(in) :: self

        has_errors = self%count() > 0
    end function has_errors

    pure logical function has_fatal_errors(self)
        class(ErrorStack), intent(in) :: self
        integer :: index

        has_fatal_errors = .false.
        do index = 1, self%count()
            if (self%items(index)%severity == ERROR_FATAL) then
                has_fatal_errors = .true.
                return
            end if
        end do
    end function has_fatal_errors

    pure logical function has_warnings(self)
        class(ErrorStack), intent(in) :: self
        integer :: index

        has_warnings = .false.
        do index = 1, self%count()
            if (self%items(index)%severity == ERROR_WARNING) then
                has_warnings = .true.
                return
            end if
        end do
    end function has_warnings

    function format_all(self) result(text)
        class(ErrorStack), intent(in) :: self
        character(len=:), allocatable :: text
        integer :: index

        text = ""
        do index = 1, self%count()
            if (index > 1) text = text // new_line('a')
            text = text // self%items(index)%to_string()
        end do
    end function format_all

    subroutine print_all(self, unit)
        class(ErrorStack), intent(in) :: self
        integer, intent(in), optional :: unit
        integer :: output, index

        output = error_unit
        if (present(unit)) output = unit

        do index = 1, self%count()
            write(output, '(a)') self%items(index)%to_string()
        end do
    end subroutine print_all

    subroutine clear(self)
        class(ErrorStack), intent(inout) :: self

        if (allocated(self%items)) deallocate(self%items)
    end subroutine clear

end module fclap_error_stack
