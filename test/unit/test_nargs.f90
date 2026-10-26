module test_nargs
    use fclap_nargs, only : NargsSpec, new_nargs, normalize_nargs, &
        NARGS_INVALID, NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, &
        NARGS_ONE_OR_MORE, NARGS_REMAINDER, NARGS_UNBOUNDED, &
        NARGS_SUCCESS, NARGS_INVALID_VALUE
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_nargs

contains

    subroutine collect_nargs(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("exact count", test_exact_count), &
            new_unittest("symbolic forms", test_symbolic_forms), &
            new_unittest("reject negative count", test_reject_negative_count), &
            new_unittest("reject unknown symbol", test_reject_unknown_symbol) &
        ]
    end subroutine collect_nargs

    subroutine test_exact_count(error)
        type(error_type), allocatable, intent(out) :: error
        type(NargsSpec) :: spec
        integer :: stat

        call normalize_nargs(3, spec, stat)
        call check(error, stat, NARGS_SUCCESS)
        if (allocated(error)) return
        call check(error, spec%value(), 3)
        if (allocated(error)) return
        call check(error, spec%min_count(), 3)
        if (allocated(error)) return
        call check(error, spec%max_count(), 3)
        if (allocated(error)) return
        call check(error, spec%is_variable(), .false.)
        if (allocated(error)) return
        call check(error, spec%produces_list(), .true.)
        if (allocated(error)) return
        call check(error, spec%to_string(), "3")
    end subroutine test_exact_count

    subroutine test_symbolic_forms(error)
        type(error_type), allocatable, intent(out) :: error
        type(NargsSpec) :: spec

        spec = new_nargs("?")
        call check(error, spec%value(), NARGS_OPTIONAL)
        if (allocated(error)) return
        call check(error, spec%min_count(), 0)
        if (allocated(error)) return
        call check(error, spec%max_count(), 1)
        if (allocated(error)) return

        spec = new_nargs("*")
        call check(error, spec%value(), NARGS_ZERO_OR_MORE)
        if (allocated(error)) return
        call check(error, spec%max_count(), NARGS_UNBOUNDED)
        if (allocated(error)) return
        call check(error, spec%produces_list(), .true.)
        if (allocated(error)) return

        spec = new_nargs("+")
        call check(error, spec%value(), NARGS_ONE_OR_MORE)
        if (allocated(error)) return
        call check(error, spec%min_count(), 1)
        if (allocated(error)) return

        spec = new_nargs("remainder")
        call check(error, spec%value(), NARGS_REMAINDER)
        if (allocated(error)) return
        call check(error, spec%to_string(), "remainder")
        if (allocated(error)) return

        spec = new_nargs(NARGS_REMAINDER)
        call check(error, spec%value(), NARGS_REMAINDER)
    end subroutine test_symbolic_forms

    ! Expected-failure behavior is asserted as ordinary passing tests: invalid
    ! user input must return structured status, not abort the test process.
    subroutine test_reject_negative_count(error)
        type(error_type), allocatable, intent(out) :: error
        type(NargsSpec) :: spec
        integer :: stat

        ! -1 is especially important: it is an internal symbolic code, but a
        ! negative integer supplied through the public API is invalid.
        call normalize_nargs(-1, spec, stat)
        call check(error, stat, NARGS_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, spec%value(), NARGS_INVALID)
        if (allocated(error)) return
        call check(error, spec%is_valid(), .false.)
    end subroutine test_reject_negative_count

    subroutine test_reject_unknown_symbol(error)
        type(error_type), allocatable, intent(out) :: error
        type(NargsSpec) :: spec
        integer :: stat

        call normalize_nargs("many", spec, stat)
        call check(error, stat, NARGS_INVALID_VALUE)
        if (allocated(error)) return
        call check(error, spec%value(), NARGS_INVALID)
        if (allocated(error)) return
        call check(error, spec%to_string(), "invalid")
    end subroutine test_reject_unknown_symbol

end module test_nargs
