module test_errors
    use fclap_error_codes, only : ERR_MISSING_VALUE, &
        ERR_DEPRECATED_ARGUMENT, ERROR_FATAL, ERROR_WARNING
    use fclap_error_entry, only : ErrorEntry
    use fclap_error_stack, only : ErrorStack
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_errors

contains

    subroutine collect_errors(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("empty stack", test_empty_stack), &
            new_unittest("structured entries", test_structured_entries), &
            new_unittest("merge and clear", test_merge_and_clear) &
        ]
    end subroutine collect_errors

    subroutine test_empty_stack(error)
        type(error_type), allocatable, intent(out) :: error
        type(ErrorStack) :: stack

        call check(error, stack%count(), 0)
        if (allocated(error)) return
        call check(error, stack%has_errors(), .false.)
        if (allocated(error)) return
        call check(error, stack%has_fatal_errors(), .false.)
        if (allocated(error)) return
        call check(error, stack%has_warnings(), .false.)
        if (allocated(error)) return
        call check(error, stack%format_all(), "")
    end subroutine test_empty_stack

    subroutine test_structured_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(ErrorStack) :: stack
        type(ErrorEntry) :: entry
        character(len=:), allocatable :: expected

        call stack%add("option requires a value", code=ERR_MISSING_VALUE, &
            severity=ERROR_FATAL, arg_name="--output", token="--output", &
            token_index=2)
        call stack%add("option is deprecated", code=ERR_DEPRECATED_ARGUMENT, &
            severity=ERROR_WARNING, arg_name="--old")

        call check(error, stack%count(), 2)
        if (allocated(error)) return
        call check(error, stack%has_errors(), .true.)
        if (allocated(error)) return
        call check(error, stack%has_fatal_errors(), .true.)
        if (allocated(error)) return
        call check(error, stack%has_warnings(), .true.)
        if (allocated(error)) return

        entry = stack%get(1)
        call check(error, entry%code, ERR_MISSING_VALUE)
        if (allocated(error)) return
        call check(error, entry%severity, ERROR_FATAL)
        if (allocated(error)) return
        call check(error, entry%arg_name, "--output")
        if (allocated(error)) return
        call check(error, entry%flag_index, 2)
        if (allocated(error)) return

        expected = "[FATAL] option requires a value" // &
            " (argument: --output) (flag: --output) (index: 2)" // &
            new_line('a') // &
            "[WARNING] option is deprecated (argument: --old)"
        call check(error, stack%format_all(), expected)
    end subroutine test_structured_entries

    subroutine test_merge_and_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(ErrorStack) :: first, second

        call first%add("fatal", code=ERR_MISSING_VALUE)
        call second%add("warning", code=ERR_DEPRECATED_ARGUMENT, &
            severity=ERROR_WARNING)
        call first%merge(second)

        call check(error, first%count(), 2)
        if (allocated(error)) return
        call check(error, second%count(), 1)
        if (allocated(error)) return

        call first%clear()
        call check(error, first%count(), 0)
        if (allocated(error)) return
        call check(error, second%count(), 1)
    end subroutine test_merge_and_clear

end module test_errors
