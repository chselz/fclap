module test_values
    use fclap_utils_accuracy, only : ip, wp
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : StringValue, IntegerValue, RealValue, &
        LogicalValue, ListValue, new_value
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_values

contains

    subroutine collect_values(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("scalar values", test_scalar_values), &
            new_unittest("deep clone", test_deep_clone), &
            new_unittest("typed list", test_typed_list), &
            new_unittest("empty typed list", test_empty_typed_list) &
        ]
    end subroutine collect_values

    subroutine test_scalar_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(ValueBox) :: box

        box = new_value("input.dat")
        call check(error, box%type_name(), "string")
        if (allocated(error)) return
        call check(error, box%to_string(), "'input.dat'")
        if (allocated(error)) return

        box = new_value(7_ip)
        call check(error, box%type_name(), "integer")
        if (allocated(error)) return
        call check(error, box%to_string(), "7")
        if (allocated(error)) return

        box = new_value(2.5_wp)
        select type (stored => box%item)
        type is (RealValue)
            call check(error, stored%value, 2.5_wp)
        class default
            call check(error, .false., "expected RealValue")
        end select
        if (allocated(error)) return

        box = new_value(.true.)
        select type (stored => box%item)
        type is (LogicalValue)
            call check(error, stored%value, .true.)
        class default
            call check(error, .false., "expected LogicalValue")
        end select
    end subroutine test_scalar_values

    subroutine test_deep_clone(error)
        type(error_type), allocatable, intent(out) :: error
        type(ValueBox) :: original, copied

        original = new_value("before")
        copied = original%clone()
        select type (stored => original%item)
        type is (StringValue)
            stored%value = "after"
        class default
            call check(error, .false., "expected StringValue")
            return
        end select

        call check(error, original%to_string(), "'after'")
        if (allocated(error)) return
        call check(error, copied%to_string(), "'before'")
    end subroutine test_deep_clone

    subroutine test_typed_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(ValueBox) :: box, item

        box = new_value([1_ip, 2_ip, 3_ip])
        call check(error, box%type_name(), "list")
        if (allocated(error)) return
        call check(error, box%to_string(), "[1, 2, 3]")
        if (allocated(error)) return

        select type (list => box%item)
        type is (ListValue)
            call check(error, list%item_type(), "integer")
            if (allocated(error)) return
            call check(error, list%size(), 3)
            if (allocated(error)) return
            item = list%get(2)
        class default
            call check(error, .false., "expected ListValue")
            return
        end select

        select type (stored => item%item)
        type is (IntegerValue)
            call check(error, stored%value, 2_ip)
        class default
            call check(error, .false., "expected IntegerValue")
        end select
    end subroutine test_typed_list

    subroutine test_empty_typed_list(error)
        type(error_type), allocatable, intent(out) :: error
        type(ValueBox) :: box
        character(len=1) :: empty(0)

        box = new_value(empty)
        select type (list => box%item)
        type is (ListValue)
            call check(error, list%size(), 0)
            if (allocated(error)) return
            call check(error, list%item_type(), "string")
        class default
            call check(error, .false., "expected ListValue")
        end select
    end subroutine test_empty_typed_list

end module test_values
