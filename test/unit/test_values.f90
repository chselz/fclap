module test_values
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_positive_inf, &
        ieee_negative_inf, ieee_quiet_nan
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
            new_unittest("empty typed list", test_empty_typed_list), &
            new_unittest("real text form", test_real_text) &
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
            call check(error, int(stored%value, kind=ip), 2_ip)
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

    subroutine test_real_text(error)
        type(error_type), allocatable, intent(out) :: error
        real(wp) :: special

        call check_real_text(error, 0.0_wp, "0.0")
        if (allocated(error)) return
        call check_real_text(error, 1.0_wp, "1.0")
        if (allocated(error)) return
        call check_real_text(error, 0.1_wp, "0.1")
        if (allocated(error)) return
        call check_real_text(error, -2.5_wp, "-2.5")
        if (allocated(error)) return
        call check_real_text(error, 0.1_wp + 0.2_wp, "0.30000000000000004")
        if (allocated(error)) return
        call check_real_text(error, 1.0_wp / 3.0_wp, "0.3333333333333333")
        if (allocated(error)) return
        call check_real_text(error, 1.0e-3_wp, "0.001")
        if (allocated(error)) return
        call check_real_text(error, 999999.5_wp, "999999.5")
        if (allocated(error)) return

        call check_real_text(error, 1.0e-6_wp, "1e-6")
        if (allocated(error)) return
        call check_real_text(error, -1.5e-4_wp, "-1.5e-4")
        if (allocated(error)) return
        call check_real_text(error, 1.0e6_wp, "1e6")
        if (allocated(error)) return
        call check_real_text(error, 2.5e10_wp, "2.5e10")
        if (allocated(error)) return
        call check_real_text(error, huge(1.0_wp), "1.7976931348623157e308")
        if (allocated(error)) return
        call check_real_text(error, tiny(1.0_wp), "2.2250738585072014e-308")
        if (allocated(error)) return

        special = ieee_value(special, ieee_positive_inf)
        call check(error, real_text(special), "inf")
        if (allocated(error)) return
        special = ieee_value(special, ieee_negative_inf)
        call check(error, real_text(special), "-inf")
        if (allocated(error)) return
        special = ieee_value(special, ieee_quiet_nan)
        call check(error, real_text(special), "nan")
    end subroutine test_real_text

    !> Check the text of a real and that it reads back to the same value.
    subroutine check_real_text(error, value, expected)
        type(error_type), allocatable, intent(out) :: error
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: expected
        real(wp) :: back
        integer :: io_status

        call check(error, real_text(value), expected)
        if (allocated(error)) return
        read(expected, *, iostat=io_status) back
        call check(error, io_status == 0 .and. back == value, &
            "'" // expected // "' does not read back to the same value")
    end subroutine check_real_text

    function real_text(value) result(text)
        real(wp), intent(in) :: value
        character(len=:), allocatable :: text
        type(ValueBox) :: box

        box = new_value(value)
        text = box%to_string()
    end function real_text

end module test_values
