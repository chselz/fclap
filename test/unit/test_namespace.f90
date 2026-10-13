module test_namespace
    ! Import the public facade here so every build system also verifies that
    ! the Phase 1 foundation is usable without internal module names.
    use, intrinsic :: iso_fortran_env, only : wp => real64
    use fclap, only : Namespace, FCLAP_OK, &
        ERR_NAMESPACE_MISSING_KEY, ERR_NAMESPACE_TYPE_MISMATCH
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_namespace

contains

    subroutine collect_namespace(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("scalar round trip", test_scalar_round_trip), &
            new_unittest("list round trip", test_list_round_trip), &
            new_unittest("append", test_append), &
            new_unittest("missing key result", test_missing_key), &
            new_unittest("type mismatch result", test_type_mismatch), &
            new_unittest("rank mismatch result", test_rank_mismatch), &
            new_unittest("empty list keeps type", test_empty_list_type), &
            new_unittest("deep merge precedence", test_merge), &
            new_unittest("clear", test_clear) &
        ]
    end subroutine collect_namespace

    subroutine test_scalar_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        character(len=:), allocatable :: string_value
        integer :: integer_value
        real(wp) :: real_value
        logical :: logical_value
        integer :: stat

        call args%set("name", "fclap")
        call args%set("threads", 4)
        call args%set("ratio", 1.25_wp)
        call args%set("enabled", .true.)

        call check(error, args%size(), 4)
        if (allocated(error)) return
        call check(error, args%contains("threads"), .true.)
        if (allocated(error)) return

        string_value = "unchanged"
        call args%get("name", string_value, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, trim(string_value), "fclap")
        if (allocated(error)) return

        call args%get("threads", integer_value, stat)
        call check(error, integer_value, 4)
        if (allocated(error)) return
        call args%get("ratio", real_value, stat)
        call check(error, real_value, 1.25_wp)
        if (allocated(error)) return
        call args%get("enabled", logical_value, stat)
        call check(error, logical_value, .true.)
        if (allocated(error)) return

        call args%set("threads", 8)
        call check(error, args%size(), 4)
        if (allocated(error)) return
        call args%get("threads", integer_value, stat)
        call check(error, integer_value, 8)
    end subroutine test_scalar_round_trip

    subroutine test_list_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        character(len=:), allocatable :: names(:)
        integer, allocatable :: numbers(:)
        real(wp), allocatable :: reals(:)
        logical, allocatable :: flags(:)
        integer :: stat

        call args%set("names", [character(len=5) :: "alpha", "beta"])
        call args%set("numbers", [1, 2, 3])
        call args%set("reals", [1.0_wp, 2.0_wp])
        call args%set("flags", [.true., .false.])

        call args%get("names", names, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(names), 2)
        if (allocated(error)) return
        call check(error, trim(names(2)), "beta")
        if (allocated(error)) return

        call args%get("numbers", numbers, stat)
        call check(error, all(numbers == [1, 2, 3]), .true.)
        if (allocated(error)) return
        call args%get("reals", reals, stat)
        call check(error, all(reals == [1.0_wp, 2.0_wp]), .true.)
        if (allocated(error)) return
        call args%get("flags", flags, stat)
        call check(error, all(flags .eqv. [.true., .false.]), .true.)
    end subroutine test_list_round_trip

    subroutine test_append(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        integer, allocatable :: values(:)
        integer :: stat

        call args%append("ids", 2, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call args%append("ids", 5, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call args%get("ids", values, stat)
        call check(error, all(values == [2, 5]), .true.)
        if (allocated(error)) return

        call args%append("ids", "wrong", stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call args%get("ids", values, stat)
        call check(error, size(values), 2)
    end subroutine test_append

    ! These expected failures pass only when the API reports the exact status
    ! and leaves caller-owned output unchanged.
    subroutine test_missing_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        integer :: value
        integer :: stat

        value = 91
        call args%get("absent", value, stat)
        call check(error, stat, ERR_NAMESPACE_MISSING_KEY)
        if (allocated(error)) return
        call check(error, value, 91)
    end subroutine test_missing_key

    subroutine test_type_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        integer :: value
        integer :: stat

        call args%set("mode", "fast")
        value = 73
        call args%get("mode", value, stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call check(error, value, 73)
    end subroutine test_type_mismatch

    subroutine test_rank_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        integer, allocatable :: list(:)
        integer :: scalar
        integer :: stat

        call args%set("scalar", 1)
        allocate(list(1), source=99)
        call args%get("scalar", list, stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call check(error, list(1), 99)
        if (allocated(error)) return

        call args%set("list", [1, 2])
        scalar = 99
        call args%get("list", scalar, stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call check(error, scalar, 99)
    end subroutine test_rank_mismatch

    subroutine test_empty_list_type(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        character(len=1) :: empty(0)
        character(len=:), allocatable :: strings(:)
        integer, allocatable :: integers(:)
        integer :: stat

        call args%set("empty", empty)
        call args%get("empty", strings, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(strings), 0)
        if (allocated(error)) return

        allocate(integers(1), source=42)
        call args%get("empty", integers, stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call check(error, integers(1), 42)
    end subroutine test_empty_list_type

    subroutine test_merge(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: parent, child
        character(len=:), allocatable :: name
        integer :: level
        integer :: stat

        call parent%set("name", "parent")
        call child%set("name", "child")
        call child%set("level", 2)

        call parent%merge(child, overwrite=.false.)
        call parent%get("name", name, stat)
        call check(error, trim(name), "parent")
        if (allocated(error)) return
        call parent%get("level", level, stat)
        call check(error, level, 2)
        if (allocated(error)) return

        ! The earlier merge is a deep copy, and the default policy replaces
        ! conflicting entries with the incoming namespace value.
        call child%set("level", 3)
        call parent%get("level", level, stat)
        call check(error, level, 2)
        if (allocated(error)) return
        call parent%merge(child)
        call parent%get("name", name, stat)
        call check(error, trim(name), "child")
        if (allocated(error)) return
        call parent%get("level", level, stat)
        call check(error, level, 3)
    end subroutine test_merge

    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args

        call args%set("value", 1)
        call args%clear()
        call check(error, args%size(), 0)
        if (allocated(error)) return
        call check(error, args%contains("value"), .false.)
    end subroutine test_clear

end module test_namespace
