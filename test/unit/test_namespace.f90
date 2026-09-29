module test_namespace
    ! Import the public facade here so every build system also verifies that
    ! the Phase 1 foundation is usable without internal module names.
    use fclap, only : Namespace, ip, wp, FCLAP_OK, &
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
        character(len=32) :: string_value
        integer(ip) :: integer_value
        real(wp) :: real_value
        logical :: logical_value
        integer :: stat

        call args%set("name", "fclap")
        call args%set("threads", 4_ip)
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
        call check(error, integer_value, 4_ip)
        if (allocated(error)) return
        call args%get("ratio", real_value, stat)
        call check(error, real_value, 1.25_wp)
        if (allocated(error)) return
        call args%get("enabled", logical_value, stat)
        call check(error, logical_value, .true.)
        if (allocated(error)) return

        call args%set("threads", 8_ip)
        call check(error, args%size(), 4)
        if (allocated(error)) return
        call args%get("threads", integer_value, stat)
        call check(error, integer_value, 8_ip)
    end subroutine test_scalar_round_trip

    subroutine test_list_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        character(len=:), allocatable :: names(:)
        integer(ip), allocatable :: numbers(:)
        real(wp), allocatable :: reals(:)
        logical, allocatable :: flags(:)
        integer :: stat

        call args%set("names", [character(len=5) :: "alpha", "beta"])
        call args%set("numbers", [1_ip, 2_ip, 3_ip])
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
        call check(error, all(numbers == [1_ip, 2_ip, 3_ip]), .true.)
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
        integer(ip), allocatable :: values(:)
        integer :: stat

        call args%append("ids", 2_ip, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call args%append("ids", 5_ip, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call args%get("ids", values, stat)
        call check(error, all(values == [2_ip, 5_ip]), .true.)
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
        integer(ip) :: value
        integer :: stat

        value = 91_ip
        call args%get("absent", value, stat)
        call check(error, stat, ERR_NAMESPACE_MISSING_KEY)
        if (allocated(error)) return
        call check(error, value, 91_ip)
    end subroutine test_missing_key

    subroutine test_type_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        integer(ip) :: value
        integer :: stat

        call args%set("mode", "fast")
        value = 73_ip
        call args%get("mode", value, stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call check(error, value, 73_ip)
    end subroutine test_type_mismatch

    subroutine test_rank_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        integer(ip), allocatable :: list(:)
        integer(ip) :: scalar
        integer :: stat

        call args%set("scalar", 1_ip)
        allocate(list(1), source=99_ip)
        call args%get("scalar", list, stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call check(error, list(1), 99_ip)
        if (allocated(error)) return

        call args%set("list", [1_ip, 2_ip])
        scalar = 99_ip
        call args%get("list", scalar, stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call check(error, scalar, 99_ip)
    end subroutine test_rank_mismatch

    subroutine test_empty_list_type(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        character(len=1) :: empty(0)
        character(len=:), allocatable :: strings(:)
        integer(ip), allocatable :: integers(:)
        integer :: stat

        call args%set("empty", empty)
        call args%get("empty", strings, stat)
        call check(error, stat, FCLAP_OK)
        if (allocated(error)) return
        call check(error, size(strings), 0)
        if (allocated(error)) return

        allocate(integers(1), source=42_ip)
        call args%get("empty", integers, stat)
        call check(error, stat, ERR_NAMESPACE_TYPE_MISMATCH)
        if (allocated(error)) return
        call check(error, integers(1), 42_ip)
    end subroutine test_empty_list_type

    subroutine test_merge(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: parent, child
        character(len=16) :: name
        integer(ip) :: level
        integer :: stat

        call parent%set("name", "parent")
        call child%set("name", "child")
        call child%set("level", 2_ip)

        call parent%merge(child, overwrite=.false.)
        call parent%get("name", name, stat)
        call check(error, trim(name), "parent")
        if (allocated(error)) return
        call parent%get("level", level, stat)
        call check(error, level, 2_ip)
        if (allocated(error)) return

        ! The earlier merge is a deep copy, and the default policy replaces
        ! conflicting entries with the incoming namespace value.
        call child%set("level", 3_ip)
        call parent%get("level", level, stat)
        call check(error, level, 2_ip)
        if (allocated(error)) return
        call parent%merge(child)
        call parent%get("name", name, stat)
        call check(error, trim(name), "child")
        if (allocated(error)) return
        call parent%get("level", level, stat)
        call check(error, level, 3_ip)
    end subroutine test_merge

    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args

        call args%set("value", 1_ip)
        call args%clear()
        call check(error, args%size(), 0)
        if (allocated(error)) return
        call check(error, args%contains("value"), .false.)
    end subroutine test_clear

end module test_namespace
