!> Unit tests for fclap argument parser
program tester
    use fclap
    implicit none

    logical :: all_passed
    
    all_passed = .true.

    call test_basic_parsing(all_passed)
    call test_store_true_false(all_passed)
    call test_count_action(all_passed)
    call test_default_values(all_passed)
    call test_help_generation(all_passed)
    call test_type_conversion(all_passed)
    call test_nargs(all_passed)
    call test_deprecated_warning(all_passed)
    call test_hidden_arguments(all_passed)
    call test_mutex_groups(all_passed)
    call test_parent_parsers(all_passed)
    call test_argument_groups(all_passed)

    if (all_passed) then
        print *, ""
        print *, "========================================"
        print *, "ALL TESTS PASSED!"
        print *, "========================================"
    else
        print *, ""
        print *, "========================================"
        print *, "SOME TESTS FAILED!"
        print *, "========================================"
        error stop 1
    end if

contains

    subroutine test_basic_parsing(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(3)
        character(len=256) :: tmp_input, tmp_output

        print *, "Test: Basic argument parsing..."

        call parser%init(prog="test_prog", description="A test program", add_help=.false.)
        call parser%add_argument("-o", "--output", help="Output file")
        call parser%add_argument("input", help="Input file")

        test_args(1) = "input.txt"
        test_args(2) = "-o"
        test_args(3) = "result.txt"

        args = parser%parse_args_array(test_args)

        call args%get("input", tmp_input)
        call args%get("output", tmp_output)

        if (tmp_input /= "input.txt") then
            print *, "  FAILED: input should be 'input.txt', got: ", trim(tmp_input)
            passed = .false.
        else if (tmp_output /= "result.txt") then
            print *, "  FAILED: output should be 'result.txt', got: ", trim(tmp_output)
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_basic_parsing

    subroutine test_store_true_false(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(1)
        logical :: tmp_verbose, tmp_quiet

        print *, "Test: store_true and store_false actions..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-v", "--verbose", action="store_true", help="Verbose output")
        call parser%add_argument("-q", "--quiet", action="store_false", help="Quiet mode")

        test_args(1) = "-v"

        args = parser%parse_args_array(test_args)

        call args%get("verbose", tmp_verbose)
        call args%get("quiet", tmp_quiet)

        if (.not. tmp_verbose) then
            print *, "  FAILED: verbose should be .true."
            passed = .false.
        else if (.not. tmp_quiet) then
            print *, "  FAILED: quiet should be .true. (default for store_false)"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_store_true_false

    subroutine test_count_action(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(3)
        integer :: tmp_count

        print *, "Test: count action..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-v", "--verbose", action="count", help="Increase verbosity")

        test_args(1) = "-v"
        test_args(2) = "-v"
        test_args(3) = "-v"

        args = parser%parse_args_array(test_args)

        call args%get("verbose", tmp_count)

        if (tmp_count /= 3) then
            print *, "  FAILED: verbose count should be 3, got: ", tmp_count
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_count_action

    subroutine test_default_values(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(1)
        character(len=256) :: tmp_output

        print *, "Test: default values..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-o", "--output", default_val="default.txt", help="Output file")
        call parser%add_argument("input", help="Input file")

        test_args(1) = "input.txt"

        args = parser%parse_args_array(test_args)

        call args%get("output", tmp_output)

        if (tmp_output /= "default.txt") then
            print *, "  FAILED: output should be 'default.txt', got: ", trim(tmp_output)
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_default_values

    subroutine test_help_generation(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text

        print *, "Test: help text generation..."

        call parser%init(prog="my_program", description="A sample program", add_help=.false.)
        call parser%add_argument("-o", "--output", help="Output file", metavar="FILE")
        call parser%add_argument("-v", "--verbose", action="store_true", help="Verbose output")
        call parser%add_argument("input", help="Input file")

        help_text = parser%format_help()

        if (index(help_text, "my_program") == 0) then
            print *, "  FAILED: help should contain program name"
            passed = .false.
        else if (index(help_text, "A sample program") == 0) then
            print *, "  FAILED: help should contain description"
            passed = .false.
        else if (index(help_text, "--output") == 0) then
            print *, "  FAILED: help should contain --output"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_help_generation

    subroutine test_type_conversion(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(4)
        integer :: tmp_number
        real :: tmp_factor

        print *, "Test: type conversion (integer, real)..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-n", "--number", data_type="int", help="An integer")
        call parser%add_argument("-f", "--factor", data_type="float", help="A float")

        test_args(1) = "-n"
        test_args(2) = "42"
        test_args(3) = "-f"
        test_args(4) = "3.14"

        args = parser%parse_args_array(test_args)

        call args%get("number", tmp_number)
        call args%get("factor", tmp_factor)

        if (tmp_number /= 42) then
            print *, "  FAILED: number should be 42, got: ", tmp_number
            passed = .false.
        else if (abs(tmp_factor - 3.14) > 0.01) then
            print *, "  FAILED: factor should be ~3.14, got: ", tmp_factor
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_type_conversion

    subroutine test_nargs(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(5)
        character(len=256) :: files(10)
        integer :: file_count

        print *, "Test: nargs with append action..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-f", "--file", action="append", help="Add a file")

        test_args(1) = "-f"
        test_args(2) = "file1.txt"
        test_args(3) = "-f"
        test_args(4) = "file2.txt"
        test_args(5) = "-f"

        ! Only parse first 4 to avoid needing value for last -f
        args = parser%parse_args_array(test_args(1:4))

        call args%get_string_list("file", files, file_count)

        if (file_count /= 2) then
            print *, "  FAILED: should have 2 files, got: ", file_count
            passed = .false.
        else if (trim(files(1)) /= "file1.txt" .or. trim(files(2)) /= "file2.txt") then
            print *, "  FAILED: files don't match"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_nargs

    subroutine test_deprecated_warning(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(2)
        character(len=256) :: tmp_old_option

        print *, "Test: deprecated argument warning..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-o", "--old-option", status=STATUS_DEPRECATED, &
                                 help="Old option", deprecated_msg="Use --new-option instead")
        call parser%add_argument("-n", "--new-option", help="New option")

        test_args(1) = "-o"
        test_args(2) = "value"

        ! This should print a warning but still work
        print *, "  (Expected warning below:)"
        args = parser%parse_args_array(test_args)

        call args%get("old_option", tmp_old_option)

        if (tmp_old_option /= "value") then
            print *, "  FAILED: deprecated option should still work"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_deprecated_warning

    subroutine test_hidden_arguments(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=:), allocatable :: help_text
        character(len=256) :: test_args(2)
        character(len=256) :: tmp_hidden

        print *, "Test: hidden arguments..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-v", "--visible", help="Visible option")
        call parser%add_argument("-H", "--hidden", visible=.false., help="Hidden option")

        help_text = parser%format_help()

        ! Check that visible option appears in help
        if (index(help_text, "--visible") == 0) then
            print *, "  FAILED: visible option should appear in help"
            passed = .false.
            return
        end if

        ! Check that hidden option does NOT appear in help
        if (index(help_text, "--hidden") > 0) then
            print *, "  FAILED: hidden option should NOT appear in help"
            passed = .false.
            return
        end if

        ! But hidden option should still work when used
        test_args(1) = "-H"
        test_args(2) = "secret"

        args = parser%parse_args_array(test_args)

        call args%get("hidden", tmp_hidden)

        if (tmp_hidden /= "secret") then
            print *, "  FAILED: hidden option should still work"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_hidden_arguments

    subroutine test_mutex_groups(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(1)
        integer :: mutex_idx
        logical :: tmp_foo, tmp_bar

        print *, "Test: mutually exclusive groups..."

        call parser%init(prog="test_prog", add_help=.false.)
        
        ! Create a mutex group
        mutex_idx = parser%add_mutually_exclusive_group(required=.false.)
        
        ! Add mutually exclusive options
        call parser%add_argument("--foo", action="store_true", help="Foo option", mutex_group_idx=mutex_idx)
        call parser%add_argument("--bar", action="store_true", help="Bar option", mutex_group_idx=mutex_idx)

        ! Test with only one option (should work)
        test_args(1) = "--foo"
        args = parser%parse_args_array(test_args)

        call args%get("foo", tmp_foo)
        call args%get("bar", tmp_bar)

        if (.not. tmp_foo) then
            print *, "  FAILED: --foo should be true"
            passed = .false.
        else if (tmp_bar) then
            print *, "  FAILED: --bar should be false"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_mutex_groups

    subroutine test_parent_parsers(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parent, child
        type(Namespace) :: args
        character(len=256) :: test_args(4)
        character(len=256) :: tmp_parent_opt, tmp_child_opt

        print *, "Test: parent parser inheritance..."

        ! Create parent parser (without help to avoid conflicts)
        call parent%init(prog="parent", add_help=.false.)
        call parent%add_argument("--parent-opt", help="Option from parent", default_val="default")

        ! Create child parser with parent
        call child%init_with_parents([parent], prog="child", add_help=.false.)
        call child%add_argument("--child-opt", help="Option from child")

        ! Test that child has both options
        test_args(1) = "--parent-opt"
        test_args(2) = "from_parent"
        test_args(3) = "--child-opt"
        test_args(4) = "from_child"

        args = child%parse_args_array(test_args)

        call args%get("parent_opt", tmp_parent_opt)
        call args%get("child_opt", tmp_child_opt)

        if (tmp_parent_opt /= "from_parent") then
            print *, "  FAILED: parent option should work, got: ", trim(tmp_parent_opt)
            passed = .false.
        else if (tmp_child_opt /= "from_child") then
            print *, "  FAILED: child option should work, got: ", trim(tmp_child_opt)
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_parent_parsers

    subroutine test_argument_groups(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text
        integer :: group_idx

        print *, "Test: argument groups..."

        call parser%init(prog="test_prog", add_help=.false.)
        
        ! Create an argument group
        group_idx = parser%add_argument_group("Input Options", "Options related to input files")
        
        call parser%add_argument("-i", "--input", help="Input file", group_idx=group_idx)
        call parser%add_argument("-f", "--format", help="Input format", group_idx=group_idx)

        help_text = parser%format_help()

        ! Check that group title appears in help
        if (index(help_text, "Input Options") == 0) then
            print *, "  FAILED: help should contain group title 'Input Options'"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_argument_groups

end program tester
