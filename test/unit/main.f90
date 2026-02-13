!> Unit tests for fclap argument parser
program tester
    use fclap
    implicit none

    logical :: all_passed
    logical, parameter :: RUN_DEFERRED_FAILURE_TESTS = .false.
    
    all_passed = .true.

    call test_basic_parsing(all_passed)
    call test_store_true_false(all_passed)
    call test_count_action(all_passed)
    call test_default_values(all_passed)
    call test_default_roundtrip_types(all_passed)
    call test_help_generation(all_passed)
    call test_help_default_display(all_passed)
    call test_help_choices_display(all_passed)
    call test_print_default_matrix(all_passed)
    call test_print_choices_matrix(all_passed)
    call test_choices_format_by_type(all_passed)
    call test_real_default_format_edges(all_passed)
    call test_wp_real_precision_display(all_passed)
    call test_real_get_overload_compat(all_passed)
    call test_mismatched_default_rejected(all_passed)
    call test_type_conversion(all_passed)
    call test_append(all_passed)
    call test_nargs(all_passed)
    call test_deprecated_warning(all_passed)
    call test_hidden_arguments(all_passed)
    call test_mutex_groups(all_passed)
    call test_parent_parsers(all_passed)
    call test_argument_groups(all_passed)
    call test_subparsers(all_passed)

    !> The following tests have to be activated/revised when the error handling of
    !> fclap was revised; right now there are hard exits which do not work with
    !> the current test infrastructure
    if (RUN_DEFERRED_FAILURE_TESTS) then
        call test_fail_missing_required_positional(all_passed)
        call test_fail_missing_required_optional(all_passed)
        call test_fail_option_missing_value(all_passed)
        call test_fail_invalid_int_value(all_passed)
        call test_fail_invalid_real_value(all_passed)
        call test_fail_invalid_logical_value(all_passed)
        call test_fail_invalid_choice_string(all_passed)
        call test_fail_invalid_choice_integer(all_passed)
        call test_fail_mutex_conflict(all_passed)
        call test_fail_required_mutex_missing(all_passed)
        call test_fail_not_less_than_violation(all_passed)
        call test_fail_not_bigger_than_violation(all_passed)
        call test_fail_unknown_option(all_passed)
        call test_fail_extra_positional(all_passed)
        call test_fail_unknown_subcommand(all_passed)
        call test_fail_removed_argument_used(all_passed)
        call test_fail_append_missing_value(all_passed)
    end if

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
        integer :: tmp_retries
        real(wp) :: tmp_threshold
        logical :: tmp_enabled

        print *, "Test: default values..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-o", "--output", default_val="default.txt", help="Output file")
        call parser%add_argument("-r", "--retries", data_type="int", default_val=3, &
                                 help="Number of retries")
        call parser%add_argument("-t", "--threshold", data_type="float", default_val=1.25_wp, &
                                 help="Threshold value")
        call parser%add_argument("-e", "--enabled", data_type="bool", default_val=.true., &
                                 help="Enable feature")
        call parser%add_argument("input", help="Input file")

        test_args(1) = "input.txt"

        args = parser%parse_args_array(test_args)

        call args%get("output", tmp_output)
        call args%get("retries", tmp_retries)
        call args%get("threshold", tmp_threshold)
        call args%get("enabled", tmp_enabled)

        if (tmp_output /= "default.txt") then
            print *, "  FAILED: output should be 'default.txt', got: ", trim(tmp_output)
            passed = .false.
        else if (tmp_retries /= 3) then
            print *, "  FAILED: retries should be 3, got: ", tmp_retries
            passed = .false.
        else if (abs(tmp_threshold - 1.25_wp) > 0.01_wp) then
            print *, "  FAILED: threshold should be ~1.25, got: ", tmp_threshold
            passed = .false.
        else if (.not. tmp_enabled) then
            print *, "  FAILED: enabled should be .true."
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_default_values

    !> Verifies default values roundtrip correctly for all scalar types.
    subroutine test_default_roundtrip_types(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=64) :: expected_name, got_name
        integer :: expected_count, got_count
        real(wp) :: expected_temp, got_temp
        logical :: expected_enabled, got_enabled
        character(len=1) :: empty_args(1)

        print *, "Test: default roundtrip for all types..."

        expected_name = "cpcm"
        expected_count = 7
        expected_temp = 298.15_wp
        expected_enabled = .true.

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--name", default_val=expected_name, help="Name")
        call parser%add_argument("--count", data_type="int", default_val=expected_count, &
                                 help="Count")
        call parser%add_argument("--temp", data_type="real", default_val=expected_temp, &
                                 help="Temperature")
        call parser%add_argument("--enabled", data_type="bool", default_val=expected_enabled, &
                                 help="Enabled")

        args = parser%parse_args_array(empty_args(1:0))

        call args%get("name", got_name)
        call args%get("count", got_count)
        call args%get("temp", got_temp)
        call args%get("enabled", got_enabled)

        if (trim(got_name) /= trim(expected_name)) then
            print *, "  FAILED: string default mismatch"
            passed = .false.
        else if (got_count /= expected_count) then
            print *, "  FAILED: integer default mismatch"
            passed = .false.
        else if (abs(got_temp - expected_temp) > 1.0e-12_wp) then
            print *, "  FAILED: real default mismatch"
            passed = .false.
        else if (got_enabled .neqv. expected_enabled) then
            print *, "  FAILED: logical default mismatch"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_default_roundtrip_types

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

    !> Solvation inspired test for checking that default values are displayed in help
    !> with correct formatting and that print_default toggle works
    subroutine test_help_default_display(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text

        print *, "Test: help default display toggle..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--retries", data_type="int", default_val=3, &
                                 help="Number of retries")
        call parser%add_argument("--proj-tol", data_type="real", default_val=1.0d-10, &
                                 action=not_less_than(0.0d0), metavar="REAL", &
                                 help="Projection convergence tolerance")
        call parser%add_argument("--ratio", data_type="real", default_val=0.25, &
                                 help="Ratio")
        call parser%add_argument("--scale", data_type="real", default_val=3.0, &
                                 help="Scale")
        call parser%add_argument("--token", default_val="secret", print_default=.false., &
                                 help="Auth token")
        call parser%add_argument("--plain", help="No default here")

        help_text = parser%format_help()

        if (index(help_text, "default: 3") == 0) then
            print *, "  FAILED: help should contain integer default"
            passed = .false.
        else if (index(help_text, "Projection convergence tolerance (default: 1.0E-10)") == 0) then
            print *, "  FAILED: help should contain proj-tol default"
            passed = .false.
        else if (index(help_text, "Ratio (default: 2.5E-01)") == 0) then
            print *, "  FAILED: help should format <1 real defaults exponentially"
            passed = .false.
        else if (index(help_text, "Scale (default: 3.0)") == 0) then
            print *, "  FAILED: help should format >=1 real defaults in fixed format"
            passed = .false.
        else if (index(help_text, "default: 'secret'") > 0) then
            print *, "  FAILED: help should hide default when print_default=.false."
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_help_default_display

    subroutine test_help_choices_display(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text
        character(len=:), allocatable :: expected_choices_line
        integer :: help_col

        print *, "Test: help choices display toggle..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--radii", default_val="cpcm", &
                                 choices=[character(len=5) :: "cpcm", "smd", "d3", "cosmo", "bondi"], &
                                 metavar="RADII", help="Atomic radii set", print_choices=.true.)
        call parser%add_argument("--method", default_val="r2scan", &
                                 choices=[character(len=6) :: "r2scan", "pbe"], &
                                 help="Method")

        help_text = parser%format_help()
        help_col = parser%max_help_position
        expected_choices_line = new_line('A') // repeat(" ", help_col) // &
                                "[choices: 'cpcm', 'smd', 'd3', 'cosmo', 'bondi']"

        if (index(help_text, "Atomic radii set") == 0) then
            print *, "  FAILED: help should contain radii help text"
            passed = .false.
        else if (index(help_text, expected_choices_line) == 0) then
            print *, "  FAILED: choices should be on aligned new line"
            passed = .false.
        else if (index(help_text, "[choices: r2scan, pbe]") > 0) then
            print *, "  FAILED: choices should be hidden by default"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_help_choices_display

    !> Verifies print_default combinations (shown, hidden, missing default).
    subroutine test_print_default_matrix(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text

        print *, "Test: print_default matrix..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--shown", data_type="int", default_val=11, print_default=.true., &
                                 help="Shown default")
        call parser%add_argument("--hidden", data_type="int", default_val=22, print_default=.false., &
                                 help="Hidden default")
        call parser%add_argument("--nodef", print_default=.true., &
                                 help="No default value")

        help_text = parser%format_help()

        if (index(help_text, "Shown default (default: 11)") == 0) then
            print *, "  FAILED: shown default should be printed"
            passed = .false.
        else if (index(help_text, "Hidden default (default: 22)") > 0) then
            print *, "  FAILED: hidden default should not be printed"
            passed = .false.
        else if (index(help_text, "No default value (default:") > 0) then
            print *, "  FAILED: default should not be printed when unset"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_print_default_matrix

    !> Verifies print_choices combinations (shown, hidden, and empty choices).
    subroutine test_print_choices_matrix(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text

        print *, "Test: print_choices matrix..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--shown", choices=[character(len=2) :: "a", "b"], &
                                 print_choices=.true., help="Shown choices")
        call parser%add_argument("--hidden", choices=[character(len=2) :: "c", "d"], &
                                 help="Hidden choices")
        call parser%add_argument("--empty", print_choices=.true., help="Empty choices")

        help_text = parser%format_help()

        if (index(help_text, "[choices: 'a', 'b']") == 0) then
            print *, "  FAILED: shown choices should be printed"
            passed = .false.
        else if (index(help_text, "[choices: 'c', 'd']") > 0) then
            print *, "  FAILED: hidden choices should not be printed"
            passed = .false.
        else if (index(help_text, "[choices: ]") > 0) then
            print *, "  FAILED: empty choices should not print placeholder"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_print_choices_matrix

    !> Verifies string choices are quoted while numeric choices are unquoted.
    subroutine test_choices_format_by_type(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text

        print *, "Test: choices formatting by type..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--color", choices=[character(len=4) :: "red", "blue"], &
                                 print_choices=.true., help="Color")
        call parser%add_argument("--level", data_type="int", &
                                 choices=[character(len=1) :: "1", "2"], &
                                 print_choices=.true., help="Level")

        help_text = parser%format_help()

        if (index(help_text, "[choices: 'red', 'blue']") == 0) then
            print *, "  FAILED: string choices should be quoted"
            passed = .false.
        else if (index(help_text, "[choices: 1, 2]") == 0) then
            print *, "  FAILED: integer choices should be unquoted"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_choices_format_by_type

    !> Verifies edge-case formatting for real defaults.
    subroutine test_real_default_format_edges(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: help_text

        print *, "Test: real default formatting edges..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--zero", data_type="real", default_val=0.0_wp, help="Zero")
        call parser%add_argument("--one", data_type="real", default_val=1.0_wp, help="One")
        call parser%add_argument("--small", data_type="real", default_val=0.25_wp, help="Small")
        call parser%add_argument("--neg-small", data_type="real", default_val=-0.25_wp, &
                                 help="Neg small")
        call parser%add_argument("--trim", data_type="real", default_val=3.140000_wp, help="Trim")
        call parser%add_argument("--huge", data_type="real", default_val=1.0e9_wp, help="Huge")

        help_text = parser%format_help()

        if (index(help_text, "Zero (default: 0.0)") == 0) then
            print *, "  FAILED: zero should render as 0.0"
            passed = .false.
        else if (index(help_text, "One (default: 1.0)") == 0) then
            print *, "  FAILED: one should render as 1.0"
            passed = .false.
        else if (index(help_text, "Small (default: 2.5E-01)") == 0) then
            print *, "  FAILED: small should render in exponential format"
            passed = .false.
        else if (index(help_text, "Neg small (default: -2.5E-01)") == 0) then
            print *, "  FAILED: negative small should render in exponential format"
            passed = .false.
        else if (index(help_text, "Trim (default: 3.14)") == 0) then
            print *, "  FAILED: fixed format should trim trailing zeros"
            passed = .false.
        else if (index(help_text, "Huge (default: 1.0E+09)") == 0) then
            print *, "  FAILED: very large values should render in exponential format"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_real_default_format_edges

    !> Verifies wp defaults avoid single-precision display artifacts.
    subroutine test_wp_real_precision_display(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: empty_args(1)
        character(len=:), allocatable :: help_text
        real(wp) :: tmp_temp

        print *, "Test: wp real precision display..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--temp", data_type="real", default_val=298.15_wp, &
                                 help="Temperature")

        help_text = parser%format_help()
        args = parser%parse_args_array(empty_args(1:0))
        call args%get("temp", tmp_temp)

        if (index(help_text, "Temperature (default: 298.15)") == 0) then
            print *, "  FAILED: expected clean wp default in help"
            passed = .false.
        else if (index(help_text, "298.149994") > 0) then
            print *, "  FAILED: should not show single-precision artifacts"
            passed = .false.
        else if (abs(tmp_temp - 298.15_wp) > 1.0e-12_wp) then
            print *, "  FAILED: parsed wp default should preserve precision"
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_wp_real_precision_display

    !> Verifies real getter overloads for both real and real(wp).
    subroutine test_real_get_overload_compat(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(2)
        real :: tmp_sp
        real(wp) :: tmp_wp
        real(wp) :: tol_wp, tol_sp

        print *, "Test: real get overload compatibility..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-f", "--factor", data_type="float", help="Factor")

        test_args(1) = "-f"
        test_args(2) = "4.5"
        args = parser%parse_args_array(test_args)

        call args%get("factor", tmp_sp)
        call args%get("factor", tmp_wp)
        call args%get("missing", tmp_sp, default=2.5)
        call args%get("missing", tmp_wp, default=2.5_wp)

        tol_wp = 100.0_wp * epsilon(1.0_wp)
        tol_sp = 100.0_wp * real(epsilon(1.0), kind=wp)

        if (abs(real(tmp_sp, kind=wp) - 2.5_wp) > tol_sp) then
            print *, "  FAILED: missing real default (sp) should be 2.5"
            passed = .false.
        else if (abs(tmp_wp - 2.5_wp) > tol_wp) then
            print *, "  FAILED: missing real default (wp) should be 2.5"
            passed = .false.
        else
            call args%get("factor", tmp_sp)
            call args%get("factor", tmp_wp)
            if (abs(real(tmp_sp, kind=wp) - 4.5_wp) > tol_sp) then
                print *, "  FAILED: factor (sp) should be ~4.5"
                passed = .false.
            else if (abs(tmp_wp - 4.5_wp) > tol_wp) then
                print *, "  FAILED: factor (wp) should be ~4.5"
                passed = .false.
            else
                print *, "  PASSED"
            end if
        end if
    end subroutine test_real_get_overload_compat

    !> Verifies mismatched defaults are rejected with parser errors.
    subroutine test_mismatched_default_rejected(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        character(len=:), allocatable :: err

        print *, "Test: mismatched default rejection..."

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--ival", data_type="int", default_val=1.5_wp, help="Integer")
        if (.not. parser%last_error%has_error) then
            print *, "  FAILED: int argument should reject real default"
            passed = .false.
            return
        end if
        err = parser%last_error%message
        if (index(err, "invalid default") == 0) then
            print *, "  FAILED: rejection should set default-related parser error"
            passed = .false.
            return
        end if
        if (parser%num_actions /= 0) then
            print *, "  FAILED: rejected argument should not be registered"
            passed = .false.
            return
        end if

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--rval", data_type="real", default_val=2, help="Real")
        if (.not. parser%last_error%has_error) then
            print *, "  FAILED: real argument should reject integer default"
            passed = .false.
            return
        end if

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--flag", data_type="bool", default_val=1, help="Flag")
        if (.not. parser%last_error%has_error) then
            print *, "  FAILED: logical argument should reject integer default"
            passed = .false.
            return
        end if

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--count", data_type="int", default_val="abc", help="Count")
        if (.not. parser%last_error%has_error) then
            print *, "  FAILED: int argument should reject non-numeric string default"
            passed = .false.
            return
        end if

        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--valid", data_type="real", default_val=2.5_wp, help="Valid")
        if (parser%last_error%has_error) then
            print *, "  FAILED: valid default should not set parser error"
            passed = .false.
            return
        end if

        print *, "  PASSED"
    end subroutine test_mismatched_default_rejected

    subroutine test_type_conversion(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: test_args(4)
        integer :: tmp_number
        real(wp) :: tmp_factor
        real :: tmp_factor_sp

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
        call args%get("factor", tmp_factor_sp)

        if (tmp_number /= 42) then
            print *, "  FAILED: number should be 42, got: ", tmp_number
            passed = .false.
        else if (abs(tmp_factor - 3.14_wp) > 0.01_wp) then
            print *, "  FAILED: factor should be ~3.14, got: ", tmp_factor
            passed = .false.
        else if (abs(real(tmp_factor_sp, kind=wp) - 3.14_wp) > 0.01_wp) then
            print *, "  FAILED: factor (sp get) should be ~3.14, got: ", tmp_factor_sp
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine test_type_conversion

    subroutine test_append(passed)
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
    end subroutine test_append

    subroutine test_nargs(passed)
        logical, intent(in) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
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

    subroutine test_subparsers(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser, sub_a, sub_b
        type(Namespace) :: args
        character(len=256) :: test_args(4)
        character(len=256) :: tmp_cmd, tmp_url, tmp_branch, tmp_msg
        character(len=:), allocatable :: help_text
        integer :: tmp_bar
        logical :: tmp_foo

        print *, "Test: subparser per-command arguments..."

        ! --- Test 1: subparser with positional + optional args ---
        call parser%init(prog="PROG", add_help=.false.)
        call parser%add_argument("--foo", action="store_true", help="foo help")
        call parser%add_subparsers(dest="command")

        call sub_a%init(prog="PROG a", add_help=.false.)
        call sub_a%add_argument("bar", data_type="int", help="bar help")
        call parser%add_parser("a", sub_a, help_text="a help")

        call sub_b%init(prog="PROG b", add_help=.false.)
        call sub_b%add_argument("--baz", help="baz help")
        call parser%add_parser("b", sub_b, help_text="b help")

        ! Test: PROG a 12
        test_args(1) = "a"
        test_args(2) = "12"
        args = parser%parse_args_array(test_args(1:2))

        call args%get("command", tmp_cmd)
        call args%get("bar", tmp_bar)
        call args%get("foo", tmp_foo)

        if (trim(tmp_cmd) /= "a") then
            print *, "  FAILED: command should be 'a', got: ", trim(tmp_cmd)
            passed = .false.
            return
        end if
        if (tmp_bar /= 12) then
            print *, "  FAILED: bar should be 12, got: ", tmp_bar
            passed = .false.
            return
        end if
        if (tmp_foo) then
            print *, "  FAILED: foo should be .false."
            passed = .false.
            return
        end if

        ! Test: PROG --foo b --baz Z
        test_args(1) = "--foo"
        test_args(2) = "b"
        test_args(3) = "--baz"
        test_args(4) = "Z"
        args = parser%parse_args_array(test_args(1:4))

        call args%get("command", tmp_cmd)
        call args%get("foo", tmp_foo)
        call args%get("baz", tmp_msg)

        if (trim(tmp_cmd) /= "b") then
            print *, "  FAILED: command should be 'b', got: ", trim(tmp_cmd)
            passed = .false.
            return
        end if
        if (.not. tmp_foo) then
            print *, "  FAILED: foo should be .true."
            passed = .false.
            return
        end if
        if (trim(tmp_msg) /= "Z") then
            print *, "  FAILED: baz should be 'Z', got: ", trim(tmp_msg)
            passed = .false.
            return
        end if

        ! --- Test 2: help text shows subparser names ---
        help_text = parser%format_help()
        if (index(help_text, "a") == 0 .or. index(help_text, "b") == 0) then
            print *, "  FAILED: help should list subcommands 'a' and 'b'"
            passed = .false.
            return
        end if

        print *, "  PASSED"
    end subroutine test_subparsers

    !* ================================================================================ *!
    !*                   The following tests have to be added when the                  *!
    !*                   error handling in fclap is revised so there is                 *!
    !*                               no hard exit anymore                               *!
    !* ================================================================================ *!

    !> Helper check for deferred expected-failure tests.
    subroutine assert_expected_failure(parser, passed, context)
        type(ArgumentParser), intent(in) :: parser
        logical, intent(inout) :: passed
        character(len=*), intent(in) :: context

        if (.not. parser%last_error%has_error) then
            print *, "  FAILED: expected parser error for ", trim(context)
            passed = .false.
        else
            print *, "  PASSED"
        end if
    end subroutine assert_expected_failure

    !> Fails when required positional argument is missing.
    subroutine test_fail_missing_required_positional(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=1) :: empty_args(1)

        print *, "Deferred fail test: missing required positional..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("input", help="Input")
        args = parser%parse_args_array(empty_args(1:0))
        call assert_expected_failure(parser, passed, "missing required positional")
    end subroutine test_fail_missing_required_positional

    !> Fails when required optional argument is missing.
    subroutine test_fail_missing_required_optional(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=1) :: empty_args(1)

        print *, "Deferred fail test: missing required optional..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--output", required=.true., help="Output")
        args = parser%parse_args_array(empty_args(1:0))
        call assert_expected_failure(parser, passed, "missing required optional")
    end subroutine test_fail_missing_required_optional

    !> Fails when option value is missing.
    subroutine test_fail_option_missing_value(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(1)

        print *, "Deferred fail test: option missing value..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--output", help="Output")
        bad_args(1) = "--output"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "option missing value")
    end subroutine test_fail_option_missing_value

    !> Fails when integer argument receives non-integer input.
    subroutine test_fail_invalid_int_value(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)

        print *, "Deferred fail test: invalid integer value..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--n", data_type="int", help="Count")
        bad_args(1) = "--n"
        bad_args(2) = "abc"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "invalid integer value")
    end subroutine test_fail_invalid_int_value

    !> Fails when real argument receives non-real input.
    subroutine test_fail_invalid_real_value(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)

        print *, "Deferred fail test: invalid real value..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--x", data_type="real", help="Real value")
        bad_args(1) = "--x"
        bad_args(2) = "abc"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "invalid real value")
    end subroutine test_fail_invalid_real_value

    !> Fails when logical argument receives invalid token.
    subroutine test_fail_invalid_logical_value(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)

        print *, "Deferred fail test: invalid logical value..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--flag", data_type="bool", help="Flag")
        bad_args(1) = "--flag"
        bad_args(2) = "maybe"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "invalid logical value")
    end subroutine test_fail_invalid_logical_value

    !> Fails when string argument receives disallowed choice.
    subroutine test_fail_invalid_choice_string(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)

        print *, "Deferred fail test: invalid string choice..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--radii", choices=[character(len=5) :: "cpcm", "bondi"], help="Radii")
        bad_args(1) = "--radii"
        bad_args(2) = "smd"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "invalid string choice")
    end subroutine test_fail_invalid_choice_string

    !> Fails when integer argument receives value outside allowed choices.
    subroutine test_fail_invalid_choice_integer(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)

        print *, "Deferred fail test: invalid integer choice..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--level", data_type="int", &
                                 choices=[character(len=1) :: "1", "2"], help="Level")
        bad_args(1) = "--level"
        bad_args(2) = "3"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "invalid integer choice")
    end subroutine test_fail_invalid_choice_integer

    !> Fails when both options in mutex group are provided.
    subroutine test_fail_mutex_conflict(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)
        integer :: mutex_idx

        print *, "Deferred fail test: mutex conflict..."
        call parser%init(prog="test_prog", add_help=.false.)
        mutex_idx = parser%add_mutually_exclusive_group(required=.false.)
        call parser%add_argument("--foo", action="store_true", mutex_group_idx=mutex_idx)
        call parser%add_argument("--bar", action="store_true", mutex_group_idx=mutex_idx)
        bad_args(1) = "--foo"
        bad_args(2) = "--bar"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "mutex conflict")
    end subroutine test_fail_mutex_conflict

    !> Fails when required mutex group has no selected option.
    subroutine test_fail_required_mutex_missing(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=1) :: empty_args(1)
        integer :: mutex_idx

        print *, "Deferred fail test: required mutex missing..."
        call parser%init(prog="test_prog", add_help=.false.)
        mutex_idx = parser%add_mutually_exclusive_group(required=.true.)
        call parser%add_argument("--foo", action="store_true", mutex_group_idx=mutex_idx)
        call parser%add_argument("--bar", action="store_true", mutex_group_idx=mutex_idx)
        args = parser%parse_args_array(empty_args(1:0))
        call assert_expected_failure(parser, passed, "required mutex missing")
    end subroutine test_fail_required_mutex_missing

    !> Fails when value violates not_less_than bound.
    subroutine test_fail_not_less_than_violation(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)

        print *, "Deferred fail test: not_less_than violation..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--x", data_type="real", action=not_less_than(0.0_wp))
        bad_args(1) = "--x"
        bad_args(2) = "-0.1"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "not_less_than violation")
    end subroutine test_fail_not_less_than_violation

    !> Fails when value violates not_bigger_than bound.
    subroutine test_fail_not_bigger_than_violation(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)

        print *, "Deferred fail test: not_bigger_than violation..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--x", data_type="real", action=not_bigger_than(1.0_wp))
        bad_args(1) = "--x"
        bad_args(2) = "1.1"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "not_bigger_than violation")
    end subroutine test_fail_not_bigger_than_violation

    !> Fails when unknown option is provided.
    subroutine test_fail_unknown_option(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(1)

        print *, "Deferred fail test: unknown option..."
        call parser%init(prog="test_prog", add_help=.false.)
        bad_args(1) = "--does-not-exist"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "unknown option")
    end subroutine test_fail_unknown_option

    !> Fails when too many positional arguments are provided.
    subroutine test_fail_extra_positional(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(2)

        print *, "Deferred fail test: extra positional argument..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("input", help="Input")
        bad_args(1) = "a"
        bad_args(2) = "b"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "extra positional argument")
    end subroutine test_fail_extra_positional

    !> Fails when unknown subcommand is used.
    subroutine test_fail_unknown_subcommand(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser, sub_a
        type(Namespace) :: args
        character(len=256) :: bad_args(1)

        print *, "Deferred fail test: unknown subcommand..."
        call parser%init(prog="PROG", add_help=.false.)
        call parser%add_subparsers(dest="command")
        call sub_a%init(prog="PROG a", add_help=.false.)
        call parser%add_parser("a", sub_a, help_text="a help")
        bad_args(1) = "z"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "unknown subcommand")
    end subroutine test_fail_unknown_subcommand

    !> Fails when removed argument is used.
    subroutine test_fail_removed_argument_used(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(1)

        print *, "Deferred fail test: removed argument used..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("--old", action="store_true", status=STATUS_REMOVED)
        bad_args(1) = "--old"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "removed argument used")
    end subroutine test_fail_removed_argument_used

    !> Fails when append action is missing its value.
    subroutine test_fail_append_missing_value(passed)
        logical, intent(inout) :: passed
        type(ArgumentParser) :: parser
        type(Namespace) :: args
        character(len=256) :: bad_args(1)

        print *, "Deferred fail test: append missing value..."
        call parser%init(prog="test_prog", add_help=.false.)
        call parser%add_argument("-f", action="append", help="File")
        bad_args(1) = "-f"
        args = parser%parse_args_array(bad_args)
        call assert_expected_failure(parser, passed, "append missing value")
    end subroutine test_fail_append_missing_value

end program tester
