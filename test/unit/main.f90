program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use test_actions, only : collect_actions
    use test_cli_syntax, only : collect_cli_syntax
    use test_conversion, only : collect_conversion
    use test_delimiters, only : collect_delimiters
    use test_environment, only : collect_environment
    use test_errors, only : collect_errors
    use test_formatter, only : collect_formatter
    use test_groups, only : collect_groups
    use test_help_options, only : collect_help_options
    use test_known_args, only : collect_known_args
    use test_namespace, only : collect_namespace
    use test_nargs, only : collect_nargs
    use test_parse_actions, only : collect_parse_actions
    use test_parse_engine, only : collect_parse_engine
    use test_parse_nargs, only : collect_parse_nargs
    use test_registration, only : collect_registration
    use test_rules, only : collect_rules
    use test_subparsers, only : collect_subparsers
    use test_suggestions, only : collect_suggestions
    use test_usability, only : collect_usability
    use test_validators, only : collect_validators
    use test_values, only : collect_values
    use testdrive, only : new_testsuite, run_testsuite, testsuite_type
    implicit none

    type(testsuite_type), allocatable :: suites(:)
    integer :: index, stat

    stat = 0
    suites = [ &
        new_testsuite("nargs", collect_nargs), &
        new_testsuite("errors", collect_errors), &
        new_testsuite("values", collect_values), &
        new_testsuite("namespace", collect_namespace), &
        new_testsuite("registration", collect_registration), &
        new_testsuite("actions", collect_actions), &
        new_testsuite("parse engine", collect_parse_engine), &
        new_testsuite("parse nargs", collect_parse_nargs), &
        new_testsuite("parse actions", collect_parse_actions), &
        new_testsuite("validators", collect_validators), &
        new_testsuite("formatter", collect_formatter), &
        new_testsuite("groups", collect_groups), &
        new_testsuite("subparsers", collect_subparsers), &
        new_testsuite("conversion", collect_conversion), &
        new_testsuite("cli syntax", collect_cli_syntax), &
        new_testsuite("usability", collect_usability), &
        new_testsuite("suggestions", collect_suggestions), &
        new_testsuite("rules", collect_rules), &
        new_testsuite("environment", collect_environment), &
        new_testsuite("delimiters", collect_delimiters), &
        new_testsuite("known args", collect_known_args), &
        new_testsuite("help options", collect_help_options) &
    ]

    do index = 1, size(suites)
        write(error_unit, "(a, 1x, a)") "Testing:", suites(index)%name
        call run_testsuite(suites(index)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, "(i0, 1x, a)") stat, "test(s) failed"
        error stop 1
    end if
end program tester
