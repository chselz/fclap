module test_suggestions
    use fclap, only : ArgumentParser, ErrorEntry, ParseResult, PARSE_FAILURE, &
        ERR_UNKNOWN_ARGUMENT, ERR_UNKNOWN_SUBCOMMAND, ERR_INVALID_CHOICE, &
        store_true
    use fclap_utils_accuracy, only : wp
    use fclap_utils_similarity, only : jaro_similarity, closest_match
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_suggestions

contains

    subroutine collect_suggestions(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("jaro similarity", test_jaro_similarity), &
            new_unittest("closest match threshold", test_closest_match), &
            new_unittest("unknown option suggests a known option", &
                test_unknown_option), &
            new_unittest("unrelated or short options get no suggestion", &
                test_no_option_suggestion), &
            new_unittest("hidden and removed options are not suggested", &
                test_hidden_not_suggested), &
            new_unittest("unknown subcommand suggests a command", &
                test_unknown_subcommand), &
            new_unittest("invalid choice suggests a choice", &
                test_invalid_choice) &
        ]
    end subroutine collect_suggestions

    subroutine test_jaro_similarity(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, jaro_similarity("output", "output"), 1.0_wp)
        if (allocated(error)) return
        call check(error, jaro_similarity("", ""), 1.0_wp)
        if (allocated(error)) return
        call check(error, jaro_similarity("abc", ""), 0.0_wp)
        if (allocated(error)) return
        call check(error, jaro_similarity("abc", "xyz"), 0.0_wp)
        if (allocated(error)) return
        ! Reference values from Jaro's original examples.
        call check(error, jaro_similarity("MARTHA", "MARHTA"), &
            0.944444444444444_wp, thr=1.0e-12_wp)
        if (allocated(error)) return
        call check(error, jaro_similarity("DIXON", "DICKSONX"), &
            0.766666666666667_wp, thr=1.0e-12_wp)
        if (allocated(error)) return
        ! Trailing blanks of fixed-length strings do not count.
        call check(error, jaro_similarity("run   ", "run"), 1.0_wp)
    end subroutine test_jaro_similarity

    subroutine test_closest_match(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, closest_match("stauts", &
            [character(len=6) :: "run", "status", "stash"]), 2)
        if (allocated(error)) return
        call check(error, closest_match("zzz", &
            [character(len=6) :: "run", "status"]), 0)
        if (allocated(error)) return
        call check(error, closest_match("x", [character(len=1) ::]), 0)
    end subroutine test_closest_match

    subroutine test_unknown_option(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-o", "--output")
        call parser%add_argument("-v", "--verbose", action=store_true())

        result = parser%parse_tokens([character(len=12) :: "--outptu", "x"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_ARGUMENT)
        if (allocated(error)) return
        call check(error, allocated(entry%suggestion))
        if (allocated(error)) return
        call check(error, entry%suggestion, "--output")
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: unknown option '--outptu' (did you mean '--output'?)")
        if (allocated(error)) return

        ! An attached value is not part of the compared name.
        result = parser%parse_tokens([character(len=13) :: "--verbsoe=yes"])
        entry = result%errors%get(1)
        call check(error, allocated(entry%suggestion))
        if (allocated(error)) return
        call check(error, entry%suggestion, "--verbose")
    end subroutine test_unknown_option

    subroutine test_no_option_suggestion(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("-o", "--output")
        call parser%add_argument("-v", action=store_true())
        call parser%add_argument("--ab", action=store_true())

        result = parser%parse_tokens([character(len=8) :: "--zzzzz"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_ARGUMENT)
        if (allocated(error)) return
        call check(error, .not. allocated(entry%suggestion))
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: unknown option '--zzzzz'")
        if (allocated(error)) return

        ! Single-letter names are never compared.
        result = parser%parse_tokens([character(len=8) :: "-q"])
        entry = result%errors%get(1)
        call check(error, .not. allocated(entry%suggestion))
        if (allocated(error)) return

        ! Hyphens alone do not make two names similar.
        result = parser%parse_tokens([character(len=8) :: "--cd"])
        entry = result%errors%get(1)
        call check(error, .not. allocated(entry%suggestion))
    end subroutine test_no_option_suggestion

    subroutine test_hidden_not_suggested(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--secret", visible=.false.)
        call parser%add_argument("--legacy", removed_msg="use --modern")

        result = parser%parse_tokens([character(len=8) :: "--secert", "x"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_ARGUMENT)
        if (allocated(error)) return
        call check(error, .not. allocated(entry%suggestion))
        if (allocated(error)) return

        result = parser%parse_tokens([character(len=8) :: "--legcay", "x"])
        entry = result%errors%get(1)
        call check(error, .not. allocated(entry%suggestion))
    end subroutine test_hidden_not_suggested

    subroutine test_unknown_subcommand(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser, run_parser, status_parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call run_parser%init(add_help=.false.)
        call status_parser%init(add_help=.false.)
        call parser%init(prog="tool", add_help=.false.)
        call parser%add_subparsers(dest="command", required=.true.)
        call parser%add_parser("run", run_parser)
        call parser%add_parser("status", status_parser)

        result = parser%parse_tokens([character(len=8) :: "stauts"])
        call check(error, result%outcome, PARSE_FAILURE)
        if (allocated(error)) return
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_UNKNOWN_SUBCOMMAND)
        if (allocated(error)) return
        call check(error, allocated(entry%suggestion))
        if (allocated(error)) return
        call check(error, entry%suggestion, "status")
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: unknown subcommand 'stauts' (did you mean 'status'?)")
    end subroutine test_unknown_subcommand

    subroutine test_invalid_choice(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(ParseResult) :: result
        type(ErrorEntry) :: entry

        call parser%init(prog="tool", add_help=.false.)
        call parser%add_argument("--mode", choices=[character(len=4) :: &
            "fast", "safe"])
        call parser%add_argument("--color", choices=[character(len=5) :: &
            "red", "green"], case_insensitive_choices=.true.)
        call parser%add_argument("--level", data_type="integer", &
            choices=[1, 2])

        result = parser%parse_tokens([character(len=8) :: "--mode", "fsat"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_CHOICE)
        if (allocated(error)) return
        call check(error, allocated(entry%suggestion))
        if (allocated(error)) return
        call check(error, entry%suggestion, "fast")
        if (allocated(error)) return
        call check(error, parser%format_diagnostic(entry), &
            "tool: error: argument --mode: invalid choice: choose from " // &
            "'fast', 'safe' (got 'fsat') (did you mean 'fast'?)")
        if (allocated(error)) return

        ! Case-insensitive choices compare without case and suggest the
        ! declared spelling.
        result = parser%parse_tokens([character(len=8) :: "--color", "GRENE"])
        entry = result%errors%get(1)
        call check(error, allocated(entry%suggestion))
        if (allocated(error)) return
        call check(error, entry%suggestion, "green")
        if (allocated(error)) return

        ! Numeric choices are not suggested.
        result = parser%parse_tokens([character(len=8) :: "--level", "3"])
        entry = result%errors%get(1)
        call check(error, entry%code, ERR_INVALID_CHOICE)
        if (allocated(error)) return
        call check(error, .not. allocated(entry%suggestion))
    end subroutine test_invalid_choice

end module test_suggestions
