module test_actions
    use fclap, only : ArgumentParser, ActionType, Namespace, ErrorStack, &
        ValueBox, ip, new_value, &
        new_nargs, NARGS_ZERO, NARGS_ONE, ACTION_CONTINUE, &
        ACTION_HELP_REQUESTED, ACTION_VERSION_REQUESTED, store, store_const, &
        store_true, store_false, append, count, StoreAction, StoreConstAction, &
        StoreTrueAction, StoreFalseAction, AppendAction, CountAction
    use fclap_actions_builtin, only : HelpAction, VersionAction, &
        help_action, version_action
    use fclap_nargs, only : NargsSpec
    use fclap_argument, only : Argument
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_actions

    type, extends(ActionType) :: TestAction
    contains
        procedure :: apply => test_action_apply
    end type TestAction

contains

    subroutine collect_actions(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("action arity contracts", test_action_arity), &
            new_unittest("store family apply", test_store_family), &
            new_unittest("append and count apply", test_append_and_count), &
            new_unittest("help and version outcomes", test_control_outcomes), &
            new_unittest("external action extension", test_external_action) &
        ]
    end subroutine collect_actions

    subroutine test_action_arity(error)
        type(error_type), allocatable, intent(out) :: error
        type(StoreAction) :: store_op
        type(StoreTrueAction) :: true_op
        type(AppendAction) :: append_op
        type(CountAction) :: count_op
        type(NargsSpec) :: zero, one

        store_op = store()
        true_op = store_true()
        append_op = append()
        count_op = count()
        zero = new_nargs(NARGS_ZERO)
        one = new_nargs(NARGS_ONE)

        call check(error, store_op%default_nargs(), NARGS_ONE)
        if (allocated(error)) return
        call check(error, store_op%accepts_nargs(one), .true.)
        if (allocated(error)) return
        call check(error, store_op%accepts_nargs(zero), .false.)
        if (allocated(error)) return
        call check(error, true_op%default_nargs(), NARGS_ZERO)
        if (allocated(error)) return
        call check(error, true_op%accepts_nargs(one), .false.)
        if (allocated(error)) return
        call check(error, append_op%produces_list(one), .true.)
        if (allocated(error)) return
        call check(error, count_op%value_type(), "integer")
    end subroutine test_action_arity

    subroutine test_store_family(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        type(ErrorStack) :: errors
        type(ValueBox) :: value
        type(StoreAction) :: store_op
        type(StoreConstAction) :: const_op
        type(StoreTrueAction) :: true_op
        type(StoreFalseAction) :: false_op
        character(len=16) :: text
        logical :: flag
        integer :: outcome, stat

        store_op = store()
        value = new_value("input.dat")
        call store_op%apply("input", value, args, outcome, errors)
        call check(error, outcome, ACTION_CONTINUE)
        if (allocated(error)) return
        text = ""
        call args%get("input", text, stat)
        call check(error, trim(text), "input.dat")
        if (allocated(error)) return

        const_op = store_const("fast")
        call const_op%apply("mode", value, args, outcome, errors)
        text = ""
        call args%get("mode", text, stat)
        call check(error, trim(text), "fast")
        if (allocated(error)) return

        true_op = store_true()
        call true_op%apply("enabled", value, args, outcome, errors)
        call args%get("enabled", flag, stat)
        call check(error, flag, .true.)
        if (allocated(error)) return

        false_op = store_false()
        call false_op%apply("enabled", value, args, outcome, errors)
        call args%get("enabled", flag, stat)
        call check(error, flag, .false.)
        if (allocated(error)) return
        call check(error, errors%count(), 0)
    end subroutine test_store_family

    subroutine test_append_and_count(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        type(ErrorStack) :: errors
        type(ValueBox) :: value
        type(AppendAction) :: append_op
        type(CountAction) :: count_op
        integer(ip), allocatable :: values(:)
        integer(ip) :: occurrences
        integer :: outcome, stat

        append_op = append()
        value = new_value([1_ip, 2_ip])
        call append_op%apply("ids", value, args, outcome, errors)
        value = new_value(3_ip)
        call append_op%apply("ids", value, args, outcome, errors)
        call args%get("ids", values, stat)
        call check(error, all(values == [1_ip, 2_ip, 3_ip]), .true.)
        if (allocated(error)) return

        count_op = count()
        call value%clear()
        call count_op%apply("verbose", value, args, outcome, errors)
        call count_op%apply("verbose", value, args, outcome, errors)
        call args%get("verbose", occurrences, stat)
        call check(error, occurrences, 2_ip)
        if (allocated(error)) return
        call check(error, errors%count(), 0)
    end subroutine test_append_and_count

    subroutine test_control_outcomes(error)
        type(error_type), allocatable, intent(out) :: error
        type(Namespace) :: args
        type(ErrorStack) :: errors
        type(ValueBox) :: value
        type(HelpAction) :: help_request
        type(VersionAction) :: version_request
        integer :: outcome

        help_request = help_action()
        call help_request%apply("help", value, args, outcome, errors)
        call check(error, outcome, ACTION_HELP_REQUESTED)
        if (allocated(error)) return

        version_request = version_action()
        call version_request%apply("version", value, args, outcome, errors)
        call check(error, outcome, ACTION_VERSION_REQUESTED)
        if (allocated(error)) return
        call check(error, errors%count(), 0)
    end subroutine test_control_outcomes

    subroutine test_external_action(error)
        type(error_type), allocatable, intent(out) :: error
        type(ArgumentParser) :: parser
        type(Argument) :: definition
        type(TestAction) :: custom

        call parser%init(add_help=.false.)
        call parser%add_argument("--custom", action=custom)
        call check(error, parser%is_valid(), .true.)
        if (allocated(error)) return
        definition = parser%get_argument(1)
        select type (action => definition%action)
        type is (TestAction)
            call check(error, action%default_nargs(), NARGS_ONE)
        class default
            call check(error, .false., "custom action dynamic type was not cloned")
        end select
    end subroutine test_external_action

    subroutine test_action_apply(self, dest, value, args, outcome, errors)
        class(TestAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        call args%set_value(dest, value)
        outcome = ACTION_CONTINUE
    end subroutine test_action_apply

end module test_actions
