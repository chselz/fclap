!> Actions that accumulate repeated argument occurrences.
module fclap_actions_accumulate
    use fclap_actions_abstract, only : ActionType, ACTION_CONTINUE
    use fclap_error_codes, only : FCLAP_OK, ERR_NAMESPACE_MISSING_KEY
    use fclap_error_stack, only : ErrorStack
    use fclap_namespace, only : Namespace
    use fclap_nargs, only : NargsSpec, NARGS_ZERO
    use fclap_utils_accuracy, only : ip
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : ListValue, new_value
    implicit none
    private

    public :: AppendAction, CountAction
    public :: append, count

    type, extends(ActionType) :: AppendAction
    contains
        procedure :: apply => append_apply
        procedure :: accepts_nargs => append_accepts_nargs
        procedure :: produces_list => append_produces_list
    end type AppendAction

    type, extends(ActionType) :: CountAction
    contains
        procedure :: apply => count_apply
        procedure :: default_nargs => count_default_nargs
        procedure :: accepts_nargs => count_accepts_nargs
        procedure :: value_type => integer_value_type
        procedure :: implicit_default => count_default
    end type CountAction

contains

    function append() result(action)
        type(AppendAction) :: action

        action = AppendAction()
    end function append

    function count() result(action)
        type(CountAction) :: action

        action = CountAction()
    end function count

    subroutine append_apply(self, dest, value, args, outcome, errors)
        class(AppendAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors
        integer :: index, stat

        select type (list => value%item)
        type is (ListValue)
            if (list%size() == 0 .and. .not. args%contains(dest)) then
                call args%set_value(dest, value)
            end if
            do index = 1, list%size()
                call args%append_value(dest, list%items(index), stat)
            end do
        class default
            call args%append_value(dest, value, stat)
        end select
        outcome = ACTION_CONTINUE
    end subroutine append_apply

    subroutine count_apply(self, dest, value, args, outcome, errors)
        class(CountAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors
        integer(ip) :: current
        integer :: stat

        current = 0_ip
        call args%get_integer(dest, current, stat)
        if (stat /= FCLAP_OK .and. stat /= ERR_NAMESPACE_MISSING_KEY) then
            call errors%add("count destination is not an integer", code=stat, &
                arg_name=dest)
            outcome = ACTION_CONTINUE
            return
        end if
        call args%set_integer(dest, current + 1_ip)
        outcome = ACTION_CONTINUE
    end subroutine count_apply

    pure logical function append_accepts_nargs(self, nargs) result(accepted)
        class(AppendAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() /= NARGS_ZERO
    end function append_accepts_nargs

    pure logical function append_produces_list(self, nargs) &
        result(produces_list)
        class(AppendAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        produces_list = .true.
    end function append_produces_list

    pure integer function count_default_nargs(self) result(value)
        class(CountAction), intent(in) :: self

        value = NARGS_ZERO
    end function count_default_nargs

    pure logical function count_accepts_nargs(self, nargs) result(accepted)
        class(CountAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() == NARGS_ZERO
    end function count_accepts_nargs

    function integer_value_type(self) result(name)
        class(CountAction), intent(in) :: self
        character(len=:), allocatable :: name

        name = "integer"
    end function integer_value_type

    subroutine count_default(self, value, has_default)
        class(CountAction), intent(in) :: self
        type(ValueBox), intent(out) :: value
        logical, intent(out) :: has_default

        value = new_value(0_ip)
        has_default = .true.
    end subroutine count_default

end module fclap_actions_accumulate
