!> Boolean flag actions.
module fclap_actions_boolean
    use fclap_actions_abstract, only : ActionType, ACTION_CONTINUE
    use fclap_error_stack, only : ErrorStack
    use fclap_namespace, only : Namespace
    use fclap_nargs, only : NargsSpec, NARGS_ZERO
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : new_value
    implicit none
    private

    public :: StoreTrueAction, StoreFalseAction
    public :: store_true, store_false

    type, extends(ActionType) :: StoreTrueAction
    contains
        procedure :: apply => store_true_apply
        procedure :: default_nargs => store_true_default_nargs
        procedure :: accepts_nargs => store_true_accepts_nargs
        procedure :: value_type => store_true_value_type
        procedure :: implicit_default => store_true_default
    end type StoreTrueAction

    type, extends(ActionType) :: StoreFalseAction
    contains
        procedure :: apply => store_false_apply
        procedure :: default_nargs => store_false_default_nargs
        procedure :: accepts_nargs => store_false_accepts_nargs
        procedure :: value_type => store_false_value_type
        procedure :: implicit_default => store_false_default
    end type StoreFalseAction

contains

    function store_true() result(action)
        type(StoreTrueAction) :: action

        action = StoreTrueAction()
    end function store_true

    function store_false() result(action)
        type(StoreFalseAction) :: action

        action = StoreFalseAction()
    end function store_false

    subroutine store_true_apply(self, dest, value, args, outcome, errors)
        class(StoreTrueAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        call args%set_logical(dest, .true.)
        outcome = ACTION_CONTINUE
    end subroutine store_true_apply

    subroutine store_false_apply(self, dest, value, args, outcome, errors)
        class(StoreFalseAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        call args%set_logical(dest, .false.)
        outcome = ACTION_CONTINUE
    end subroutine store_false_apply

    pure integer function store_true_default_nargs(self) result(value)
        class(StoreTrueAction), intent(in) :: self

        value = NARGS_ZERO
    end function store_true_default_nargs

    pure logical function store_true_accepts_nargs(self, nargs) &
        result(accepted)
        class(StoreTrueAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() == NARGS_ZERO
    end function store_true_accepts_nargs

    pure integer function store_false_default_nargs(self) result(value)
        class(StoreFalseAction), intent(in) :: self

        value = NARGS_ZERO
    end function store_false_default_nargs

    pure logical function store_false_accepts_nargs(self, nargs) &
        result(accepted)
        class(StoreFalseAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() == NARGS_ZERO
    end function store_false_accepts_nargs

    function store_true_value_type(self) result(name)
        class(StoreTrueAction), intent(in) :: self
        character(len=:), allocatable :: name

        name = "logical"
    end function store_true_value_type

    function store_false_value_type(self) result(name)
        class(StoreFalseAction), intent(in) :: self
        character(len=:), allocatable :: name

        name = "logical"
    end function store_false_value_type

    subroutine store_true_default(self, value, has_default)
        class(StoreTrueAction), intent(in) :: self
        type(ValueBox), intent(out) :: value
        logical, intent(out) :: has_default

        value = new_value(.false.)
        has_default = .true.
    end subroutine store_true_default

    subroutine store_false_default(self, value, has_default)
        class(StoreFalseAction), intent(in) :: self
        type(ValueBox), intent(out) :: value
        logical, intent(out) :: has_default

        value = new_value(.true.)
        has_default = .true.
    end subroutine store_false_default

end module fclap_actions_boolean
