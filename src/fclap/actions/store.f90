!> Actions that store a parsed value or a predefined constant.
module fclap_actions_store
    use fclap_actions_abstract, only : ActionType, ACTION_CONTINUE
    use fclap_error_stack, only : ErrorStack
    use fclap_namespace, only : Namespace
    use fclap_nargs, only : NargsSpec, NARGS_ZERO
    use fclap_utils_accuracy, only : ip, wp
    use fclap_value_abstract, only : ValueBox
    use fclap_value_builtin, only : new_value
    implicit none
    private

    public :: StoreAction, StoreConstAction
    public :: store, store_const

    type, extends(ActionType) :: StoreAction
    contains
        procedure :: apply => store_apply
        procedure :: accepts_nargs => store_accepts_nargs
    end type StoreAction

    type, extends(ActionType) :: StoreConstAction
        type(ValueBox) :: constant
    contains
        procedure :: apply => store_const_apply
        procedure :: default_nargs => store_const_default_nargs
        procedure :: accepts_nargs => store_const_accepts_nargs
        procedure :: value_type => store_const_value_type
    end type StoreConstAction

    interface store_const
        module procedure :: store_const_character
        module procedure :: store_const_integer
        module procedure :: store_const_real
        module procedure :: store_const_logical
    end interface store_const

contains

    function store() result(action)
        type(StoreAction) :: action

        action = StoreAction()
    end function store

    function store_const_character(value) result(action)
        character(len=*), intent(in) :: value
        type(StoreConstAction) :: action

        action%constant = new_value(value)
    end function store_const_character

    function store_const_integer(value) result(action)
        integer(ip), intent(in) :: value
        type(StoreConstAction) :: action

        action%constant = new_value(value)
    end function store_const_integer

    function store_const_real(value) result(action)
        real(wp), intent(in) :: value
        type(StoreConstAction) :: action

        action%constant = new_value(value)
    end function store_const_real

    function store_const_logical(value) result(action)
        logical, intent(in) :: value
        type(StoreConstAction) :: action

        action%constant = new_value(value)
    end function store_const_logical

    subroutine store_apply(self, dest, value, args, outcome, errors)
        class(StoreAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        call args%set_value(dest, value)
        outcome = ACTION_CONTINUE
    end subroutine store_apply

    subroutine store_const_apply(self, dest, value, args, outcome, errors)
        class(StoreConstAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        call args%set_value(dest, self%constant)
        outcome = ACTION_CONTINUE
    end subroutine store_const_apply

    pure logical function store_accepts_nargs(self, nargs) result(accepted)
        class(StoreAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() /= NARGS_ZERO
    end function store_accepts_nargs

    pure integer function store_const_default_nargs(self) result(value)
        class(StoreConstAction), intent(in) :: self

        value = NARGS_ZERO
    end function store_const_default_nargs

    pure logical function store_const_accepts_nargs(self, nargs) &
        result(accepted)
        class(StoreConstAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() == NARGS_ZERO
    end function store_const_accepts_nargs

    function store_const_value_type(self) result(name)
        class(StoreConstAction), intent(in) :: self
        character(len=:), allocatable :: name

        name = self%constant%type_name()
    end function store_const_value_type

end module fclap_actions_store
