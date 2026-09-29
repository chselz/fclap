!> Abstract extension contract for argument actions.
module fclap_actions_abstract
    use fclap_error_stack, only : ErrorStack
    use fclap_namespace, only : Namespace
    use fclap_nargs, only : NargsSpec, NARGS_ONE
    use fclap_value_abstract, only : ValueBox
    implicit none
    private

    integer, parameter, public :: ACTION_CONTINUE = 0
    integer, parameter, public :: ACTION_HELP_REQUESTED = 1
    integer, parameter, public :: ACTION_VERSION_REQUESTED = 2

    public :: ActionType

    type, abstract :: ActionType
    contains
        procedure(action_apply_interface), deferred :: apply
        procedure :: default_nargs => action_default_nargs
        procedure :: accepts_nargs => action_accepts_nargs
        procedure :: produces_list => action_produces_list
        procedure :: value_type => action_value_type
        procedure :: implicit_default => action_implicit_default
    end type ActionType

    abstract interface
        subroutine action_apply_interface(self, dest, value, args, outcome, errors)
            import :: ActionType, ErrorStack, Namespace, ValueBox
            class(ActionType), intent(in) :: self
            character(len=*), intent(in) :: dest
            type(ValueBox), intent(in) :: value
            type(Namespace), intent(inout) :: args
            integer, intent(out) :: outcome
            type(ErrorStack), intent(inout) :: errors
        end subroutine action_apply_interface
    end interface

contains

    pure integer function action_default_nargs(self) result(value)
        class(ActionType), intent(in) :: self

        value = NARGS_ONE
    end function action_default_nargs

    pure logical function action_accepts_nargs(self, nargs) result(accepted)
        class(ActionType), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid()
    end function action_accepts_nargs

    pure logical function action_produces_list(self, nargs) result(produces_list)
        class(ActionType), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        produces_list = nargs%produces_list()
    end function action_produces_list

    function action_value_type(self) result(name)
        class(ActionType), intent(in) :: self
        character(len=:), allocatable :: name

        name = ""
    end function action_value_type

    subroutine action_implicit_default(self, value, has_default)
        class(ActionType), intent(in) :: self
        type(ValueBox), intent(out) :: value
        logical, intent(out) :: has_default

        call value%clear()
        has_default = .false.
    end subroutine action_implicit_default

end module fclap_actions_abstract
