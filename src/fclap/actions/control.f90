!> Actions that request parser control-flow outcomes.
module fclap_actions_control
    use fclap_actions_abstract, only : ActionType, ACTION_HELP_REQUESTED, &
        ACTION_VERSION_REQUESTED
    use fclap_error_stack, only : ErrorStack
    use fclap_namespace, only : Namespace
    use fclap_nargs, only : NargsSpec, NARGS_ZERO
    use fclap_value_abstract, only : ValueBox
    implicit none
    private

    public :: HelpAction, VersionAction
    public :: help_action, version_action

    type, extends(ActionType) :: HelpAction
    contains
        procedure :: apply => help_apply
        procedure :: default_nargs => help_default_nargs
        procedure :: accepts_nargs => help_accepts_nargs
        procedure :: value_type => help_value_type
    end type HelpAction

    type, extends(ActionType) :: VersionAction
    contains
        procedure :: apply => version_apply
        procedure :: default_nargs => version_default_nargs
        procedure :: accepts_nargs => version_accepts_nargs
        procedure :: value_type => version_value_type
    end type VersionAction

contains

    function help_action() result(action)
        type(HelpAction) :: action

        action = HelpAction()
    end function help_action

    function version_action() result(action)
        type(VersionAction) :: action

        action = VersionAction()
    end function version_action

    subroutine help_apply(self, dest, value, args, outcome, errors)
        class(HelpAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        outcome = ACTION_HELP_REQUESTED
    end subroutine help_apply

    subroutine version_apply(self, dest, value, args, outcome, errors)
        class(VersionAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        outcome = ACTION_VERSION_REQUESTED
    end subroutine version_apply

    pure integer function help_default_nargs(self) result(value)
        class(HelpAction), intent(in) :: self

        value = NARGS_ZERO
    end function help_default_nargs

    pure logical function help_accepts_nargs(self, nargs) result(accepted)
        class(HelpAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() == NARGS_ZERO
    end function help_accepts_nargs

    pure integer function version_default_nargs(self) result(value)
        class(VersionAction), intent(in) :: self

        value = NARGS_ZERO
    end function version_default_nargs

    pure logical function version_accepts_nargs(self, nargs) result(accepted)
        class(VersionAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() == NARGS_ZERO
    end function version_accepts_nargs

    function help_value_type(self) result(name)
        class(HelpAction), intent(in) :: self
        character(len=:), allocatable :: name

        name = "logical"
    end function help_value_type

    function version_value_type(self) result(name)
        class(VersionAction), intent(in) :: self
        character(len=:), allocatable :: name

        name = "logical"
    end function version_value_type

end module fclap_actions_control
