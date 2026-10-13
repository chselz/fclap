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
        !> Add spellings to the names given at registration
        procedure :: option_names => action_option_names
        !> Apply one occurrence, knowing which option spelling was used
        procedure :: apply_option => action_apply_option
        !> Whether the first explicit occurrence discards the default value
        procedure :: replaces_default => action_replaces_default
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

    !> Keep the registered names unchanged.
    !>
    !> A subroutine rather than a function: returning a deferred-length
    !> character array from a type-bound function crashes gfortran 13.
    subroutine action_option_names(self, names, expanded)
        class(ActionType), intent(in) :: self
        character(len=*), intent(in) :: names(:)
        character(len=:), allocatable, intent(out) :: expanded(:)

        expanded = names
    end subroutine action_option_names

    !> Apply an occurrence regardless of the spelling used.
    !>
    !> The parse engine calls this for options; `option_name` is the
    !> registered spelling that matched. Actions whose effect depends on the
    !> spelling, such as `--name`/`--no-name`, override it.
    subroutine action_apply_option(self, option_name, dest, value, args, &
        outcome, errors)
        class(ActionType), intent(in) :: self
        character(len=*), intent(in) :: option_name
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        call self%apply(dest, value, args, outcome, errors)
    end subroutine action_apply_option

    pure logical function action_replaces_default(self) result(replaces)
        class(ActionType), intent(in) :: self

        replaces = .false.
    end function action_replaces_default

end module fclap_actions_abstract
