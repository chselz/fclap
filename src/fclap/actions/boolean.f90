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

    public :: StoreTrueAction, StoreFalseAction, BooleanOptionalAction
    public :: store_true, store_false, boolean_optional

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

    !> Flag with a negated spelling: `--name` stores true, `--no-name` false
    type, extends(ActionType) :: BooleanOptionalAction
    contains
        procedure :: apply => boolean_optional_apply
        procedure :: apply_option => boolean_optional_apply_option
        procedure :: option_names => boolean_optional_names
        procedure :: default_nargs => boolean_optional_default_nargs
        procedure :: accepts_nargs => boolean_optional_accepts_nargs
        procedure :: value_type => boolean_optional_value_type
        procedure :: implicit_default => boolean_optional_default
    end type BooleanOptionalAction

contains

    function boolean_optional() result(action)
        type(BooleanOptionalAction) :: action

        action = BooleanOptionalAction()
    end function boolean_optional

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

    subroutine boolean_optional_apply(self, dest, value, args, outcome, errors)
        class(BooleanOptionalAction), intent(in) :: self
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        call args%set_logical(dest, .true.)
        outcome = ACTION_CONTINUE
    end subroutine boolean_optional_apply

    subroutine boolean_optional_apply_option(self, option_name, dest, value, &
        args, outcome, errors)
        class(BooleanOptionalAction), intent(in) :: self
        character(len=*), intent(in) :: option_name
        character(len=*), intent(in) :: dest
        type(ValueBox), intent(in) :: value
        type(Namespace), intent(inout) :: args
        integer, intent(out) :: outcome
        type(ErrorStack), intent(inout) :: errors

        call args%set_logical(dest, .not. is_negated(option_name))
        outcome = ACTION_CONTINUE
    end subroutine boolean_optional_apply_option

    !> Add `--no-name` after every long `--name` spelling.
    subroutine boolean_optional_names(self, names, expanded)
        class(BooleanOptionalAction), intent(in) :: self
        character(len=*), intent(in) :: names(:)
        character(len=:), allocatable, intent(out) :: expanded(:)
        integer :: index, count_long, position

        count_long = 0
        do index = 1, size(names)
            if (is_long_positive(names(index))) count_long = count_long + 1
        end do
        allocate(character(len=len(names) + 3) :: expanded(size(names) + count_long))
        position = 0
        do index = 1, size(names)
            position = position + 1
            expanded(position) = names(index)
        end do
        do index = 1, size(names)
            if (.not. is_long_positive(names(index))) cycle
            position = position + 1
            expanded(position) = "--no-" // trim(names(index)(3:))
        end do
    end subroutine boolean_optional_names

    pure integer function boolean_optional_default_nargs(self) result(value)
        class(BooleanOptionalAction), intent(in) :: self

        value = NARGS_ZERO
    end function boolean_optional_default_nargs

    pure logical function boolean_optional_accepts_nargs(self, nargs) &
        result(accepted)
        class(BooleanOptionalAction), intent(in) :: self
        type(NargsSpec), intent(in) :: nargs

        accepted = nargs%is_valid() .and. nargs%value() == NARGS_ZERO
    end function boolean_optional_accepts_nargs

    function boolean_optional_value_type(self) result(name)
        class(BooleanOptionalAction), intent(in) :: self
        character(len=:), allocatable :: name

        name = "logical"
    end function boolean_optional_value_type

    subroutine boolean_optional_default(self, value, has_default)
        class(BooleanOptionalAction), intent(in) :: self
        type(ValueBox), intent(out) :: value
        logical, intent(out) :: has_default

        value = new_value(.false.)
        has_default = .true.
    end subroutine boolean_optional_default

    pure logical function is_long_positive(name) result(long)
        character(len=*), intent(in) :: name

        long = len_trim(name) > 2
        if (long) long = name(1:2) == "--" .and. .not. is_negated(name)
    end function is_long_positive

    pure logical function is_negated(name) result(negated)
        character(len=*), intent(in) :: name

        negated = len_trim(name) > 5
        if (negated) negated = name(1:5) == "--no-"
    end function is_negated

end module fclap_actions_boolean
