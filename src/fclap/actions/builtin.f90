!> Stable import surface for all built-in action implementations.
module fclap_actions_builtin
    use fclap_actions_accumulate, only : AppendAction, CountAction, &
        append, count
    use fclap_actions_boolean, only : StoreTrueAction, StoreFalseAction, &
        BooleanOptionalAction, store_true, store_false, boolean_optional
    use fclap_actions_control, only : HelpAction, VersionAction, &
        help_action, version_action
    use fclap_actions_store, only : StoreAction, StoreConstAction, &
        store, store_const
    implicit none
    private

    public :: StoreAction, StoreConstAction, StoreTrueAction, StoreFalseAction
    public :: BooleanOptionalAction, boolean_optional
    public :: AppendAction, CountAction, HelpAction, VersionAction
    public :: store, store_const, store_true, store_false, append, count
    public :: help_action, version_action

end module fclap_actions_builtin
