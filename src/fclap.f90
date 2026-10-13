!> Public interface of fclap, a Fortran command-line argument parser.
!>
!> Import names with an `only` list: short factory names such as `count`
!> would otherwise shadow intrinsics.
module fclap
    use fclap_argparser, only : ArgumentParser
    use fclap_argument, only : Argument
    use fclap_actions_abstract, only : ActionType, ACTION_CONTINUE, &
        ACTION_HELP_REQUESTED, ACTION_VERSION_REQUESTED
    use fclap_actions_builtin, only : StoreAction, StoreConstAction, &
        StoreTrueAction, StoreFalseAction, BooleanOptionalAction, AppendAction, &
        CountAction, store, store_const, store_true, store_false, &
        boolean_optional, append, count
    use fclap_namespace, only : Namespace
    use fclap_parse_result, only : ParseResult, PARSE_SUCCESS, PARSE_FAILURE, &
        PARSE_HELP, PARSE_VERSION, SOURCE_NONE, SOURCE_DEFAULT, &
        SOURCE_ENVIRONMENT, SOURCE_COMMAND_LINE
    use fclap_nargs, only : NargsSpec, new_nargs, normalize_nargs, &
        NARGS_INVALID, NARGS_OPTIONAL, NARGS_ZERO_OR_MORE, &
        NARGS_ONE_OR_MORE, NARGS_REMAINDER, NARGS_ZERO, NARGS_ONE, &
        NARGS_UNBOUNDED, NARGS_SUCCESS, NARGS_INVALID_VALUE
    use fclap_error_entry, only : ErrorEntry
    use fclap_error_stack, only : ErrorStack
    use fclap_error_codes
    use fclap_value_abstract, only : ValueType, ValueBox
    use fclap_value_builtin, only : StringValue, IntegerValue, RealValue, &
        LogicalValue, ListValue, new_value
    use fclap_validators_abstract, only : ValidatorType
    use fclap_validators_bounds, only : LowerBoundValidator, &
        UpperBoundValidator, RangeValidator, not_less_than, not_bigger_than, &
        in_range
    use fclap_version, only : get_fclap_version, fclap_version_string, &
        fclap_version_compact
    use fclap_formatter_model, only : HelpArgument, HelpGroup, HelpCommand, &
        HelpModel
    use fclap_formatter_abstract, only : FormatterType
    use fclap_formatter_standard, only : StandardFormatter
    use fclap_groups_abstract, only : GroupHandle
    implicit none
    private

    ! Application interface: everything a command-line program needs.
    public :: ArgumentParser, Namespace, ParseResult, GroupHandle
    public :: PARSE_SUCCESS, PARSE_FAILURE, PARSE_HELP, PARSE_VERSION
    public :: SOURCE_NONE, SOURCE_DEFAULT, SOURCE_ENVIRONMENT
    public :: SOURCE_COMMAND_LINE
    public :: store, store_const, store_true, store_false, boolean_optional
    public :: append, count
    public :: not_less_than, not_bigger_than, in_range
    public :: StandardFormatter
    public :: ErrorEntry, ErrorStack, ERROR_FATAL, ERROR_WARNING
    public :: FCLAP_OK
    public :: ERR_INVALID_ARGUMENT_NAME, ERR_DUPLICATE_OPTION
    public :: ERR_DUPLICATE_DEST, ERR_INVALID_NARGS, ERR_INCOMPATIBLE_ACTION
    public :: ERR_INVALID_DEFAULT, ERR_INVALID_GROUP, ERR_INVALID_TYPE_NAME
    public :: ERR_INVALID_LIFECYCLE, ERR_INCOMPATIBLE_VALIDATOR
    public :: ERR_INVALID_RULE, ERR_INVALID_ENV
    public :: ERR_UNKNOWN_ARGUMENT, ERR_MISSING_VALUE, ERR_EXTRA_POSITIONAL
    public :: ERR_MISSING_REQUIRED, ERR_INVALID_VALUE, ERR_INVALID_CHOICE
    public :: ERR_VALIDATION_FAILED, ERR_MUTEX_CONFLICT, ERR_MUTEX_REQUIRED
    public :: ERR_REMOVED_ARGUMENT, ERR_DEPRECATED_ARGUMENT
    public :: ERR_UNEXPECTED_VALUE, ERR_MISSING_DEPENDENCY
    public :: ERR_ARGUMENT_CONFLICT
    public :: ERR_UNKNOWN_SUBCOMMAND, ERR_MISSING_SUBCOMMAND
    public :: ERR_NAMESPACE_MISSING_KEY, ERR_NAMESPACE_TYPE_MISMATCH
    public :: ERR_NAMESPACE_OUT_OF_RANGE
    public :: get_fclap_version, fclap_version_string, fclap_version_compact

    ! Extension interface: abstract types and the data they exchange, for
    ! custom actions, validators, and formatters.
    public :: ActionType, ACTION_CONTINUE, ACTION_HELP_REQUESTED
    public :: ACTION_VERSION_REQUESTED
    public :: StoreAction, StoreConstAction, StoreTrueAction, StoreFalseAction
    public :: BooleanOptionalAction, AppendAction, CountAction
    public :: ValidatorType, LowerBoundValidator, UpperBoundValidator
    public :: RangeValidator
    public :: FormatterType, HelpArgument, HelpGroup, HelpCommand, HelpModel
    public :: Argument
    public :: ValueType, ValueBox, StringValue, IntegerValue, RealValue
    public :: LogicalValue, ListValue, new_value
    public :: NargsSpec, new_nargs, normalize_nargs
    public :: NARGS_INVALID, NARGS_OPTIONAL, NARGS_ZERO_OR_MORE
    public :: NARGS_ONE_OR_MORE, NARGS_REMAINDER, NARGS_ZERO, NARGS_ONE
    public :: NARGS_UNBOUNDED, NARGS_SUCCESS, NARGS_INVALID_VALUE

end module fclap
