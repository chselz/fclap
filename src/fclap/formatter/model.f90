!> Data-only snapshot consumed by help formatters.
module fclap_formatter_model
    implicit none
    private

    public :: HelpArgument
    public :: HelpGroup
    public :: HelpCommand
    public :: HelpModel

    !> Formatting-relevant view of one registered argument.
    !>
    !> This type deliberately contains no parser, action, validator, or namespace
    !> state. A formatter can therefore be implemented outside fclap without
    !> depending on parser internals.
    type :: HelpArgument
        character(len=:), allocatable :: names(:)
        character(len=:), allocatable :: dest
        character(len=:), allocatable :: metavar
        character(len=:), allocatable :: help
        character(len=:), allocatable :: default_text
        character(len=:), allocatable :: choices_text
        character(len=:), allocatable :: deprecated_msg
        character(len=:), allocatable :: removed_msg
        integer :: nargs = 1
        logical :: is_optional = .false.
        logical :: required = .false.
        logical :: visible = .true.
        logical :: has_default = .false.
        logical :: has_choices = .false.
        logical :: print_default = .true.
        logical :: print_choices = .false.
    end type HelpArgument

    !> Formatting-relevant view of one ordinary or mutually exclusive group.
    type :: HelpGroup
        character(len=:), allocatable :: title
        character(len=:), allocatable :: description
        integer, allocatable :: argument_indices(:)
        logical :: is_mutex = .false.
        logical :: required = .false.
    end type HelpGroup

    !> Formatting-relevant view of one registered subcommand.
    type :: HelpCommand
        character(len=:), allocatable :: name
        character(len=:), allocatable :: help
    end type HelpCommand

    !> Immutable-by-convention parser metadata supplied to a formatter.
    type :: HelpModel
        character(len=:), allocatable :: prog
        character(len=:), allocatable :: usage
        character(len=:), allocatable :: description
        character(len=:), allocatable :: epilog
        type(HelpArgument), allocatable :: arguments(:)
        type(HelpGroup), allocatable :: groups(:)
        character(len=:), allocatable :: subcommand_title
        character(len=:), allocatable :: subcommand_description
        logical :: subcommand_required = .false.
        type(HelpCommand), allocatable :: commands(:)
    end type HelpModel

end module fclap_formatter_model
