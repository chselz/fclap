!> Ordinary argument group that contributes a section to formatted help.
module fclap_groups_helppage
    use fclap_formatter_model, only : HelpGroup
    use fclap_groups_abstract, only : GroupType
    implicit none
    private

    public :: ArgumentGroup

    type, extends(GroupType) :: ArgumentGroup
    contains
        procedure :: help_snapshot => argument_group_help_snapshot
    end type ArgumentGroup

contains

    !> Build the formatter-facing representation of an ordinary group.
    function argument_group_help_snapshot(self) result(snapshot)
        class(ArgumentGroup), intent(in) :: self
        type(HelpGroup) :: snapshot

        snapshot = self%make_help_snapshot(.false.)
    end function argument_group_help_snapshot

end module fclap_groups_helppage
