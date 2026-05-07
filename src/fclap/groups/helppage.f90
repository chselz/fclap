module fclap_groups_helppage
    use fclap_groups_abstract, only: GroupType
    implicit none
    private

    public :: HelpPage
    
    type, extends(GroupType) :: HelpPage
        !> @brief List of argument names that belong to this help page
        character(len=:), allocatable :: arg_names(:)
    end type HelpPage
contains
    
end module fclap_groups_helppage
