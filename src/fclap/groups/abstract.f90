module fclap_groups_abstract
    implicit none
    private

    public :: GroupType

    type, abstract :: GroupType
        !> @brief Title displayed above the group in help output
        character(len=:), allocatable :: title
        !> @brief Optional description text for the group
        character(len=:), allocatable :: description
        !> @brief Whether at least one argument in the group is required
        logical :: required = .false.
    end type GroupType

    
contains
    
end module fclap_groups_abstract