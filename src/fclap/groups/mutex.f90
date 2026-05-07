module fclap_groups_mutex
    use fclap_groups_helppage, only: HelpPage

    implicit none
    private

    type, extends(HelpPage) :: MutexGroup
        !> @brief List of argument names that are mutually exclusive
        character(len=:), allocatable :: mutex_args(:)
    end type MutexGroup
    
contains
    
end module fclap_groups_mutex