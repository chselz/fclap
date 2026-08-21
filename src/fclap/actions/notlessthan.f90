module fclap_actions_notlessthan
    use fclap_actions_abstract, only : ActionType
    implicit none
    
    private

    type, extends(ActionType) :: NotLessThanAction
    end type NotLessThanAction

    interface not_less_than
        module procedure new_not_less_than_real
        module procedure new_not_less_than_integer
    end interface not_less_than

contains

    
    
end module fclap_actions_notlessthan