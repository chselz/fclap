module fclap_actions_store
    use fclap_actions_abstract, only : ActionType
    use fclap_namespace, only : Namespace
    use fclap_error_stack, only : ErrorStack
    implicit none

    private
    public :: StoreAction

    type, extends(ActionType) :: StoreAction
    contains
        procedure :: execute => store_action_execute
    end type StoreAction

contains

    subroutine store_action_execute(self, args, values, error_stack)
        class(StoreAction), intent(inout) :: self
        type(Namespace), intent(inout) :: args
        character(len=*), intent(in) :: values(:)
        type(ErrorStack), intent(inout), optional :: error_stack

        integer :: i

        if (.not. allocated(self%dest)) return

        if (size(values) > 0) then
            call args%set_string(self%dest, values(1))
        end if
    end subroutine store_action_execute

end module fclap_actions_store
