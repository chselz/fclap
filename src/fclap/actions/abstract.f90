module fclap_actions_abstract
    use fclap_namespace, only : Namespace
    use fclap_error_stack, only : ErrorStack

    implicit none

    private
    public :: ActionType

    type, abstract :: ActionType
        character(len=:), allocatable :: dest
        character(len=:), allocatable :: option_strings(:)
    contains
        procedure(execute_interface), deferred :: execute
    end type ActionType

    abstract interface

        subroutine execute_interface(self, args, values, error_stack)
            import :: ActionType, Namespace, ErrorStack
            class(ActionType), intent(inout) :: self
            type(Namespace), intent(inout) :: args
            character(len=*), intent(in) :: values(:)
            type(ErrorStack), intent(inout), optional :: error_stack
        end subroutine execute_interface
        
    end interface

end module fclap_actions_abstract
