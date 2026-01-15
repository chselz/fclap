module fclap_argparser
    implicit none
    private
    public ArgParser
    
    type :: ArgParser
    contains
        procedure :: add_argument
    end type
contains

    subroutine add_argument(self)
        type(ArgParser), intent(out) :: self
    end subroutine add_argument
    
end module fclap_argparser    
    