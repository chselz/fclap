module fclap_formatter_abstract
    use fclap_formatter_model, only : HelpModel
    implicit none

    private
    public :: FormatterType

    !> Abstract formatter type
    type, abstract :: FormatterType
    contains
        procedure(format_help_interface), deferred :: format_help
        procedure(format_usage_interface), deferred :: format_usage
    end type FormatterType

    abstract interface 
        function format_help_interface(self, model) result(res)
            import :: FormatterType, HelpModel
            class(FormatterType), intent(in) :: self
            type(HelpModel), intent(in) :: model
            character(len=:), allocatable :: res
        end function format_help_interface

        function format_usage_interface(self, model) result(res)
            import :: FormatterType, HelpModel
            class(FormatterType), intent(in) :: self
            type(HelpModel), intent(in) :: model
            character(len=:), allocatable :: res
        end function format_usage_interface
    end interface
    
end module fclap_formatter_abstract
