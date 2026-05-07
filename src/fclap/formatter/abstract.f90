! in python there ar 5 formatters default, metavar, defaults, raw description, rawtext


module flcap_formatter_abstract
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
        function format_help_interface(self, parser) result(res)
            import :: FormatterType
            ! Can't import ArgumentParser easily as it would create circular dependency,
            ! so we pass parser components or just class(*)
            class(FormatterType), intent(in) :: self
            class(*), intent(in) :: parser
            character(len=:), allocatable :: res
        end function format_help_interface

        function format_usage_interface(self, parser) result(res)
            import :: FormatterType
            class(FormatterType), intent(in) :: self
            class(*), intent(in) :: parser
            character(len=:), allocatable :: res
        end function format_usage_interface
    end interface
    
end module flcap_formatter_abstract
