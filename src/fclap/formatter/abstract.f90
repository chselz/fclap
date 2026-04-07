! in python there ar 5 formatters default, metavar, defaults, raw description, rawtext


module flcap_formatter_abstract
    implicit none

    private

    !> Abstract formatter type
    type, abstract :: FormatterType
    contains
        procedure(format_help_interface), deferred :: format_help
        procedure(format_usage_interface), deferred ::format_usage
    end type FormatterType

    abstract interface 
        
    
    
end module flcap_formatter_abstract
