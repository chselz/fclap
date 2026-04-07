module fclap_formatter_standard
    use flcap_formatter_abstract, only : FormatterType
    implicit none

    private

    type, extends(FormatterType) :: StandardFormatter
        ! configuration flags
        logical :: show_defaults = .true.
        logical :: show_choices = .true.
        logical :: raw_description = .false.
        logical :: raw_help_text = .false.
        ! TODO: test later that this is not less than 1 otherwise try to get terminal size or set back to default of 80
        integer :: help_width = 80
    end type StandardFormatter
    
contains
    
end module fclap_formatter_standard