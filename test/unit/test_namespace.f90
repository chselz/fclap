module test_namespace
    use fclap_utils_accuracy, only : wp
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed

    implicit none
    private
    
    public :: collect_namespace
contains

    subroutine collect_namespace
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            & new_unittest(), &
            & ]
    end subroutine collect_namespace
    
end module test_namespace