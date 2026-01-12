module fclap
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fclap!"
  end subroutine say_hello
end module fclap
