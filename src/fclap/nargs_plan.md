When porting a mixed-type argument like Python's `nargs` (which can be an integer like `2` or a string like `"+"`) to Fortran, your suggested approach—forcing the user to pass a string (e.g., `nargs="2"`) and parsing it at runtime—will work. You would use a non-advancing `read` with an `iostat` check to see if it's an integer, and if that fails, fall back to checking for `"+"`, `"*"`, or `"?"`.

However, **there are two better, more idiomatic approaches in modern Fortran (F2003+)** that don't require runtime string parsing and allow you to keep the Python-like API where the user can pass either an actual integer or a character.

### 1. The Best Approach: Generic Interfaces (Function Overloading)
Fortran allows you to define multiple specific subroutines and group them under a single generic name. The compiler will automatically figure out which one to call based on whether the user passes an integer or a string. This provides compile-time safety.

```fortran
module argparse_mod
    implicit none
    
    ! Expose a single interface to the user
    interface add_argument
        module procedure add_argument_int
        module procedure add_argument_char
    end interface

contains

    ! Called if the user passes an integer: call add_argument("--foo", nargs=2)
    subroutine add_argument_int(name, nargs)
        character(len=*), intent(in) :: name
        integer, intent(in)          :: nargs
        
        print *, "Exact number of args requested: ", nargs
        ! Store as a specific integer flag in your internal state
    end subroutine add_argument_int

    ! Called if the user passes a string: call add_argument("--foo", nargs="+")
    subroutine add_argument_char(name, nargs)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: nargs
        
        select case (trim(nargs))
            case ("+")
                print *, "One or more args requested"
            case ("*")
                print *, "Zero or more args requested"
            case ("?")
                print *, "Zero or one arg requested"
            case default
                print *, "Invalid nargs string!"
        end select
    end subroutine add_argument_char

end module argparse_mod
```

### 2. The Alternative Approach: Unlimited Polymorphism (`class(*)`)
If you strongly prefer keeping everything inside a single subroutine, you can use Fortran's unlimited polymorphic type (`class(*)`). This is the closest analog to Python's dynamic typing, where you check the type at runtime using `select type`.

```fortran
subroutine add_argument(name, nargs)
    character(len=*), intent(in) :: name
    class(*), intent(in)         :: nargs
    
    select type (nargs)
        type is (integer)
            print *, "Integer passed: ", nargs
        type is (character(len=*))
            print *, "String passed: ", nargs
            ! Check for '+', '*', '?' here
        class default
            print *, "Unsupported type for nargs"
    end select
end subroutine add_argument
```

### Summary Recommendation
Use **Generic Interfaces (Approach 1)**. It is universally considered the cleanest and safest way to handle this in Fortran. It avoids runtime string conversion overhead, catches type errors at compile-time, and perfectly mimics the convenience of Python's `argparse` API from the caller's perspective.
