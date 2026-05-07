To implement this kind of flexible argument parser in Fortran without relying on string matching for actions (like `"store_true"`), you would leverage **Fortran 2003+ Object-Oriented Programming (OOP)** features. 

Instead of passing strings, you would use **Polymorphic Derived Types** (`class(...)`). Every action would indeed be represented by a derived type (a class), and you would use **factory functions** (constructors) to create and return instances of these types to pass into your `add_argument` subroutine.

Here is how you would architect this in Fortran:

### 1. Define an Abstract Base `Action` Type
First, define an abstract base class that all actions will inherit from. It enforces that every action must have a `parse` or `execute` method.

```fortran
module argparse_mod
    implicit none

    ! Abstract base type for all argument actions
    type, abstract :: Action
    contains
        ! Deferred procedure that must be implemented by child types
        procedure(action_parse_iface), deferred, pass :: parse
    end type Action

    abstract interface
        subroutine action_parse_iface(this, value_str, success, error_msg)
            import :: Action
            class(Action), intent(inout) :: this
            character(len=*), intent(in) :: value_str
            logical, intent(out) :: success
            character(len=:), allocatable, intent(out) :: error_msg
        end subroutine action_parse_iface
    end interface
```

### 2. Implement the `StoreTrue` Action
Instead of passing `"store_true"`, you create a specific type for it. It ignores the input string (since it's a flag) and just sets an internal boolean state to `.true.`.

```fortran
    ! Action for "store_true"
    type, extends(Action) :: StoreTrueAction
        logical :: value = .false.
    contains
        procedure, pass :: parse => parse_store_true
    end type StoreTrueAction

    interface StoreTrueAction
        module procedure new_store_true
    end interface
```

### 3. Implement the `NotLessThan` Action
For `action_not_less_than(-10.0)`, you create a type that stores the `min_value` state upon initialization. When its `parse` method is called, it converts the string to a real number and checks it against `min_value`.

```fortran
    ! Action for "not_less_than"
    type, extends(Action) :: NotLessThanAction
        real :: min_value
        real :: parsed_value
    contains
        procedure, pass :: parse => parse_not_less_than
    end type NotLessThanAction

    ! Factory function (Constructor)
    interface NotLessThanAction
        module procedure new_not_less_than
    end interface
```

### 4. The `add_argument` Subroutine
Your parser's `add_argument` method will accept a polymorphic variable (`class(Action)`) instead of a string for the action. 

```fortran
    type :: ArgumentParser
        ! Array of pointers or allocatable polymorphic types to hold arguments
    contains
        procedure :: add_argument
    end type ArgumentParser

contains

    ! The add_argument routine
    subroutine add_argument(this, name, action, help_text)
        class(ArgumentParser), intent(inout) :: this
        character(len=*), intent(in) :: name
        class(Action), allocatable, intent(in) :: action  ! Polymorphic input
        character(len=*), intent(in), optional :: help_text
        
        ! Store the allocated action into the parser's internal list...
    end subroutine add_argument
```

### 5. Factory Functions (Constructors)
To answer your question: **Yes, every action would essentially be a factory function that returns a type.**

```fortran
    function new_store_true() result(res)
        type(StoreTrueAction) :: res
        res%value = .false.
    end function new_store_true

    function new_not_less_than(min_val) result(res)
        real, intent(in) :: min_val
        type(NotLessThanAction) :: res
        res%min_value = min_val
    end function new_not_less_than

    ! Implementation of the parsing logic
    subroutine parse_not_less_than(this, value_str, success, error_msg)
        class(NotLessThanAction), intent(inout) :: this
        character(len=*), intent(in) :: value_str
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        ! 1. Convert value_str to real (omitted for brevity)
        ! 2. Check against this%min_value
        if (parsed_val < this%min_value) then
            success = .false.
            error_msg = "Value is less than minimum allowed."
        else
            this%parsed_value = parsed_val
            success = .true.
        end if
    end subroutine parse_not_less_than
end module argparse_mod
```

### How you would use it in practice:
When building your parser in your main program, it would look remarkably similar to the Python code, but entirely strongly-typed:

```fortran
program main
    use argparse_mod
    type(ArgumentParser) :: p

    ! Equivalent to: p.add_argument("--strict", action="store_true")
    call p%add_argument("--strict", action=StoreTrueAction())

    ! Equivalent to: p.add_argument("-c", action=action_not_less_than(-10.0))
    call p%add_argument("-c", action=NotLessThanAction(-10.0))
    
end program main
```

### Summary
By using **Polymorphism (`class(Action)`)**, you completely eliminate string-parsing for the `action` argument. 
1. **"store_true"** becomes a standalone derived type (`StoreTrueAction`) that just knows how to set itself to `.true.`.
2. **`action_not_less_than`** becomes a parameterized derived type (`NotLessThanAction`) constructed by a factory function, holding the `-10.0` as internal state to be used during the parsing step.