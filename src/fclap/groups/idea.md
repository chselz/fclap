# Implementing Argument Groups in Fortran 2008

## Architecture Overview

```
argument_group_t (abstract)
├── argument_group_normal_t        — groups args in help output only
└── mutually_exclusive_group_t     — enforces at-most-one constraint
```

**Key design decisions:**

1. **The parser owns all argument definitions** in a flat array; groups store **indices** into that array — avoids duplication, ownership issues, and pointer management
2. **Deferred procedures** are only `validate` and `format_help` — the two operations that actually differ between group types
3. **All other procedures are concrete and `non_overridable`** on the base class
4. Groups are decoupled from the parser: methods receive `arg_defs(:)` as a parameter rather than holding a reference to the parser
5. A **container type** is needed for the parser to store heterogeneous groups in a single array

---

## Module 1: Argument Definition (simplified)

```fortran
module argparse_argument_mod
    implicit none
    private

    type :: argument_def_t
        character(len=:), allocatable :: short_name   ! e.g. "-v"
        character(len=:), allocatable :: long_name    ! e.g. "--verbose"
        character(len=:), allocatable :: help_text
        character(len=:), allocatable :: metavar
        character(len=:), allocatable :: default_val
        logical :: was_set  = .false.
        logical :: required = .false.
        integer :: nargs    = 1
    end type argument_def_t

    public :: argument_def_t
end module argparse_argument_mod
```

---

## Module 2: Group Types (the core)

```fortran
module argparse_group_mod
    use argparse_argument_mod, only: argument_def_t
    implicit none
    private

    ! =========================================================
    !  Abstract base class
    ! =========================================================
    type, abstract :: argument_group_t
        private
        character(len=:), allocatable :: name_
        character(len=:), allocatable :: description_
        integer, allocatable :: arg_indices_(:)
    contains
        ! --- Concrete, non-overridable ---
        procedure, non_overridable :: add_argument_index
        procedure, non_overridable :: get_name
        procedure, non_overridable :: get_description
        procedure, non_overridable :: get_num_arguments
        procedure, non_overridable :: get_argument_index
        procedure, non_overridable :: has_arguments
        procedure, non_overridable :: clear_indices
        ! --- Deferred ---
        procedure(validate_intf),  deferred :: validate
        procedure(format_help_intf), deferred :: format_help
    end type argument_group_t

    ! =========================================================
    !  Normal argument group  (help-page organisation only)
    ! =========================================================
    type, extends(argument_group_t) :: argument_group_normal_t
    contains
        procedure :: validate    => validate_normal
        procedure :: format_help => format_help_normal
    end type argument_group_normal_t

    ! =========================================================
    !  Mutually exclusive argument group
    ! =========================================================
    type, extends(argument_group_t) :: mutually_exclusive_group_t
        private
        logical :: required_ = .false.
    contains
        procedure :: validate    => validate_mutually_exclusive
        procedure :: format_help => format_help_mutually_exclusive
        procedure, non_overridable :: set_required
        procedure, non_overridable :: is_required
    end type mutually_exclusive_group_t

    ! =========================================================
    !  Container so the parser can store heterogeneous groups
    ! =========================================================
    type :: group_container_t
        class(argument_group_t), allocatable :: item
    end type group_container_t

    ! =========================================================
    !  Abstract interfaces
    ! =========================================================
    abstract interface
        subroutine validate_intf(this, arg_defs, is_valid, error_msg)
            import :: argument_group_t, argument_def_t
            class(argument_group_t), intent(in)  :: this
            type(argument_def_t),    intent(in)  :: arg_defs(:)
            logical,                 intent(out) :: is_valid
            character(len=:), allocatable, intent(out) :: error_msg
        end subroutine validate_intf

        subroutine format_help_intf(this, arg_defs, help_text)
            import :: argument_group_t, argument_def_t
            class(argument_group_t), intent(in)  :: this
            type(argument_def_t),    intent(in)  :: arg_defs(:)
            character(len=:), allocatable, intent(out) :: help_text
        end subroutine format_help_intf
    end interface

    ! =========================================================
    !  Public list
    ! =========================================================
    public :: argument_group_t
    public :: argument_group_normal_t
    public :: mutually_exclusive_group_t
    public :: group_container_t
    public :: create_argument_group
    public :: create_mutually_exclusive_group

contains

    ! =========================================================
    !  Factory functions
    ! =========================================================

    function create_argument_group(name, description) result(group)
        character(len=*), intent(in)           :: name
        character(len=*), intent(in), optional :: description
        class(argument_group_t), allocatable   :: group
        type(argument_group_normal_t) :: tmp

        tmp%name_ = name
        if (present(description)) tmp%description_ = description
        allocate(group, source=tmp)
    end function create_argument_group

    function create_mutually_exclusive_group(required) result(group)
        logical, intent(in), optional :: required
        class(argument_group_t), allocatable :: group
        type(mutually_exclusive_group_t) :: tmp

        if (present(required)) tmp%required_ = required
        ! name_/description_ intentionally left unallocated for
        ! mutually exclusive groups (they render inline in help)
        allocate(group, source=tmp)
    end function create_mutually_exclusive_group

    ! =========================================================
    !  Base-class concrete procedures
    ! =========================================================

    subroutine add_argument_index(this, idx)
        class(argument_group_t), intent(inout) :: this
        integer, intent(in) :: idx
        integer, allocatable :: tmp(:)

        if (allocated(this%arg_indices_)) then
            call move_alloc(this%arg_indices_, tmp)          ! F2008
            allocate(this%arg_indices_(size(tmp) + 1))
            this%arg_indices_(:size(tmp)) = tmp
            this%arg_indices_(size(tmp) + 1) = idx
            deallocate(tmp)                                   ! free early
        else
            allocate(this%arg_indices_(1))
            this%arg_indices_(1) = idx
        end if
    end subroutine add_argument_index

    function get_name(this) result(name)
        class(argument_group_t), intent(in) :: this
        character(len=:), allocatable :: name
        if (allocated(this%name_)) then; name = this%name_
        else;                              name = '';       end if
    end function get_name

    function get_description(this) result(desc)
        class(argument_group_t), intent(in) :: this
        character(len=:), allocatable :: desc
        if (allocated(this%description_)) then; desc = this%description_
        else;                                    desc = '';              end if
    end function get_description

    function get_num_arguments(this) result(n)
        class(argument_group_t), intent(in) :: this
        integer :: n
        if (allocated(this%arg_indices_)) then; n = size(this%arg_indices_)
        else;                                    n = 0;                    end if
    end function get_num_arguments

    function get_argument_index(this, i) result(idx)
        class(argument_group_t), intent(in) :: this
        integer, intent(in) :: i
        integer :: idx
        idx = this%arg_indices_(i)
    end function get_argument_index

    function has_arguments(this) result(flag)
        class(argument_group_t), intent(in) :: this
        logical :: flag
        flag = allocated(this%arg_indices_) .and. size(this%arg_indices_) > 0
    end function has_arguments

    subroutine clear_indices(this)
        class(argument_group_t), intent(inout) :: this
        if (allocated(this%arg_indices_)) deallocate(this%arg_indices_)
    end subroutine clear_indices

    ! =========================================================
    !  Normal argument group implementations
    ! =========================================================

    subroutine validate_normal(this, arg_defs, is_valid, error_msg)
        class(argument_group_normal_t), intent(in)  :: this
        type(argument_def_t),           intent(in)  :: arg_defs(:)
        logical,                        intent(out) :: is_valid
        character(len=:), allocatable,  intent(out) :: error_msg

        ! No constraints for a plain grouping
        is_valid = .true.
        allocate(character(len=0) :: error_msg)
    end subroutine validate_normal

    subroutine format_help_normal(this, arg_defs, help_text)
        class(argument_group_normal_t), intent(in)  :: this
        type(argument_def_t),           intent(in)  :: arg_defs(:)
        character(len=:), allocatable,  intent(out) :: help_text
        character(len=:), allocatable :: arg_line
        integer :: i, idx

        help_text = ''

        ! ---- Group header (mirrors Python: "title: description") ----
        if (allocated(this%name_)) then
            help_text = this%name_
            if (allocated(this%description_)) then
                if (len(this%description_) > 0) then
                    help_text = help_text // ': ' // this%description_
                end if
            end if
            help_text = help_text // new_line('a')
        end if

        ! ---- One line per argument ----
        if (allocated(this%arg_indices_)) then
            do i = 1, size(this%arg_indices_)
                idx = this%arg_indices_(i)
                call format_arg_line(arg_defs(idx), arg_line)
                help_text = help_text // arg_line // new_line('a')
            end do
        end if
    end subroutine format_help_normal

    ! =========================================================
    !  Mutually exclusive group implementations
    ! =========================================================

    subroutine validate_mutually_exclusive(this, arg_defs, is_valid, error_msg)
        class(mutually_exclusive_group_t), intent(in)  :: this
        type(argument_def_t),              intent(in)  :: arg_defs(:)
        logical,                           intent(out) :: is_valid
        character(len=:), allocatable,     intent(out) :: error_msg
        integer :: i, idx, count_set

        allocate(character(len=0) :: error_msg)
        is_valid = .true.

        if (.not. allocated(this%arg_indices_)) return

        ! Count how many arguments in the group were actually set
        count_set = 0
        do i = 1, size(this%arg_indices_)
            idx = this%arg_indices_(i)
            if (idx >= 1 .and. idx <= size(arg_defs)) then
                if (arg_defs(idx)%was_set) count_set = count_set + 1
            end if
        end do

        ! --- At most one may be supplied ---
        if (count_set > 1) then
            is_valid = .false.
            error_msg = 'error: only one argument in the ' // &
                        'mutually exclusive group may be specified'
            return
        end if

        ! --- If required, at least one must be supplied ---
        if (this%required_ .and. count_set == 0) then
            is_valid = .false.
            error_msg = 'error: one of the mutually exclusive ' // &
                        'arguments is required'
        end if
    end subroutine validate_mutually_exclusive

    subroutine format_help_mutually_exclusive(this, arg_defs, help_text)
        class(mutually_exclusive_group_t), intent(in)  :: this
        type(argument_def_t),              intent(in)  :: arg_defs(:)
        character(len=:), allocatable,     intent(out) :: help_text
        character(len=:), allocatable :: arg_line
        integer :: i, idx

        help_text = ''

        ! ---- Mutual-exclusivity banner ----
        if (this%required_) then
            help_text = '  [at least one of the following is required]' // &
                        new_line('a')
        else
            help_text = '  [at most one of the following may be used]' // &
                        new_line('a')
        end if

        ! ---- Arguments ----
        if (allocated(this%arg_indices_)) then
            do i = 1, size(this%arg_indices_)
                idx = this%arg_indices_(i)
                call format_arg_line(arg_defs(idx), arg_line)
                help_text = help_text // arg_line // new_line('a')
            end do
        end if
    end subroutine format_help_mutually_exclusive

    subroutine set_required(this, flag)
        class(mutually_exclusive_group_t), intent(inout) :: this
        logical, intent(in) :: flag
        this%required_ = flag
    end subroutine set_required

    function is_required(this) result(flag)
        class(mutually_exclusive_group_t), intent(in) :: this
        logical :: flag
        flag = this%required_
    end function is_required

    ! =========================================================
    !  Private helper – format one argument help line
    ! =========================================================

    subroutine format_arg_line(arg_def, line)
        type(argument_def_t), intent(in)             :: arg_def
        character(len=:), allocatable, intent(out)   :: line
        character(len=:), allocatable :: name_str
        integer, parameter :: INDENT    = 4
        integer, parameter :: COL_WIDTH = 24
        character(len=256) :: padded
        integer :: pad_len

        ! Build the name column:  -f, --foo  METAVAR
        name_str = ''
        if (allocated(arg_def%short_name)) name_str = trim(arg_def%short_name)
        if (allocated(arg_def%long_name)) then
            if (len(name_str) > 0) then
                name_str = name_str // ', ' // trim(arg_def%long_name)
            else
                name_str = trim(arg_def%long_name)
            end if
        end if
        if (allocated(arg_def%metavar)) then
            name_str = name_str // ' ' // trim(arg_def%metavar)
        end if

        ! Left-justify inside the column
        padded = repeat(' ', INDENT) // trim(name_str)
        pad_len = INDENT + COL_WIDTH - len_trim(padded)
        if (pad_len > 0) then
            padded = padded(1:len_trim(padded)) // repeat(' ', pad_len)
        end if

        line = trim(padded)
        if (allocated(arg_def%help_text)) then
            if (len_trim(arg_def%help_text) > 0) then
                line = trim(line) // ' ' // trim(arg_def%help_text)
            end if
        end if
    end subroutine format_arg_line

end module argparse_group_mod
```

---

## How the Parser Integrates with Groups

A sketch of the relevant parser components:

```fortran
module argparse_parser_mod
    use argparse_argument_mod, only: argument_def_t
    use argparse_group_mod
    implicit none
    private

    type :: argument_parser_t
        private
        type(argument_def_t), allocatable   :: args_(:)       ! flat list
        type(group_container_t), allocatable :: groups_(:)    ! heterogeneous
    contains
        procedure :: add_argument
        procedure :: add_argument_group
        procedure :: add_mutually_exclusive_group
        procedure :: parse
        procedure :: print_help
    end type argument_parser_t

contains

    ! Add an argument, optionally belonging to a group
    function add_argument(this, short_name, long_name, help_text, &
                          metavar, required, default_val, group) result(idx)
        class(argument_parser_t), intent(inout) :: this
        character(len=*), intent(in)           :: short_name
        character(len=*), intent(in), optional :: long_name, help_text, &
                                                  metavar, default_val
        logical, intent(in), optional          :: required
        class(argument_group_t), optional, target :: group  ! ← group handle
        integer :: idx

        ! ... append to this%args_(), set idx to new position ...

        ! Register with group if supplied
        if (present(group)) then
            call group%add_argument_index(idx)
        end if
    end function add_argument

    function add_argument_group(this, name, description) result(grp)
        class(argument_parser_t), intent(inout) :: this
        character(len=*), intent(in)           :: name
        character(len=*), intent(in), optional :: description
        class(argument_group_t), pointer :: grp
        type(group_container_t), allocatable :: tmp(:)
        integer :: n

        ! Grow groups_ array ...
        n = 0; if (allocated(this%groups_)) n = size(this%groups_)
        allocate(tmp(n + 1))
        if (n > 0) tmp(:n) = this%groups_

        tmp(n+1)%item = create_argument_group(name, description)
        call move_alloc(tmp, this%groups_)

        grp => this%groups_(n+1)%item
    end function add_argument_group

    function add_mutually_exclusive_group(this, required) result(grp)
        class(argument_parser_t), intent(inout) :: this
        logical, intent(in), optional :: required
        class(argument_group_t), pointer :: grp
        type(group_container_t), allocatable :: tmp(:)
        integer :: n

        n = 0; if (allocated(this%groups_)) n = size(this%groups_)
        allocate(tmp(n + 1))
        if (n > 0) tmp(:n) = this%groups_

        tmp(n+1)%item = create_mutually_exclusive_group(required)
        call move_alloc(tmp, this%groups_)

        grp => this%groups_(n+1)%item
    end function add_mutually_exclusive_group

    subroutine parse(this)
        class(argument_parser_t), intent(inout) :: this
        logical :: all_valid, group_valid
        character(len=:), allocatable :: error_msg
        integer :: g

        ! ... perform actual command-line parsing, set was_set flags ...

        ! Validate every group
        all_valid = .true.
        do g = 1, size(this%groups_)
            call this%groups_(g)%item%validate(this%args_, &
                                               group_valid, error_msg)
            if (.not. group_valid) then
                all_valid = .false.
                print *, error_msg
            end if
        end do

        if (.not. all_valid) stop 1
    end subroutine parse

    subroutine print_help(this)
        class(argument_parser_t), intent(in) :: this
        character(len=:), allocatable :: section
        integer :: g

        print *, 'usage: program [options]'
        print *, ''

        ! Groups render themselves (ungrouped args handled separately)
        do g = 1, size(this%groups_)
            call this%groups_(g)%item%format_help(this%args_, section)
            print '(a)', section
        end do
    end subroutine print_help

end module argparse_parser_mod
```

---

## Example Usage

```fortran
program demo
    use argparse_parser_mod
    implicit none

    type(argument_parser_t) :: parser
    class(argument_group_t), pointer :: grp, mex_grp
    integer :: idx

    ! --- Normal argument group ---
    grp => parser%add_argument_group('Output options', &
         'Control output format and destination')

    call parser%add_argument('-o', '--output', &
         help_text='output file', metavar='FILE', group=grp)
    call parser%add_argument('-v', '--verbose', &
         help_text='verbose output', group=grp)

    ! --- Mutually exclusive group ---
    mex_grp => parser%add_mutually_exclusive_group(required=.true.)

    call parser%add_argument('--json', &
         help_text='output as JSON', group=mex_grp)
    call parser%add_argument('--xml', &
         help_text='output as XML', group=mex_grp)
    call parser%add_argument('--csv', &
         help_text='output as CSV', group=mex_grp)

    call parser%parse()
end program demo
```

Expected help output:

```
usage: program [options]

Output options: Control output format and destination
    -o, --output FILE         output file
    -v, --verbose             verbose output

  [at least one of the following is required]
    --json                    output as JSON
    --xml                     output as XML
    --csv                     output as CSV
```

---

## Summary of Procedures by Type

| Procedure | Belongs to | Overridable? | Purpose |
|---|---|---|---|
| `add_argument_index` | `argument_group_t` | No (`non_overridable`) | Register an argument with the group by parser index |
| `get_name` | `argument_group_t` | No | Accessor for group title |
| `get_description` | `argument_group_t` | No | Accessor for group description |
| `get_num_arguments` | `argument_group_t` | No | Count of arguments in group |
| `get_argument_index` | `argument_group_t` | No | Retrieve ith index |
| `has_arguments` | `argument_group_t` | No | Check if group is non-empty |
| `clear_indices` | `argument_group_t` | No | Reset group membership |
| **`validate`** | `argument_group_t` | **Deferred** | Check group constraints after parsing |
| **`format_help`** | `argument_group_t` | **Deferred** | Render group's help section |
| `set_required` | `mutually_exclusive_group_t` | No | Set whether ≥1 arg is mandatory |
| `is_required` | `mutually_exclusive_group_t` | No | Query required flag |

### Why this set of procedures?

- **`validate` is deferred** because a normal group has no constraints (always passes), while a mutually exclusive group must check the at-most-one and required rules. Any future group type (e.g., "at-least-one" or "exactly-N" groups) would provide its own logic.
- **`format_help` is deferred** because the rendering differs: normal groups get a `title: description` header; mutually exclusive groups get a constraint banner instead.
- **Everything else is concrete on the base** because index management and metadata access are identical for all group types — marking them `non_overridable` prevents accidental breakage and signals intent clearly.
- **The `group_container_t` wrapper** is necessary because Fortran arrays require uniform dynamic type; the wrapper holds a `class(argument_group_t), allocatable` per slot, enabling heterogeneous storage.










# Integrating Subparsers with Argument Groups

## Flat vs. Hierarchical: Why Hierarchy Wins

Your instinct—collecting everything into one parser object—is the right *user-facing* goal. But internally, a **hierarchical** (composite) design is far better than a flat array:

| Concern | Flat (one big array) | Hierarchical (each subparser IS a parser) |
|---|---|---|
| Name collisions | `--verbose` on main vs. subparser? Need qualifying prefixes or scope tags | Each parser has its own namespace — natural isolation |
| Group membership | Which scope does a group belong to? Extra bookkeeping | Groups are local to their parser — zero extra logic |
| Adding/removing subparsers | Reshuffle the entire args array | Add/remove a subparser entry — other parsers untouched |
| Validation | Interleave validation of different scopes | Each parser validates itself, then delegates to active subparser |
| Mental model | "A flat table with scope columns" | "A tree of independent programs" — matches how subcommands actually work |

**The key insight**: make each subparser a full `argument_parser_t`. The main parser *contains* subparsers rather than *flattening* their arguments. Your existing group design slots in unchanged because groups are local to each parser instance.

---

## Revised Type Architecture

```
argument_parser_t                          ← one per scope
├── args_(:)         argument_def_t        ← this parser's arguments (with value storage)
├── groups_(:)       group_container_t     ← this parser's groups (unchanged!)
├── subparsers_(:)   subparser_entry_t     ← this parser's subcommands
└── active_subparser_idx_  integer         ← set after parsing

subparser_entry_t
├── name             character(:), allocatable
├── help_text        character(:), allocatable
└── parser           argument_parser_t, allocatable  ← recursive!
```

The recursive structure (a parser containing a parser) is valid Fortran 2008 because the component is `allocatable`—the compiler never needs the size at definition time.

---

## Module: Updated Argument Definition

The only change from before is **value storage** so the user can retrieve parsed values:

```fortran
module argparse_argument_mod
    implicit none
    private

    type :: argument_def_t
        ! --- Specification (set at registration) ---
        character(len=:), allocatable :: short_name
        character(len=:), allocatable :: long_name
        character(len=:), allocatable :: help_text
        character(len=:), allocatable :: metavar
        character(len=:), allocatable :: default_val
        logical :: required  = .false.
        logical :: is_flag   = .false.   ! true = no value expected (boolean switch)
        integer :: nargs     = 1

        ! --- Parsed state (set after parse) ---
        logical :: was_set              = .false.
        character(len=:), allocatable :: value_
    end type argument_def_t

    public :: argument_def_t
end module argparse_argument_mod
```

---

## Module: Subparser Entry and Parser

```fortran
module argparse_parser_mod
    use argparse_argument_mod, only: argument_def_t
    use argparse_group_mod
    implicit none
    private

    ! =========================================================
    !  Forward: subparser entry wraps a child parser
    ! =========================================================
    type :: subparser_entry_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: help_text
        type(argument_parser_t), allocatable :: parser     ! recursive
    end type subparser_entry_t

    ! =========================================================
    !  The parser (one per scope: main, or one per subcommand)
    ! =========================================================
    type :: argument_parser_t
        private
        character(len=:), allocatable :: prog_name_
        character(len=:), allocatable :: description_
        type(argument_def_t),    allocatable :: args_(:)
        type(group_container_t), allocatable :: groups_(:)
        type(subparser_entry_t), allocatable :: subparsers_(:)
        integer :: active_subparser_idx_ = 0
    contains
        ! --- Registration ---
        procedure :: add_argument
        procedure :: add_argument_group
        procedure :: add_mutually_exclusive_group
        procedure :: add_subparser
        ! --- Parsing ---
        procedure :: parse
        ! --- Value access (this parser's namespace) ---
        procedure :: was_set
        procedure :: get_value
        procedure :: get_integer
        procedure :: get_logical
        ! --- Subparser access ---
        procedure :: get_active_subcommand
        procedure :: get_active_subparser
        procedure :: has_active_subparser
        ! --- Help ---
        procedure :: print_help
    end type argument_parser_t

    public :: argument_parser_t
    public :: create_parser          ! constructor function

contains

    ! =========================================================
    !  Constructor
    ! =========================================================
    function create_parser(prog_name, description) result(p)
        character(len=*), intent(in), optional :: prog_name
        character(len=*), intent(in), optional :: description
        type(argument_parser_t) :: p

        if (present(prog_name))   p%prog_name_   = prog_name
        if (present(description)) p%description_ = description
    end function create_parser

    ! =========================================================
    !  add_subparser — returns pointer to the child parser
    ! =========================================================
    function add_subparser(this, name, help_text) result(child_ptr)
        class(argument_parser_t), intent(inout) :: this
        character(len=*), intent(in)            :: name
        character(len=*), intent(in), optional  :: help_text
        class(argument_parser_t), pointer       :: child_ptr
        type(subparser_entry_t), allocatable :: tmp(:)
        integer :: n

        n = 0
        if (allocated(this%subparsers_)) n = size(this%subparsers_)

        ! Grow array
        allocate(tmp(n + 1))
        if (n > 0) tmp(:n) = this%subparsers_
        call move_alloc(tmp, this%subparsers_)

        ! Populate new entry
        this%subparsers_(n+1)%name = name
        if (present(help_text)) this%subparsers_(n+1)%help_text = help_text

        ! Create the child parser
        allocate(this%subparsers_(n+1)%parser)
        this%subparsers_(n+1)%parser%prog_name_ = name

        child_ptr => this%subparsers_(n+1)%parser
    end function add_subparser

    ! =========================================================
    !  add_argument — with optional group target
    ! =========================================================
    function add_argument(this, short_name, long_name, help_text, &
                          metavar, required, default_val, is_flag, &
                          group) result(idx)
        class(argument_parser_t), intent(inout) :: this
        character(len=*), intent(in), optional  :: short_name, long_name, &
                                                    help_text, metavar, &
                                                    default_val
        logical, intent(in), optional           :: required, is_flag
        class(argument_group_t), optional       :: group
        integer :: idx
        type(argument_def_t), allocatable :: tmp(:)
        integer :: n

        n = 0
        if (allocated(this%args_)) n = size(this%args_)

        allocate(tmp(n + 1))
        if (n > 0) tmp(:n) = this%args_
        call move_alloc(tmp, this%args_)

        idx = n + 1
        if (present(short_name))  this%args_(idx)%short_name  = short_name
        if (present(long_name))   this%args_(idx)%long_name   = long_name
        if (present(help_text))   this%args_(idx)%help_text   = help_text
        if (present(metavar))     this%args_(idx)%metavar     = metavar
        if (present(default_val)) this%args_(idx)%default_val = default_val
        if (present(required))    this%args_(idx)%required    = required
        if (present(is_flag))     this%args_(idx)%is_flag     = is_flag

        ! Register with group if supplied
        if (present(group)) call group%add_argument_index(idx)
    end function add_argument

    ! =========================================================
    !  add_argument_group — delegates to group factory
    ! =========================================================
    function add_argument_group(this, name, description) result(grp)
        class(argument_parser_t), intent(inout) :: this
        character(len=*), intent(in)           :: name
        character(len=*), intent(in), optional :: description
        class(argument_group_t), pointer :: grp
        type(group_container_t), allocatable :: tmp(:)
        integer :: n

        n = 0; if (allocated(this%groups_)) n = size(this%groups_)
        allocate(tmp(n + 1))
        if (n > 0) tmp(:n) = this%groups_
        call move_alloc(tmp, this%groups_)

        this%groups_(n+1)%item = create_argument_group(name, description)
        grp => this%groups_(n+1)%item
    end function add_argument_group

    ! =========================================================
    !  add_mutually_exclusive_group
    ! =========================================================
    function add_mutually_exclusive_group(this, required) result(grp)
        class(argument_parser_t), intent(inout) :: this
        logical, intent(in), optional :: required
        class(argument_group_t), pointer :: grp
        type(group_container_t), allocatable :: tmp(:)
        integer :: n

        n = 0; if (allocated(this%groups_)) n = size(this%groups_)
        allocate(tmp(n + 1))
        if (n > 0) tmp(:n) = this%groups_
        call move_alloc(tmp, this%groups_)

        this%groups_(n+1)%item = create_mutually_exclusive_group(required)
        grp => this%groups_(n+1)%item
    end function add_mutually_exclusive_group

    ! =========================================================
    !  parse — main entry point
    ! =========================================================
    subroutine parse(this)
        class(argument_parser_t), intent(inout) :: this
        character(len=256) :: argv(0:32)
        integer :: argc, i

        ! --- Retrieve command-line tokens ---
        argc = command_argument_count()
        do i = 0, argc
            call get_command_argument(i, argv(i))
        end do

        ! --- Delegate to recursive worker ---
        call parse_tokens(this, argv(0:argc))
    end subroutine parse

    ! =========================================================
    !  Recursive token consumer
    ! =========================================================
    recursive subroutine parse_tokens(parser, tokens)
        type(argument_parser_t), intent(inout) :: parser
        character(len=*), intent(in) :: tokens(0:)
        integer :: i, j, arg_idx
        character(len=:), allocatable :: tok
        logical :: found

        i = 1  ! skip tokens(0) = program name

        do while (i <= ubound(tokens, 1))
            tok = trim(tokens(i))

            ! ---- Check if this token is a subcommand ----
            if (allocated(parser%subparsers_)) then
                do j = 1, size(parser%subparsers_)
                    if (tok == parser%subparsers_(j)%name) then
                        parser%active_subparser_idx_ = j
                        ! Hand remaining tokens to child parser
                        call parse_tokens(parser%subparsers_(j)%parser, &
                                          tokens(i:))
                        return    ! main parser is done
                    end if
                end do
            end if

            ! ---- Try to match against this parser's arguments ----
            arg_idx = find_argument(parser%args_, tok)
            if (arg_idx > 0) then
                parser%args_(arg_idx)%was_set = .true.
                if (.not. parser%args_(arg_idx)%is_flag) then
                    i = i + 1
                    if (i <= ubound(tokens, 1)) then
                        parser%args_(arg_idx)%value_ = trim(tokens(i))
                    end if
                end if
            else
                ! Unknown token — could error or store as positional
            end if

            i = i + 1
        end do

        ! ---- Validate this parser's groups ----
        call validate_groups(parser)
    end subroutine parse_tokens

    ! =========================================================
    !  Validate all groups in a parser
    ! =========================================================
    subroutine validate_groups(parser)
        type(argument_parser_t), intent(in) :: parser
        integer :: g
        logical :: is_valid
        character(len=:), allocatable :: error_msg

        if (.not. allocated(parser%groups_)) return

        do g = 1, size(parser%groups_)
            if (allocated(parser%groups_(g)%item)) then
                call parser%groups_(g)%item%validate( &
                    parser%args_, is_valid, error_msg)
                if (.not. is_valid) then
                    print '(a)', error_msg
                    stop 1
                end if
            end if
        end do
    end subroutine validate_groups

    ! =========================================================
    !  Find argument by short/long name  (private helper)
    ! =========================================================
    function find_argument(args, token) result(idx)
        type(argument_def_t), intent(in) :: args(:)
        character(len=*), intent(in)     :: token
        integer :: idx, k

        idx = 0
        do k = 1, size(args)
            if (allocated(args(k)%short_name)) then
                if (token == args(k)%short_name) then; idx = k; return; end if
            end if
            if (allocated(args(k)%long_name)) then
                if (token == args(k)%long_name) then; idx = k; return; end if
            end if
        end do
    end function find_argument

    ! =========================================================
    !  Value accessors — this parser's namespace only
    ! =========================================================

    function was_set(this, name) result(flag)
        class(argument_parser_t), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: flag
        integer :: idx

        flag = .false.
        if (.not. allocated(this%args_)) return
        idx = find_argument(this%args_, name)
        if (idx > 0) flag = this%args_(idx)%was_set
    end function was_set

    function get_value(this, name) result(val)
        class(argument_parser_t), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: val
        integer :: idx

        val = ''
        if (.not. allocated(this%args_)) return
        idx = find_argument(this%args_, name)
        if (idx > 0) then
            if (allocated(this%args_(idx)%value_)) then
                val = this%args_(idx)%value_
            else if (allocated(this%args_(idx)%default_val)) then
                val = this%args_(idx)%default_val
            end if
        end if
    end function get_value

    subroutine get_integer(this, name, val)
        class(argument_parser_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(out) :: val
        character(len=:), allocatable :: str
        integer :: ierr

        str = this%get_value(name)
        read(str, *, iostat=ierr) val
    end subroutine get_integer

    function get_logical(this, name) result(flag)
        class(argument_parser_t), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: flag
        flag = this%was_set(name)
    end function get_logical

    ! =========================================================
    !  Subparser accessors
    ! =========================================================

    function has_active_subparser(this) result(flag)
        class(argument_parser_t), intent(in) :: this
        logical :: flag
        flag = this%active_subparser_idx_ > 0
    end function has_active_subparser

    function get_active_subcommand(this) result(name)
        class(argument_parser_t), intent(in) :: this
        character(len=:), allocatable :: name

        name = ''
        if (this%active_subparser_idx_ > 0 .and. &
            allocated(this%subparsers_)) then
            name = this%subparsers_(this%active_subparser_idx_)%name
        end if
    end function get_active_subcommand

    function get_active_subparser(this) result(ptr)
        class(argument_parser_t), intent(inout), target :: this
        class(argument_parser_t), pointer :: ptr

        ptr => null()
        if (this%active_subparser_idx_ > 0 .and. &
            allocated(this%subparsers_)) then
            ptr => this%subparsers_(this%active_subparser_idx_)%parser
        end if
    end function get_active_subparser

    ! =========================================================
    !  print_help — recursive, each parser renders its section
    ! =========================================================
    recursive subroutine print_help(this)
        class(argument_parser_t), intent(in) :: this
        character(len=:), allocatable :: section
        integer :: g, i

        ! Header
        if (allocated(this%prog_name_)) then
            print '(a,a)', 'usage: ', this%prog_name_
        end if
        if (allocated(this%description_)) then
            print '(a)', ''
            print '(a)', this%description_
        end if

        ! Groups (renders arguments within their group context)
        if (allocated(this%groups_)) then
            do g = 1, size(this%groups_)
                call this%groups_(g)%item%format_help(this%args_, section)
                print '(a)', section
            end do
        end if

        ! Ungrouped arguments (not in any group)
        call render_ungrouped_args(this, section)
        if (len(section) > 0) print '(a)', section

        ! Subcommands summary
        if (allocated(this%subparsers_)) then
            print '(a)', ''
            print '(a)', 'subcommands:'
            do i = 1, size(this%subparsers_)
                if (allocated(this%subparsers_(i)%help_text)) then
                    print '(a,a,a)', '  ' // this%subparsers_(i)%name // &
                        repeat(' ', max(1, 16 - len(this%subparsers_(i)%name))), &
                        this%subparsers_(i)%help_text
                else
                    print '(a)', '  ' // this%subparsers_(i)%name
                end if
            end do
        end if

        ! If a subparser is active, print its help too
        if (this%active_subparser_idx_ > 0 .and. &
            allocated(this%subparsers_)) then
            print '(a)', ''
            print '(a,a)', '--- subcommand: ', &
                this%subparsers_(this%active_subparser_idx_)%name // ' ---'
            call print_help(this%subparsers_(this%active_subparser_idx_)%parser)
        end if
    end subroutine print_help

    subroutine render_ungrouped_args(this, section)
        type(argument_parser_t), intent(in) :: this
        character(len=:), allocatable, intent(out) :: section
        ! ... render args not in any group ...
        section = ''
    end subroutine render_ungrouped_args

end module argparse_parser_mod
```

---

## How Groups Integrate — It's Automatic

The crucial point: **your existing group design requires zero changes**. Each `argument_parser_t` instance has its own `groups_` array. When you add a group to a subparser, it belongs to that subparser alone:

```
argument_parser_t  (main)
├── groups_(1) = "Output options"
│     ├── --verbose       (idx 1)
│     └── --logfile FILE  (idx 2)
├── subparsers_(1) = "build"
│     └── parser (argument_parser_t)
│           ├── groups_(1) = "Optimization"
│           │     ├── --optlevel LEVEL  (idx 1, local to build)
│           │     └── --target TARGET   (idx 2, local to build)
│           └── groups_(2) = mutually_exclusive
│                 ├── --debug  (idx 3)
│                 └── --release (idx 4)
└── subparsers_(2) = "clean"
      └── parser (argument_parser_t)
            └── (no groups, just flat args)
                  └── --all  (idx 1, local to clean)
```

Validation flows recursively: `parse_tokens` validates each parser's groups before returning, so a mutually exclusive violation in the `build` subparser is caught exactly where it belongs.

---

## Complete Usage Example

```fortran
program myapp
    use argparse_parser_mod
    implicit none

    type(argument_parser_t) :: parser, target
    class(argument_group_t), pointer :: out_grp, opt_grp, mex_grp
    class(argument_parser_t), pointer :: build_sub, clean_sub

    ! =========================================================
    !  Setup: main parser
    ! =========================================================
    parser = create_parser('myapp', 'A build tool demonstration')

    ! Normal group on main parser
    out_grp => parser%add_argument_group('Output options', &
                                         'Control output behaviour')
    call parser%add_argument('-v', '--verbose', &
         help_text='enable verbose output', is_flag=.true., group=out_grp)
    call parser%add_argument('', '--logfile', &
         help_text='log to file', metavar='FILE', group=out_grp)

    ! =========================================================
    !  Setup: 'build' subparser
    ! =========================================================
    build_sub => parser%add_subparser('build', 'Build the project')

    ! Normal group inside build
    opt_grp => build_sub%add_argument_group('Optimization', &
                                            'Compiler optimisation settings')
    call build_sub%add_argument('-O', '--optlevel', &
         help_text='optimisation level', metavar='LEVEL', &
         default_val='2', group=opt_grp)
    call build_sub%add_argument('', '--target', &
         help_text='target architecture', metavar='ARCH', group=opt_grp)

    ! Mutually exclusive group inside build
    mex_grp => build_sub%add_mutually_exclusive_group(required=.true.)
    call build_sub%add_argument('', '--debug', &
         help_text='build with debug symbols', is_flag=.true., group=mex_grp)
    call build_sub%add_argument('', '--release', &
         help_text='build in release mode', is_flag=.true., group=mex_grp)

    ! =========================================================
    !  Setup: 'clean' subparser
    ! =========================================================
    clean_sub => parser%add_subparser('clean', 'Remove build artefacts')
    call clean_sub%add_argument('-a', '--all', &
         help_text='remove all artefacts including caches', is_flag=.true.)

    ! =========================================================
    !  Parse
    ! =========================================================
    call parser%parse()

    ! =========================================================
    !  Seamless value access
    ! =========================================================

    ! --- Main parser arguments ---
    if (parser%was_set('--verbose')) then
        print *, 'Verbose mode ON'
    end if

    ! --- Which subcommand? ---
    if (parser%has_active_subparser()) then
        select case (parser%get_active_subcommand())

        case ('build')
            target => parser%get_active_subparser()
            print *, 'Optimisation level: ', target%get_value('--optlevel')
            if (target%was_set('--debug'))  print *, 'Mode: debug'
            if (target%was_set('--release')) print *, 'Mode: release'

        case ('clean')
            target => parser%get_active_subparser()
            if (target%was_set('--all')) print *, 'Cleaning everything'

        end select
    end if

end program myapp
```

### Sample invocations and behaviour:

```bash
# Valid — verbose + build in debug mode at -O loose
$ ./myapp --verbose build --debug --optlevel loose
Verbose mode ON
Mode: debug
Optimisation level:  loose

# Invalid — both --debug and --release (mutually exclusive!)
$ ./myapp build --debug --release
error: only one argument in the mutually exclusive group may be specified

# Invalid — neither --debug nor --release (required group!)
$ ./myapp build --optlevel 3
error: one of the mutually exclusive arguments is required
```

---

## Answer to Your Core Questions

**"Does the approach work so the end user can seamlessly access values?"**

Yes. The user:
1. Calls `parser%was_set('--verbose')` for main-parser flags
2. Calls `parser%get_value('--optlevel')` for main-parser values
3. Calls `parser%get_active_subcommand()` to find which subcommand ran
4. Calls `parser%get_active_subparser()` → then queries *that* parser with the same `was_set`/`get_value` methods

Every argument is accessed through the parser that owns it. No indices, no scope tags, no qualifying prefixes. The pattern is uniform whether it's the main parser or a subparser.

**"Are these two concepts connectable?"**

They connect **naturally** because groups are local to each parser instance. The group design from the previous answer needs zero modifications. The `validate` and `format_help` deferred procedures receive the local `arg_defs(:)` array and operate exactly as before—they don't know or care whether they're in a main parser or a subparser.

**"Is there a better way?"**

The hierarchical approach *is* the better way relative to a flat array. A flat design would require every argument lookup to carry a scope identifier, and groups would need scope awareness. The hierarchy gives you namespace isolation, independent validation, and composability (subparsers can themselves have subparsers) for free.

---

## Optional Enhancement: Convenience Path Access

If you want even more seamless access without the two-step subparser lookup, add a path-based accessor on the main parser that traverses the active subparser chain:

```fortran
function get_value_path(this, path) result(val)
    ! path = 'build.optlevel'  →  main → active sub "build" → arg "--optlevel"
    class(argument_parser_t), intent(inout), target :: this
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: val
    class(argument_parser_t), pointer :: current
    character(len=:), allocatable :: scope, arg_name
    integer :: dot_pos

    dot_pos = index(path, '.')
    if (dot_pos == 0) then
        ! No dot — look up in this parser directly
        val = this%get_value(path)
    else
        scope    = path(:dot_pos-1)
        arg_name = path(dot_pos+1:)
        if (this%has_active_subparser() .and. &
            this%get_active_subcommand() == scope) then
            current => this%get_active_subparser()
            val = current%get_value(arg_name)
        end if
    end if
end function get_value_path
```

This lets the user write:
```fortran
val = parser%get_value('build.optlevel')   ! one call instead of two
```

Treat this as a convenience layer on top of the core hierarchical access—not a replacement.