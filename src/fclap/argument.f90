!> @file argument.f90
!> @brief Derived type representing a single registered argument.
!>
!> @details An Argument is the internal record created for every call to
!> add_argument. It holds everything the parser needs at parse-time and
!> everything the formatter needs at help-time. It is intentionally a plain
!> data type (no parsing logic lives here) so that it can be held in
!> polymorphic arrays and passed across the actions/groups/formatter layers
!> without circular dependencies.
!>
!> Ownership model:
!>   - The ArgumentParser owns an allocatable array of Argument.
!>   - Groups hold integer index lists into that array (no copies).
!>   - Actions are stored on the Argument itself via class(ActionType).

module fclap_argument
    use fclap_nargs,           only : NARGS_ONE, nargs_is_valid, nargs_to_string
    use fclap_actions_abstract, only : ActionType
    implicit none
    private

    public :: Argument
    public :: MAX_NAMES

    !> Maximum number of aliases a single argument may have (e.g. -v, --verbose).
    !> Four covers every realistic case; adjust if truly needed.
    integer, parameter :: MAX_NAMES = 4

    ! =========================================================================
    !> @brief A single registered command-line argument.
    !>
    !> Populated once during add_argument and afterwards treated as read-only
    !> by the parser, formatter, and action layer.
    ! =========================================================================
    type :: Argument
        ! -----------------------------------------------------------------
        ! Identity
        ! -----------------------------------------------------------------

        !> All flag strings for this argument, e.g. ["-v", "--verbose", ""].
        !> Entries beyond the last alias are empty strings; use name_count
        !> to know how many are active.
        character(len=:), allocatable :: names(:)

        !> Number of active entries in names (1 .. MAX_NAMES).
        integer :: name_count = 0

        !> The key used to store the parsed value in the Namespace.
        !> Derived automatically from the longest --flag name if not set
        !> explicitly (leading dashes stripped, interior dashes become
        !> underscores so --dry-run becomes dest="dry_run").
        character(len=:), allocatable :: dest

        !> .true. when every name starts with '-' (i.e. this is an optional
        !> argument / flag).  .false. for positional arguments.
        logical :: is_optional = .false.

        ! -----------------------------------------------------------------
        ! Consumption / nargs
        ! -----------------------------------------------------------------

        !> Internal normalised nargs value.  Always one of the NARGS_*
        !> sentinels or a positive integer.  Set via the nargs_* constructors
        !> in fclap_nargs so that no other module needs to do select type.
        integer :: nargs = NARGS_ONE

        ! -----------------------------------------------------------------
        ! Type and value
        ! -----------------------------------------------------------------

        !> The expected value type: "string" (default), "integer", "real",
        !> "logical".  Used by the default StoreAction to coerce parsed
        !> tokens.
        character(len=:), allocatable :: data_type

        !> Default value, stored polymorphically so it can hold integer,
        !> real, logical, or character without a wrapper type.
        class(*), allocatable :: default_val

        !> Constant value used by store_const / store_true / store_false.
        !> Kept separate from default_val for clarity in action dispatch.
        class(*), allocatable :: const_val

        !> Allowed values.  If allocated and non-empty, the parser rejects
        !> any token not found in this list.
        character(len=:), allocatable :: choices(:)

        !> The action to run when this argument is matched.
        !> Defaults to a StoreAction when not supplied by the caller.
        class(ActionType), allocatable :: action

        ! -----------------------------------------------------------------
        ! Help / display metadata
        ! -----------------------------------------------------------------

        !> Human-readable description for the help page.
        character(len=:), allocatable :: help

        !> Metavariable name shown in usage strings (e.g. FILE, N).
        !> Defaults to dest in upper-case when not provided.
        character(len=:), allocatable :: metavar

        !> .true. when the argument must appear on the command line.
        !> Positionals are always required; optional flags default to .false.
        logical :: required = .false.

        !> .true. when the argument should appear in help output.
        !> Set to .false. to hide internal/developer flags.
        logical :: visible = .true.

        !> If non-empty, printed as a deprecation notice when this argument
        !> is encountered during parsing.
        character(len=:), allocatable :: deprecated_msg

        !> If non-empty, the argument is treated as removed: the parser
        !> emits a fatal error with this message if the flag is used.
        character(len=:), allocatable :: removed_msg

    contains
        ! -----------------------------------------------------------------
        ! Initialisation
        ! -----------------------------------------------------------------
        procedure :: init               => argument_init

        ! -----------------------------------------------------------------
        ! Derived-value helpers (pure, no side effects)
        ! -----------------------------------------------------------------

        !> Return .true. if no name starts with '-' (positional argument).
        procedure :: is_positional      => argument_is_positional

        !> Return the longest --flag name (or the sole positional name).
        procedure :: primary_name       => argument_primary_name

        !> Derive the dest key from the primary name: strip leading dashes,
        !> replace interior '-' with '_'.
        procedure :: derive_dest        => argument_derive_dest

        !> Return the metavar string, defaulting to upper-case dest.
        procedure :: effective_metavar  => argument_effective_metavar

        !> Return a display-ready nargs annotation, e.g. "N", "?", "+".
        procedure :: nargs_display      => argument_nargs_display

        !> Return .true. when this argument accepts zero or more values
        !> (i.e. its result type is a list).
        procedure :: produces_list      => argument_produces_list

    end type Argument

contains

    ! =========================================================================
    ! Initialisation
    ! =========================================================================

    !> @brief Populate an Argument from the raw parameters of add_argument.
    !>
    !> @param self          The Argument to initialise.
    !> @param names         Array of flag strings (e.g. ["-v","--verbose"]).
    !> @param nargs_val     Already-normalised nargs integer from fclap_nargs.
    !> @param data_type     Optional type hint; defaults to "string".
    !> @param dest          Optional explicit dest key.
    !> @param help          Optional help string.
    !> @param metavar       Optional metavar override.
    !> @param required      Optional required flag.
    !> @param visible       Optional visibility flag.
    !> @param deprecated_msg Optional deprecation message.
    !> @param removed_msg   Optional removal message.
    subroutine argument_init(self, names, nargs_val, data_type, dest, &
                             help, metavar, required, visible,         &
                             deprecated_msg, removed_msg)
        class(Argument),  intent(inout)        :: self
        character(len=*), intent(in)           :: names(:)
        integer,          intent(in)           :: nargs_val
        character(len=*), intent(in), optional :: data_type
        character(len=*), intent(in), optional :: dest
        character(len=*), intent(in), optional :: help
        character(len=*), intent(in), optional :: metavar
        logical,          intent(in), optional :: required
        logical,          intent(in), optional :: visible
        character(len=*), intent(in), optional :: deprecated_msg
        character(len=*), intent(in), optional :: removed_msg

        integer :: i, n

        ! ---- names ----------------------------------------------------------
        n = min(size(names), MAX_NAMES)
        self%name_count = n

        ! Store as a fixed-rank allocatable; each element gets its own length.
        ! We allocate as a deferred-length array of the longest name's length
        ! so every slot is the same declared length — required by the standard.
        ! Individual trimming is handled in accessor routines.
        allocate(character(len=len(names(1))) :: self%names(n))
        do i = 1, n
            self%names(i) = trim(names(i))
        end do

        ! ---- optional/positional classification -----------------------------
        if (n > 0) then
            self%is_optional = (len_trim(names(1)) > 0 .and. names(1)(1:1) == '-')
        end if

        ! ---- nargs ----------------------------------------------------------
        self%nargs = nargs_val   ! already validated by the caller

        ! ---- data_type ------------------------------------------------------
        if (present(data_type)) then
            self%data_type = trim(data_type)
        else
            self%data_type = "string"
        end if

        ! ---- dest -----------------------------------------------------------
        if (present(dest)) then
            self%dest = trim(dest)
        else
            self%dest = self%derive_dest()
        end if

        ! ---- help / display -------------------------------------------------
        if (present(help))           self%help           = trim(help)
        if (present(metavar))        self%metavar        = trim(metavar)
        if (present(deprecated_msg)) self%deprecated_msg = trim(deprecated_msg)
        if (present(removed_msg))    self%removed_msg    = trim(removed_msg)

        ! ---- flags ----------------------------------------------------------
        if (present(required)) then
            self%required = required
        else
            ! Positionals are always required unless the caller overrides
            self%required = .not. self%is_optional
        end if

        if (present(visible)) self%visible = visible

    end subroutine argument_init

    ! =========================================================================
    ! Pure derived-value helpers
    ! =========================================================================

    !> @brief Return .true. when this argument is positional (no leading dash).
    pure logical function argument_is_positional(self)
        class(Argument), intent(in) :: self
        argument_is_positional = .not. self%is_optional
    end function argument_is_positional

    ! -------------------------------------------------------------------------

    !> @brief Return the "primary" name used for dest derivation and display.
    !>
    !> For optional arguments this is the longest name (usually the --long
    !> form).  For positionals it is the only name.
    function argument_primary_name(self) result(name)
        class(Argument),          intent(in) :: self
        character(len=:), allocatable        :: name
        integer :: i, best, best_len, cur_len

        if (self%name_count == 0) then
            name = ""
            return
        end if

        best     = 1
        best_len = len_trim(self%names(1))

        do i = 2, self%name_count
            cur_len = len_trim(self%names(i))
            if (cur_len > best_len) then
                best     = i
                best_len = cur_len
            end if
        end do

        name = trim(self%names(best))
    end function argument_primary_name

    ! -------------------------------------------------------------------------

    !> @brief Derive the dest key from the primary name.
    !>
    !> Leading '-' characters are stripped; interior '-' become '_'.
    !> Examples:
    !>   "--dry-run"  =>  "dry_run"
    !>   "-v"         =>  "v"
    !>   "filename"   =>  "filename"
    function argument_derive_dest(self) result(dest)
        class(Argument),          intent(in) :: self
        character(len=:), allocatable        :: dest
        character(len=:), allocatable        :: raw
        integer :: i, start

        raw = self%primary_name()
        if (len_trim(raw) == 0) then
            dest = ""
            return
        end if

        ! Skip leading dashes
        start = 1
        do while (start <= len_trim(raw) .and. raw(start:start) == '-')
            start = start + 1
        end do

        dest = raw(start:len_trim(raw))

        ! Replace interior '-' with '_'
        do i = 1, len(dest)
            if (dest(i:i) == '-') dest(i:i) = '_'
        end do
    end function argument_derive_dest

    ! -------------------------------------------------------------------------

    !> @brief Return the metavar to display in usage strings.
    !>
    !> Uses the explicit metavar if set; otherwise upper-cases the dest.
    function argument_effective_metavar(self) result(mv)
        class(Argument),          intent(in) :: self
        character(len=:), allocatable        :: mv
        integer :: i
        character :: c

        if (allocated(self%metavar)) then
            mv = self%metavar
            return
        end if

        if (.not. allocated(self%dest)) then
            mv = "VALUE"
            return
        end if

        ! Upper-case the dest string character by character
        mv = self%dest
        do i = 1, len(mv)
            c = mv(i:i)
            if (c >= 'a' .and. c <= 'z') mv(i:i) = achar(iachar(c) - 32)
        end do
    end function argument_effective_metavar

    ! -------------------------------------------------------------------------

    !> @brief Return the nargs annotation string used in usage output.
    !>
    !> Returns the result of nargs_to_string from fclap_nargs.
    function argument_nargs_display(self) result(str)
        class(Argument),          intent(in) :: self
        character(len=:), allocatable        :: str
        str = nargs_to_string(self%nargs)
    end function argument_nargs_display

    ! -------------------------------------------------------------------------

    !> @brief Return .true. when the argument collects a variable-length list.
    !>
    !> This is used by the Namespace layer to decide whether to store a
    !> scalar or an array under self%dest.
    pure logical function argument_produces_list(self)
        class(Argument), intent(in) :: self
        argument_produces_list = (self%nargs == -3 .or.  &  ! NARGS_ZERO_OR_MORE
                                  self%nargs == -4 .or.  &  ! NARGS_ONE_OR_MORE
                                  self%nargs == -5 .or.  &  ! NARGS_REMAINDER
                                  self%nargs >= 2)           ! exact count >= 2
    end function argument_produces_list

end module fclap_argument