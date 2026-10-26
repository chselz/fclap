!> Numerical storage size parameters for real and integer values
module fclap_utils_accuracy
   use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
   implicit none
   private

   public :: sp, wp, i1, i2, i4, i8, ip
   public :: widen_real

   !> Single precision real numbers
   integer, parameter :: sp = selected_real_kind(6)

   !> Double precision real numbers
   integer, parameter :: dp = selected_real_kind(15)

   !> Working precision for real numbers
   integer, parameter :: wp = dp

   !> Char length for integers
   integer, parameter :: i1 = selected_int_kind(2)

   !> Short length for integers
   integer, parameter :: i2 = selected_int_kind(4)

   !> Length of default integers
   integer, parameter :: i4 = selected_int_kind(9)

   !> Long length for integers
   integer, parameter :: i8 = selected_int_kind(18)

   !> Working precision for integers
   integer, parameter :: ip = i4


contains


!> Widen a single-precision real to working precision through its shortest
!> round-trip decimal text.
!>
!> Exact conversion keeps the binary value, so a default-kind literal `0.1`
!> becomes 0.10000000149011612. This returns the working-precision value
!> nearest to the shortest decimal that reads back to `value`, here 0.1, and
!> narrowing the result gives `value` again. Zero and non-finite values are
!> converted exactly.
elemental function widen_real(value) result(widened)

   !> Single-precision value to widen
   real(sp), intent(in) :: value

   real(wp) :: widened
   character(len=32) :: text
   character(len=16) :: edit
   real(sp) :: back
   integer :: digits, io_status

   widened = real(value, kind=wp)
   if (.not. ieee_is_finite(value) .or. value == 0.0_sp) return
   do digits = 1, precision(value) + 3
      write(edit, "(a,i0,a)") "(es32.", digits - 1, "e3)"
      write(text, edit) value
      read(text, *, iostat=io_status) back
      if (io_status /= 0 .or. back /= value) cycle
      read(text, *, iostat=io_status) widened
      ! Keep the exact value should double rounding ever miss `value`.
      if (io_status /= 0 .or. real(widened, kind=sp) /= value) then
         widened = real(value, kind=wp)
      end if
      return
   end do
end function widen_real


end module fclap_utils_accuracy
