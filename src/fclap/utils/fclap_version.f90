!> Version information on fclap
module fclap_version
   implicit none(type, external)
   private

   public :: get_fclap_version
   public :: fclap_version_string, fclap_version_compact


   !> String representation of the fclap version
   character(len=*), parameter :: fclap_version_string = "0.1.0"

   !> Major version number of the above fclap version
   integer, parameter :: fclap_major = 0

   !> Minor version number of the above fclap version
   integer, parameter :: fclap_minor = 1

   !> Patch version number of the above fclap version
   integer, parameter :: fclap_patch = 0

   !> Compact numeric representation of the fclap version
   integer, parameter :: fclap_version_compact = &
      & fclap_major*10000 + fclap_minor*100 + fclap_patch


contains


!> Getter function to retrieve fclap version
subroutine get_fclap_version(major, minor, patch, string)

   !> Major version number of the fclap version
   integer, intent(out), optional :: major

   !> Minor version number of the fclap version
   integer, intent(out), optional :: minor

   !> Patch version number of the fclap version
   integer, intent(out), optional :: patch

   !> String representation of the fclap version
   character(len=:), allocatable, intent(out), optional :: string

   if (present(major)) then
      major = fclap_major
   end if
   if (present(minor)) then
      minor = fclap_minor
   end if
   if (present(patch)) then
      patch = fclap_patch
   end if
   if (present(string)) then
      string = fclap_version_string
   end if

end subroutine get_fclap_version


end module fclap_version
