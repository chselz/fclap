!> String similarity used to suggest corrections for mistyped input
module fclap_utils_similarity
   use fclap_utils_accuracy, only : wp
   implicit none
   private

   public :: jaro_similarity, closest_match


   !> Minimum Jaro similarity for a candidate to be suggested, as in clap
   real(wp), parameter :: suggestion_threshold = 0.7_wp


contains


!> Jaro similarity of two strings, 1 for identical and 0 for unrelated text.
!>
!> Plain Jaro is used rather than Jaro-Winkler, which rates strings sharing
!> a long common prefix as identical regardless of their endings.
pure function jaro_similarity(left, right) result(similarity)

   !> First string, trailing blanks are ignored
   character(len=*), intent(in) :: left

   !> Second string, trailing blanks are ignored
   character(len=*), intent(in) :: right

   !> Similarity in the closed interval [0, 1]
   real(wp) :: similarity

   logical, allocatable :: left_matched(:), right_matched(:)
   integer :: left_length, right_length, window
   integer :: i, j, first, last, matches, transpositions

   left_length = len_trim(left)
   right_length = len_trim(right)
   if (left_length == 0 .and. right_length == 0) then
      similarity = 1.0_wp
      return
   end if
   similarity = 0.0_wp
   if (left_length == 0 .or. right_length == 0) return

   window = max(0, max(left_length, right_length) / 2 - 1)
   allocate(left_matched(left_length), source=.false.)
   allocate(right_matched(right_length), source=.false.)

   matches = 0
   do i = 1, left_length
      first = max(1, i - window)
      last = min(right_length, i + window)
      do j = first, last
         if (right_matched(j)) cycle
         if (left(i:i) /= right(j:j)) cycle
         left_matched(i) = .true.
         right_matched(j) = .true.
         matches = matches + 1
         exit
      end do
   end do
   if (matches == 0) return

   transpositions = 0
   j = 1
   do i = 1, left_length
      if (.not. left_matched(i)) cycle
      do while (.not. right_matched(j))
         j = j + 1
      end do
      if (left(i:i) /= right(j:j)) transpositions = transpositions + 1
      j = j + 1
   end do

   similarity = (real(matches, wp) / real(left_length, wp) &
      & + real(matches, wp) / real(right_length, wp) &
      & + real(matches - transpositions / 2, wp) / real(matches, wp)) &
      & / 3.0_wp
end function jaro_similarity


!> Index of the candidate most similar to a mistyped token, zero if none is
!> similar enough to be worth suggesting. Ties select the first candidate.
pure function closest_match(token, candidates) result(best)

   !> Text that did not match any candidate exactly
   character(len=*), intent(in) :: token

   !> Valid spellings to choose from, trailing blanks are ignored
   character(len=*), intent(in) :: candidates(:)

   !> Index into `candidates`, or zero
   integer :: best

   real(wp) :: score, best_score
   integer :: i

   best = 0
   best_score = suggestion_threshold
   do i = 1, size(candidates)
      score = jaro_similarity(token, candidates(i))
      if (score > best_score) then
         best = i
         best_score = score
      end if
   end do
end function closest_match


end module fclap_utils_similarity
