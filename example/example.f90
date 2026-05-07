program example_to_lower
  use stdlib_string_type
  use stdlib_strings, only : strip
  implicit none
  
  type(string_type) :: string, lowercase_string

  string = "      Lowercase This String   "
  lowercase_string = to_lower(string) ! returns "lowercase this string"
  lowercase_string = strip(lowercase_string) ! returns "lowercase this string"

  print *, "Original string: ", string
  print *, "Lowercase string: ", lowercase_string
end program example_to_lower