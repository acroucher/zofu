module zofu_str_utils
  !! String handling utilities.

  implicit none
  private

  public :: str_equal, str_lower, str_startswith, str_endswith

contains

!------------------------------------------------------------------------

  logical elemental function str_equal(a, b)
    !! Returns true if two character strings are equal.

    character(len = *), intent(in) :: a, b

    str_equal = (trim(adjustl(a)) == trim(adjustl(b)))

  end function str_equal

!------------------------------------------------------------------------

  elemental function str_lower(a) result(b)
    !! Converts a string to all lower case.

    character(len = *), intent(in) :: a !! Input string
    character(len = len(a)) :: b !! Output lowercase string
    integer :: i,j

    b = a
    do i = 1, len(b)
       j = iachar(b(i:i))
       if (j >= iachar("A") .and. j <= iachar("Z") ) then
          b(i:i) = achar(iachar(b(i:i)) + 32)
       end if
    end do

  end function str_lower

!------------------------------------------------------------------------

  elemental logical function str_startswith(a, b)
    !! Returns true if string a starts with string b.

    character(len = *), intent(in) :: a, b

    str_startswith = (index(a, b) == 1)

  end function str_startswith

!------------------------------------------------------------------------

  elemental logical function str_endswith(a, b)
    !! Returns true if string a ends with string b.

    character(len = *), intent(in) :: a, b

    str_endswith = (index(a, b) == len(a) - len(b) + 1)

  end function str_endswith

!------------------------------------------------------------------------
  
end module zofu_str_utils
