!   Copyright 2018 University of Auckland.

!   This file is part of Zofu.

!   Zofu is free software: you can redistribute it and/or modify
!   it under the terms of the GNU Lesser General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.

!   Zofu is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU Lesser General Public License for more details.

!   You should have received a copy of the GNU Lesser General Public License
!   along with Zofu.  If not, see <http://www.gnu.org/licenses/>.

module zofu_str_utils
  !! String handling utilities.

  implicit none
  private

  public :: str_equal, str_lower, str_startswith, str_endswith, &
       str_strip_real_trailing_zeros

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

  function str_strip_real_trailing_zeros(a) result(str)
    !! Strips trailing zeros from string representing a real number.

    character(len = *), intent(in) :: a
    character(:), allocatable :: str
    ! Locals:
    integer :: i, dotpos
    character(len = len(a)) :: b

    b = a
    dotpos = index(b, '.')
    if (dotpos > 0) then
       do i = len(b), dotpos + 1, -1
          if (b(i:i) /= ' ') then
             if (b(i:i) == '0') then
                b(i:i) = ' '
             else
                exit
             end if
          end if
       end do
    end if
    str = trim(adjustl(b))

  end function str_strip_real_trailing_zeros

!------------------------------------------------------------------------

end module zofu_str_utils
