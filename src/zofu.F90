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

module zofu

  !! Zofu (Zofu is Object-oriented Fortran Unit-testing)

  implicit none
  private

  integer, parameter, public :: dp = kind(0.d0) !! double precision kind

  type, public :: unit_test_type
     !! Unit test type
     private
     integer, public :: num_cases !! Number of test cases
     integer, public :: num_assertions !! Number of assertions
     integer, public :: num_passed_assertions !! Number of passed assertions
     integer, public :: num_failed_assertions !! Number of failed assertions
     real :: default_relative_tol !! Relative tolerance for testing floating point equality
     real :: minimum_scale !! Minimum scale for testing floating point equality
     character(:), allocatable :: case_name
   contains
     procedure, public :: init => unit_test_init
     procedure, public :: run => unit_test_run
     procedure, public :: summary => unit_test_summary
     procedure :: pass_assertion => unit_test_pass_assertion
     procedure :: fail_assertion_message => unit_test_fail_assertion_message
     procedure :: fail_assertion => unit_test_fail_assertion
     procedure :: unit_test_equal_real_tol
     procedure :: unit_test_equal_double_tol
     generic :: equal_tol => unit_test_equal_real_tol, unit_test_equal_double_tol
     procedure :: unit_test_assert_true
     procedure :: unit_test_assert_equal_logical
     procedure :: unit_test_assert_equal_logical_array_1
     procedure :: unit_test_assert_equal_logical_array_2
     procedure :: unit_test_assert_equal_integer
     procedure :: unit_test_assert_equal_integer_array_1
     procedure :: unit_test_assert_equal_integer_array_2
     procedure :: unit_test_assert_equal_real
     procedure :: unit_test_assert_equal_real_array_1
     procedure :: unit_test_assert_equal_real_array_2
     procedure :: unit_test_assert_equal_double
     procedure :: unit_test_assert_equal_double_array_1
     procedure :: unit_test_assert_equal_double_array_2
     procedure :: unit_test_assert_equal_string
     procedure :: unit_test_assert_equal_string_array_1
     procedure :: unit_test_assert_equal_string_array_2
     generic, public :: assert => &
          unit_test_assert_true, &
          unit_test_assert_equal_logical, &
          unit_test_assert_equal_logical_array_1, &
          unit_test_assert_equal_logical_array_2, &
          unit_test_assert_equal_integer, &
          unit_test_assert_equal_integer_array_1, &
          unit_test_assert_equal_integer_array_2, &
          unit_test_assert_equal_real, &
          unit_test_assert_equal_real_array_1, &
          unit_test_assert_equal_real_array_2, &
          unit_test_assert_equal_double, &
          unit_test_assert_equal_double_array_1, &
          unit_test_assert_equal_double_array_2, &
          unit_test_assert_equal_string, &
          unit_test_assert_equal_string_array_1, &
          unit_test_assert_equal_string_array_2
  end type unit_test_type

  abstract interface
     subroutine test_case_routine(test)
       import unit_test_type
       type(unit_test_type), intent(in out) :: test
     end subroutine test_case_routine
  end interface

contains

!------------------------------------------------------------------------

  logical elemental function str_equal(a, b)
    !! Tests if two character strings are equal.

    character(len = *), intent(in) :: a, b

    str_equal = (trim(adjustl(a)) == trim(adjustl(b)))

  end function str_equal

!------------------------------------------------------------------------

  subroutine unit_test_init(self)
    !! Initialise unit test.

    class(unit_test_type), intent(in out) :: self
    ! Locals:
    real, parameter :: default_relative_tol = 1.e-6
    real, parameter :: default_minimum_scale = 1.e-6
    
    self%num_cases = 0
    self%num_assertions = 0
    self%num_passed_assertions = 0
    self%num_failed_assertions = 0

    self%default_relative_tol = default_relative_tol
    self%minimum_scale = default_minimum_scale

  end subroutine unit_test_init

!------------------------------------------------------------------------

  subroutine unit_test_run(self, test_case, case_name)
    !! Runs test case.
    
    class(unit_test_type), intent(in out) :: self
    procedure(test_case_routine) :: test_case
    character(len = *), intent(in), optional :: case_name

    self%num_cases = self%num_cases + 1

    if (present(case_name)) then
       self%case_name = case_name
    else
       self%case_name = ''
    end if

    call test_case(self)
    
  end subroutine unit_test_run

!------------------------------------------------------------------------

  subroutine unit_test_summary(self)
    !! Writes YAML summary of test statistics to stdout.

    class(unit_test_type), intent(in out) :: self

    write (*,'(a, i0, a, a, i0, a, a, i0, a, a, i0, a)') &
         '{"cases": ', self%num_cases, ', ', &
         '"assertions": ', self%num_assertions, ', ', &
         '"passed": ', self%num_passed_assertions, ', ', &
         '"failed": ', self%num_failed_assertions, '}'

  end subroutine unit_test_summary

!------------------------------------------------------------------------
! Assertion actions:
!------------------------------------------------------------------------

  subroutine unit_test_pass_assertion(self)
    !! Process passed assertion.
    class(unit_test_type), intent(in out) :: self

    self%num_assertions = self%num_assertions + 1
    self%num_passed_assertions = self%num_passed_assertions + 1

  end subroutine unit_test_pass_assertion

!------------------------------------------------------------------------

  function unit_test_fail_assertion_message(self, name) result(msg)
    !! Return YAML string for failed assertion message.

    class(unit_test_type), intent(in) :: self
    character(len = *), intent(in), optional :: name
    character(:), allocatable :: msg
    ! Locals:
    character(len = 32) :: case_num_str

    msg = '"case": '
    if (self%case_name == '') then
       write(case_num_str, '(i0)') self%num_cases
       msg = msg // trim(case_num_str)
    else
       msg = msg // '"' // trim(self%case_name) // '"'
    end if
    if (present(name)) then
       msg = msg // ', "assertion": "' // trim(name) // '"'
    end if

  end function unit_test_fail_assertion_message

!------------------------------------------------------------------------

  subroutine unit_test_fail_assertion(self, name)
    !! Process failed assertion.
    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in), optional :: name
    ! Locals:
    character(:), allocatable :: msg

    self%num_assertions = self%num_assertions + 1
    self%num_failed_assertions = self%num_failed_assertions + 1

    msg = self%fail_assertion_message(name)
    write(*, '(a)') '- {' // trim(msg) // '}'

  end subroutine unit_test_fail_assertion

!------------------------------------------------------------------------
! Floating point equality tests:
!------------------------------------------------------------------------

  elemental logical function unit_test_equal_real_tol(self, &
       a, b, tol) result(equal)
    !! Tests if two real scalars are equal to within the specified
    !! relative tolerance. If no tolerance is specified, the test
    !! default value is used.

    class(unit_test_type), intent(in) :: self
    real, intent(in) :: a, b
    real, intent(in), optional :: tol
    ! Locals:
    real :: tolerance

    if (present(tol)) then
       tolerance = tol
    else
       tolerance = self%default_relative_tol
    end if

    associate (delta => abs(b - a), scale => max(abs(a), abs(b)))
      if (scale > self%minimum_scale) then
         equal = (delta / scale < tolerance)
      else
         equal = (delta < tolerance)
      end if
    end associate

  end function unit_test_equal_real_tol

!------------------------------------------------------------------------

  elemental logical function unit_test_equal_double_tol(self, &
       a, b, tol) result(equal)
    !! Tests if two double precision scalars are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in) :: self
    real(dp), intent(in) :: a, b
    real(dp), intent(in), optional :: tol
    ! Locals:
    real(dp) :: tolerance

    if (present(tol)) then
       tolerance = tol
    else
       tolerance = dble(self%default_relative_tol)
    end if

    associate (delta => abs(b - a), scale => max(abs(a), abs(b)))
      if (scale > dble(self%minimum_scale)) then
         equal = (delta / scale < tolerance)
      else
         equal = (delta < tolerance)
      end if
    end associate

  end function unit_test_equal_double_tol

!------------------------------------------------------------------------
! Logical assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_true(self, condition, name)
    !! Assert specified condition is true.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: condition
    character(len = *), intent(in), optional :: name

    if (condition) then
       call self%pass_assertion()
    else
       call self%fail_assertion(name)
    end if

  end subroutine unit_test_assert_true
  
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_logical(self, a, b, name)
    !! Assert specified logicals are equal.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a, b
    character(len = *), intent(in), optional :: name

    call self%assert(a .eqv. b, name)

  end subroutine unit_test_assert_equal_logical

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_logical_array_1(self, a, b, name)
    !! Assert specified rank-1 logical arrays are equal.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a(:), b(:)
    character(len = *), intent(in), optional :: name

    call self%assert(all(a .eqv. b), name)

  end subroutine unit_test_assert_equal_logical_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_logical_array_2(self, a, b, name)
    !! Assert specified rank-2 logical arrays are equal.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a(:,:), b(:,:)
    character(len = *), intent(in), optional :: name

    call self%assert(all(a .eqv. b), name)

  end subroutine unit_test_assert_equal_logical_array_2

!------------------------------------------------------------------------
! Integer assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer(self, a, b, name)
    !! Assert specified integers are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a, b
    character(len = *), intent(in), optional :: name

    call self%assert(a == b, name)

  end subroutine unit_test_assert_equal_integer

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer_array_1(self, a, b, name)
    !! Assert specified rank-1 integer arrays are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(:), b(:)
    character(len = *), intent(in), optional :: name

    call self%assert(all(a == b), name)

  end subroutine unit_test_assert_equal_integer_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer_array_2(self, a, b, name)
    !! Assert specified rank-2 integer arrays are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(:,:), b(:,:)
    character(len = *), intent(in), optional :: name

    call self%assert(all(a == b), name)

  end subroutine unit_test_assert_equal_integer_array_2

!------------------------------------------------------------------------
! Real assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_real(self, a, b, tol, name)
    !! Assert specified real scalars are equal to within the specified
    !! relative tolerance. If no tolerance is specified, the test
    !! default value is used.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a, b
    real, intent(in), optional :: tol
    character(len = *), intent(in), optional :: name

    call self%assert(self%equal_tol(a, b, tol), name)

  end subroutine unit_test_assert_equal_real

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_real_array_1(self, a, b, tol, name)
    !! Assert specified real rank-1 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a(:), b(:)
    real, intent(in), optional :: tol
    character(len = *), intent(in), optional :: name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_real_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_real_array_2(self, a, b, tol, name)
    !! Assert specified real rank-2 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a(:,:), b(:,:)
    real, intent(in), optional :: tol
    character(len = *), intent(in), optional :: name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_real_array_2

!------------------------------------------------------------------------
! Double precision assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_double(self, a, b, tol, name)
    !! Assert specified double precision scalars are equal to within
    !! the specified relative tolerance. If no tolerance is specified,
    !! the test default value is used.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a, b
    real(dp), intent(in), optional :: tol
    character(len = *), intent(in), optional :: name

    call self%assert(self%equal_tol(a, b, tol), name)

  end subroutine unit_test_assert_equal_double

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_double_array_1(self, a, b, tol, name)
    !! Assert specified double precision rank-1 arrays are equal to
    !! within the specified relative tolerance. If no tolerance is
    !! specified, the test default value is used.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a(:), b(:)
    real(dp), intent(in), optional :: tol
    character(len = *), intent(in), optional :: name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_double_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_double_array_2(self, a, b, tol, name)
    !! Assert specified double precision rank-2 arrays are equal to
    !! within the specified relative tolerance. If no tolerance is
    !! specified, the test default value is used.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a(:,:), b(:,:)
    real(dp), intent(in), optional :: tol
    character(len = *), intent(in), optional :: name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_double_array_2

!------------------------------------------------------------------------
! String assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string(self, a, b, name)
    !! Assert specified strings are equal.
    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a, b
    character(len = *), intent(in), optional :: name

    call self%assert(str_equal(a, b), name)

  end subroutine unit_test_assert_equal_string

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string_array_1(self, a, b, name)
    !! Assert specified rank-1 string arrays are equal.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a(:), b(:)
    character(len = *), intent(in), optional :: name

    call self%assert(all(str_equal(a, b)), name)

  end subroutine unit_test_assert_equal_string_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string_array_2(self, a, b, name)
    !! Assert specified rank-2 string arrays are equal.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a(:,:), b(:,:)
    character(len = *), intent(in), optional :: name

    call self%assert(all(str_equal(a, b)), name)

  end subroutine unit_test_assert_equal_string_array_2

!------------------------------------------------------------------------

end module zofu
