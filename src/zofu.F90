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

  !! Zofu (Zofu is Object-oriented Fortran Unit-testing) unit test type.

  use zofu_str_utils

  implicit none
  private

  integer, parameter, public :: dp = kind(0.d0) !! double precision kind

  type, public :: test_counter_type
     !! Type for counting total number of cases or assertions,
     !! together with how many have passed and failed.
     private
     integer, public :: count !! Total count
     integer, public :: passed !! How many have passed
     integer, public :: failed !! How many have failed
   contains
     private
     procedure, public :: init => test_counter_init
     procedure, public :: pass => test_counter_pass
     procedure, public :: fail => test_counter_fail
     procedure :: test_counter_assign
     generic, public :: assignment(=) => test_counter_assign
     procedure, public :: yaml => test_counter_yaml
  end type test_counter_type

  type, public :: unit_test_type
     !! Unit test type
     private
     type(test_counter_type), public :: cases !! Test case counter
     type(test_counter_type), public :: assertions !! Test assertions counter
     type(test_counter_type), public :: case_assertions !! Test assertions counter for current case
     logical, public :: passed, failed !! Whether test passed or failed
     real :: tolerance !! Default relative tolerance for testing floating point equality
     real :: minimum_scale !! Minimum scale for testing floating point equality
     character(:), allocatable :: case_name !! Name of last case run
   contains
     procedure, public :: init => unit_test_init
     procedure, public :: run => unit_test_run
     procedure, public :: summary => unit_test_summary
     procedure :: init_counters => unit_test_init_counters
     procedure :: write_yaml => unit_test_write_yaml
     procedure :: start_case => unit_test_start_case
     procedure :: end_case => unit_test_end_case
     procedure :: pass_assertion => unit_test_pass_assertion
     procedure :: failure_yaml => unit_test_failure_yaml
     procedure :: fail_assertion => unit_test_fail_assertion
     procedure :: unit_test_equal_real_tol
     procedure :: unit_test_equal_double_tol
     procedure :: unit_test_equal_complex_tol
     generic :: equal_tol => unit_test_equal_real_tol, &
          unit_test_equal_double_tol, &
          unit_test_equal_complex_tol
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
     procedure :: unit_test_assert_equal_complex
     procedure :: unit_test_assert_equal_complex_array_1
     procedure :: unit_test_assert_equal_complex_array_2
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
          unit_test_assert_equal_complex, &
          unit_test_assert_equal_complex_array_1, &
          unit_test_assert_equal_complex_array_2, &
          unit_test_assert_equal_string, &
          unit_test_assert_equal_string_array_1, &
          unit_test_assert_equal_string_array_2
  end type unit_test_type

  abstract interface
     subroutine test_case_routine(test)
       import unit_test_type
       class(unit_test_type), intent(in out) :: test
     end subroutine test_case_routine
  end interface

contains

!------------------------------------------------------------------------
! Test counter methods:
!------------------------------------------------------------------------

  subroutine test_counter_init(self)
    !! Initializes test counter.

    class(test_counter_type), intent(in out) :: self
    self%count = 0
    self%passed = 0
    self%failed = 0

  end subroutine test_counter_init

!------------------------------------------------------------------------

  subroutine test_counter_pass(self)
    !! Adds pass to counter.

    class(test_counter_type), intent(in out) :: self

    self%count = self%count + 1
    self%passed = self%passed + 1

  end subroutine test_counter_pass

!------------------------------------------------------------------------

  subroutine test_counter_fail(self)
    !! Adds failure to counter.

    class(test_counter_type), intent(in out) :: self

    self%count = self%count + 1
    self%failed = self%failed + 1

  end subroutine test_counter_fail

!------------------------------------------------------------------------

  subroutine test_counter_assign(dest, source)
    !! Assigns one test counter to another.

    class(test_counter_type), intent(out) :: dest !! Destination counter
    type(test_counter_type), intent(in) :: source !! Source counter

    dest%count = source%count
    dest%passed = source%passed
    dest%failed = source%failed

  end subroutine test_counter_assign

!------------------------------------------------------------------------

  function test_counter_yaml(self) result(yaml)
    !! Returns YAML string summarising counter contents.

    class(test_counter_type), intent(in) :: self
    character(:), allocatable :: yaml
    ! Locals:
    character(len = 16) :: istr

    write(istr, '(i0)') self%count
    yaml = '{"count": ' // trim(istr)
    write(istr, '(i0)') self%passed
    yaml = yaml // ', "passed": ' // trim(istr)
    write(istr, '(i0)') self%failed
    yaml = yaml // ', "failed": ' // trim(istr) // '}'

  end function test_counter_yaml

!------------------------------------------------------------------------
! Unit test methods:
!------------------------------------------------------------------------

  subroutine unit_test_init_counters(self)
    !! Initializes counters for cases and assertions, and writes
    !! header for YAML list of failed assertions.

    class(unit_test_type), intent(in out) :: self

    call self%cases%init()
    call self%assertions%init()
    write(*,'(a)') 'failed assertions:'

  end subroutine unit_test_init_counters

!------------------------------------------------------------------------

  subroutine unit_test_init(self)
    !! Initialise unit test.

    class(unit_test_type), intent(in out) :: self
    ! Locals:
    real, parameter :: default_relative_tol = 1.e-6
    real, parameter :: default_minimum_scale = 1.e-6

    self%passed = .true.
    self%failed = .false.
    call self%init_counters()

    self%tolerance = default_relative_tol
    self%minimum_scale = default_minimum_scale

  end subroutine unit_test_init

!------------------------------------------------------------------------

  subroutine unit_test_start_case(self, case_name)
    !! Starts new test case.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in), optional :: case_name !! Name of test case

    call self%case_assertions%init()

    if (present(case_name)) then
       self%case_name = case_name
    else
       self%case_name = ''
    end if

  end subroutine unit_test_start_case

!------------------------------------------------------------------------

  subroutine unit_test_end_case(self)
    !! Ends test case.

    class(unit_test_type), intent(in out) :: self

    if (self%case_assertions%failed > 0) then
       call self%cases%fail()
       self%failed = .true.
       self%passed = .not. (self%failed)
    else
       call self%cases%pass()
    end if

  end subroutine unit_test_end_case

!------------------------------------------------------------------------

  subroutine unit_test_run(self, test_case, case_name)
    !! Runs test case.
    
    class(unit_test_type), intent(in out) :: self
    procedure(test_case_routine) :: test_case !! Test case subroutine
    character(len = *), intent(in), optional :: case_name !! Name of test case

    call self%start_case(case_name)
    call test_case(self)
    call self%end_case()

  end subroutine unit_test_run

!------------------------------------------------------------------------

  subroutine unit_test_write_yaml(self, assertions)
    !! Writes YAML summary of test, with specified assertion counter.

    class(unit_test_type), intent(in) :: self
    type(test_counter_type), intent(in) :: assertions !! Assertions counter
    ! Locals:
    character(5) :: pass_str

    if (self%passed) then
       pass_str = 'true'
    else
       pass_str = 'false'
    end if

    write(*,'(a, a)') 'cases: ', self%cases%yaml()
    write(*,'(a, a)') 'assertions: ', assertions%yaml()
    write(*,'(a, a)') 'passed: ', trim(adjustl(pass_str))

  end subroutine unit_test_write_yaml

!------------------------------------------------------------------------

  subroutine unit_test_summary(self)
    !! Writes YAML summary of test statistics to stdout.

    class(unit_test_type), intent(in) :: self

    call self%write_yaml(self%assertions)

  end subroutine unit_test_summary

!------------------------------------------------------------------------
! Assertion actions:
!------------------------------------------------------------------------

  subroutine unit_test_pass_assertion(self)
    !! Process passed assertion.
    class(unit_test_type), intent(in out) :: self

    call self%assertions%pass()
    call self%case_assertions%pass()

  end subroutine unit_test_pass_assertion

!------------------------------------------------------------------------

  function unit_test_failure_yaml(self, name) result(msg)
    !! Return YAML string for failed assertion message.

    class(unit_test_type), intent(in) :: self
    character(len = *), intent(in), optional :: name !! Assertion name
    character(:), allocatable :: msg
    ! Locals:
    character(len = 32) :: case_num_str

    msg = '"case": '
    if (self%case_name == '') then
       write(case_num_str, '(i0)') self%cases%count
       msg = msg // trim(case_num_str)
    else
       msg = msg // '"' // trim(self%case_name) // '"'
    end if
    if (present(name)) then
       msg = msg // ', "assertion": "' // trim(name) // '"'
    end if

  end function unit_test_failure_yaml

!------------------------------------------------------------------------

  subroutine unit_test_fail_assertion(self, name)
    !! Process failed assertion.
    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assertions%fail()
    call self%case_assertions%fail()

    write(*, '(a)') '- {' // self%failure_yaml(name) // '}'

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
    real, intent(in) :: a, b !! Value to compare
    real, intent(in), optional :: tol !! Tolerance
    ! Locals:
    real :: tolerance

    if (present(tol)) then
       tolerance = tol
    else
       tolerance = self%tolerance
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
    real(dp), intent(in) :: a, b !! Value to compare
    real(dp), intent(in), optional :: tol !! Tolerance
    ! Locals:
    real(dp) :: tolerance

    if (present(tol)) then
       tolerance = tol
    else
       tolerance = dble(self%tolerance)
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

  elemental logical function unit_test_equal_complex_tol(self, &
       a, b, tol) result(equal)
    !! Tests if two complex scalars are equal to within the specified
    !! relative tolerance. If no tolerance is specified, the test
    !! default value is used.

    class(unit_test_type), intent(in) :: self
    complex, intent(in) :: a, b !! Value to compare
    real, intent(in), optional :: tol !! Tolerance
    ! Locals:
    real :: tolerance

    if (present(tol)) then
       tolerance = tol
    else
       tolerance = self%tolerance
    end if

    associate (delta => abs(b - a), scale => max(abs(a), abs(b)))
      if (scale > self%minimum_scale) then
         equal = (delta / scale < tolerance)
      else
         equal = (delta < tolerance)
      end if
    end associate

  end function unit_test_equal_complex_tol

!------------------------------------------------------------------------
! Logical assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_true(self, condition, name)
    !! Assert specified condition is true.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: condition !! Value to assert
    character(len = *), intent(in), optional :: name !! Assertion name

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
    logical, intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(a .eqv. b, name)

  end subroutine unit_test_assert_equal_logical

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_logical_array_1(self, a, b, name)
    !! Assert specified rank-1 logical arrays are equal.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(a .eqv. b), name)

  end subroutine unit_test_assert_equal_logical_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_logical_array_2(self, a, b, name)
    !! Assert specified rank-2 logical arrays are equal.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a(:,:), b(:,:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(a .eqv. b), name)

  end subroutine unit_test_assert_equal_logical_array_2

!------------------------------------------------------------------------
! Integer assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer(self, a, b, name)
    !! Assert specified integers are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(a == b, name)

  end subroutine unit_test_assert_equal_integer

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer_array_1(self, a, b, name)
    !! Assert specified rank-1 integer arrays are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(a == b), name)

  end subroutine unit_test_assert_equal_integer_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer_array_2(self, a, b, name)
    !! Assert specified rank-2 integer arrays are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(:,:), b(:,:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

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
    real, intent(in) :: a, b !! Value to compare
    real, intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(self%equal_tol(a, b, tol), name)

  end subroutine unit_test_assert_equal_real

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_real_array_1(self, a, b, tol, name)
    !! Assert specified real rank-1 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a(:), b(:) !! Value to compare
    real, intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_real_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_real_array_2(self, a, b, tol, name)
    !! Assert specified real rank-2 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a(:,:), b(:,:) !! Value to compare
    real, intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

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
    real(dp), intent(in) :: a, b !! Value to compare
    real(dp), intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(self%equal_tol(a, b, tol), name)

  end subroutine unit_test_assert_equal_double

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_double_array_1(self, a, b, tol, name)
    !! Assert specified double precision rank-1 arrays are equal to
    !! within the specified relative tolerance. If no tolerance is
    !! specified, the test default value is used.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a(:), b(:) !! Value to compare
    real(dp), intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_double_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_double_array_2(self, a, b, tol, name)
    !! Assert specified double precision rank-2 arrays are equal to
    !! within the specified relative tolerance. If no tolerance is
    !! specified, the test default value is used.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a(:,:), b(:,:) !! Value to compare
    real(dp), intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_double_array_2

!------------------------------------------------------------------------
! Complex assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_complex(self, a, b, tol, name)
    !! Assert specified complex scalars are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a, b !! Value to compare
    real, intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(self%equal_tol(a, b, tol), name)

  end subroutine unit_test_assert_equal_complex

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_complex_array_1(self, a, b, tol, name)
    !! Assert specified complex rank-1 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a(:), b(:) !! Value to compare
    real, intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_complex_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_complex_array_2(self, a, b, tol, name)
    !! Assert specified complex rank-2 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a(:,:), b(:,:) !! Value to compare
    real, intent(in), optional :: tol !! Tolerance
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(self%equal_tol(a, b, tol)), name)

  end subroutine unit_test_assert_equal_complex_array_2

!------------------------------------------------------------------------
! String assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string(self, a, b, name)
    !! Assert specified strings are equal.
    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(str_equal(a, b), name)

  end subroutine unit_test_assert_equal_string

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string_array_1(self, a, b, name)
    !! Assert specified rank-1 string arrays are equal.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(str_equal(a, b)), name)

  end subroutine unit_test_assert_equal_string_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string_array_2(self, a, b, name)
    !! Assert specified rank-2 string arrays are equal.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a(:,:), b(:,:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(all(str_equal(a, b)), name)

  end subroutine unit_test_assert_equal_string_array_2

!------------------------------------------------------------------------

end module zofu
