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

  use zofu_kinds
  use zofu_str_utils

  implicit none
  private

  integer, parameter :: FAILURE_REASON_VALUE = 1, FAILURE_REASON_SHAPE = 2 !! assertion failure reasons

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
     procedure, public :: increment => test_counter_increment
     procedure, public :: pass => test_counter_pass
     procedure, public :: fail => test_counter_fail
     procedure :: test_counter_assign
     generic, public :: assignment(=) => test_counter_assign
     procedure, public :: yaml => test_counter_yaml
  end type test_counter_type

  type, public :: assertion_failure_type
     !! Type for assertion failure details.
     private
     integer, public :: reason !! why the assertion failed
     character(:), allocatable, public  :: value1, value2 !! string representation of unequal values
     logical, public :: scalar !! whether values are scalar or array
     character(:), allocatable, public :: index !! string representation of array indices of first assertion failure
     character(:), allocatable, public :: count !! string representation of number of assertion failures in array
   contains
     private
     procedure, public :: init => assertion_failure_init
     procedure, public :: yaml => assertion_failure_yaml
  end type assertion_failure_type

  type, public :: unit_test_type
     !! Unit test type
     private
     type(test_counter_type), public :: cases !! Test case counter
     type(test_counter_type), public :: assertions !! Test assertions counter
     type(test_counter_type), public :: case_assertions !! Test assertions counter for current case
     logical, public :: passed, failed !! Whether test passed or failed
     character(:), allocatable, public :: format_logical !! Format for logical variables
     character(:), allocatable, public :: format_integer !! Format for integer variables
     character(:), allocatable, public :: format_real !! Format for real variables
     character(:), allocatable, public :: format_real_scientific !! Format for real variables in scientific notation
     character, public :: quote_str !! Quote mark character for string output
     real, public :: tolerance !! Default relative tolerance for testing floating point equality
     real, public :: minimum_scale !! Minimum scale for testing floating point equality
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
     procedure :: unit_test_str_logical
     procedure :: unit_test_str_integer
     procedure :: unit_test_str_integer_pair
     procedure :: unit_test_str_real
     procedure :: unit_test_str_double
     procedure :: unit_test_str_complex
     procedure :: unit_test_str_string
     generic, public :: str => &
          unit_test_str_logical, &
          unit_test_str_integer, &
          unit_test_str_integer_pair, &
          unit_test_str_real, &
          unit_test_str_double, &
          unit_test_str_complex, &
          unit_test_str_string
     procedure :: unit_test_failure_logical
     procedure :: unit_test_failure_logical_array_1
     procedure :: unit_test_failure_logical_array_2
     procedure :: unit_test_failure_integer
     procedure :: unit_test_failure_integer_array_1
     procedure :: unit_test_failure_integer_array_2
     procedure :: unit_test_failure_real
     procedure :: unit_test_failure_real_array_1
     procedure :: unit_test_failure_real_array_2
     procedure :: unit_test_failure_double
     procedure :: unit_test_failure_double_array_1
     procedure :: unit_test_failure_double_array_2
     procedure :: unit_test_failure_complex_array_1
     procedure :: unit_test_failure_complex_array_2
     procedure :: unit_test_failure_complex
     procedure :: unit_test_failure_string
     procedure :: unit_test_failure_string_array_1
     procedure :: unit_test_failure_string_array_2
     generic, public :: failure => &
          unit_test_failure_logical, &
          unit_test_failure_logical_array_1, &
          unit_test_failure_logical_array_2, &
          unit_test_failure_integer, &
          unit_test_failure_integer_array_1, &
          unit_test_failure_integer_array_2, &
          unit_test_failure_real, &
          unit_test_failure_real_array_1, &
          unit_test_failure_real_array_2, &
          unit_test_failure_double, &
          unit_test_failure_double_array_1, &
          unit_test_failure_double_array_2, &
          unit_test_failure_complex, &
          unit_test_failure_complex_array_1, &
          unit_test_failure_complex_array_2, &
          unit_test_failure_string, &
          unit_test_failure_string_array_1, &
          unit_test_failure_string_array_2
  end type unit_test_type

  abstract interface
     subroutine test_case_routine(test)
       import unit_test_type
       class(unit_test_type), intent(in out) :: test
     end subroutine test_case_routine
  end interface

contains

!------------------------------------------------------------------------
! Index finding utilities:
!------------------------------------------------------------------------

  subroutine first_false_index_1(m, first)
    !! Returns index of first false index in rank-1 mask array m (or
    !! -1 if none found).

    logical, intent(in) :: m(:) !! Input mask array
    integer, intent(out) :: first !! Output index
    ! Locals:
    integer :: i
    integer, allocatable :: indices(:)

    indices = [(i, i = 1, size(m))]
    indices = pack(indices, m)
    if (size(indices) > 0) then
       first = indices(1)
    else
       first = -1
    end if

  end subroutine first_false_index_1

!------------------------------------------------------------------------

  subroutine first_false_index_2(m, first)
    !! Returns indices of first false index in rank-2 mask array m (or
    !! (-1, -1) if none found).

    logical, intent(in) :: m(:,:) !! Input mask array
    integer, intent(out) :: first(2) !! Output indices
    ! Locals:
    integer :: i, j
    integer, allocatable :: indices1(:,:), indices2(:,:)
    integer, allocatable :: indices1_pack(:), indices2_pack(:)

    associate(n1 => size(m, 1), n2 => size(m, 2))
      indices1 = reshape([((i, i = 1, n1), j = 1, n2)], [n1, n2])
      indices2 = reshape([((j, i = 1, n1), j = 1, n2)], [n1, n2])
      indices1_pack = pack(indices1, m)
      indices2_pack = pack(indices2, m)
      if (size(indices1_pack) > 0) then
         first = [indices1_pack(1), indices2_pack(1)]
      else
         first = [-1, -1]
      end if
    end associate

  end subroutine first_false_index_2

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

  subroutine test_counter_increment(self)
    !! Increments test counter count.

    class(test_counter_type), intent(in out) :: self

    self%count = self%count + 1

  end subroutine test_counter_increment

!------------------------------------------------------------------------

  subroutine test_counter_pass(self)
    !! Adds pass to counter.

    class(test_counter_type), intent(in out) :: self

    self%passed = self%passed + 1

  end subroutine test_counter_pass

!------------------------------------------------------------------------

  subroutine test_counter_fail(self)
    !! Adds failure to counter.

    class(test_counter_type), intent(in out) :: self

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
! Assertion failure methods:
!------------------------------------------------------------------------

  subroutine assertion_failure_init(self, value1, value2, reason, &
       scalar, index, num)
    !! Initialize assertion failure.

    class(assertion_failure_type), intent(in out) :: self
    character(len = *), intent(in) :: value1, value2 !! values
    integer, intent(in), optional :: reason !! Failure reason
    logical, intent(in), optional :: scalar !! whether values are scalar or array
    character(len = *), intent(in), optional :: index !! array index
    character(len = *), intent(in), optional :: num !! number of assertion failures in array
    ! Locals:
    integer, parameter :: default_reason = FAILURE_REASON_VALUE
    character, parameter :: default_index = '1'
    character, parameter :: default_count = '1'

    self%value1 = value1
    self%value2 = value2

    if (present(reason)) then
       self%reason = reason
    else
       self%reason = default_reason
    end if

    if (present(scalar)) then
       self%scalar = scalar
    else
       self%scalar = .true.
    end if

    if (self%scalar) then
       self%index = default_index
       self%count = default_count
    else
       if (present(index)) then
          self%index = index
       else
          self%index = default_index
       end if
       if (present(num)) then
          self%count = num
       else
          self%count = default_count
       end if
    end if

  end subroutine assertion_failure_init
  
!------------------------------------------------------------------------

  function assertion_failure_yaml(self) result(yaml)
    !! Initialize assertion failure.

    class(assertion_failure_type), intent(in) :: self
    character(:), allocatable :: yaml

    yaml = '"reason": '
    select case (self%reason)
    case (FAILURE_REASON_VALUE)
       yaml = yaml // '"value"'
    case(FAILURE_REASON_SHAPE)
       yaml = yaml // '"shape"'
    end select

    yaml = yaml // ', "values": [' // self%value1 // ', ' // self%value2 // ']'

    if (.not. self%scalar) then
       yaml = yaml // ', "index": ' // self%index // ', "count": ' // self%count
    end if

  end function assertion_failure_yaml
    
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
    character(3), parameter :: default_format_logical = "(l)"
    character(4), parameter :: default_format_integer = "(i0)"
    character(7), parameter :: default_format_real = "(f16.5)"
    character(7), parameter :: default_format_real_scientific = "(e16.5)"
    character, parameter :: default_quote_str = '"'

    self%passed = .true.
    self%failed = .false.
    call self%init_counters()

    self%tolerance = default_relative_tol
    self%minimum_scale = default_minimum_scale

    self%format_logical = default_format_logical
    self%format_integer = default_format_integer
    self%format_real = default_format_real
    self%format_real_scientific = default_format_real_scientific
    self%quote_str = default_quote_str

  end subroutine unit_test_init

!------------------------------------------------------------------------

  subroutine unit_test_start_case(self, case_name)
    !! Starts new test case.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in), optional :: case_name !! Name of test case

    call self%cases%increment()
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

    call self%assertions%increment()
    call self%assertions%pass()
    call self%case_assertions%increment()
    call self%case_assertions%pass()

  end subroutine unit_test_pass_assertion

!------------------------------------------------------------------------

  function unit_test_failure_yaml(self, name, failure) result(msg)
    !! Return YAML string for failed assertion message.

    class(unit_test_type), intent(in) :: self
    character(len = *), intent(in), optional :: name !! Assertion name
    type(assertion_failure_type), intent(in), optional :: failure !! Assertion failure type
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
    if (present(failure)) then
       msg = msg // ', ' // failure%yaml()
    end if

  end function unit_test_failure_yaml

!------------------------------------------------------------------------

  subroutine unit_test_fail_assertion(self, name, failure)
    !! Process failed assertion.
    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in), optional :: name !! Assertion name
    type(assertion_failure_type), intent(in), optional :: failure !! Assertion failure

    call self%assertions%increment()
    call self%assertions%fail()
    call self%case_assertions%increment()
    call self%case_assertions%fail()

    write(*, '(a)') '- {' // self%failure_yaml(name, failure) // '}'

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

  subroutine unit_test_assert_true(self, condition, name, failure)
    !! Assert specified condition is true.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: condition !! Value to assert
    character(len = *), intent(in), optional :: name !! Assertion name
    type(assertion_failure_type), intent(in), optional :: failure !! Assertion failure

    if (condition) then
       call self%pass_assertion()
    else
       call self%fail_assertion(name, failure)
    end if

  end subroutine unit_test_assert_true
  
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_logical(self, a, b, name)
    !! Assert specified logicals are equal.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(a .eqv. b, name, self%failure(a, b))

  end subroutine unit_test_assert_equal_logical

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_logical_array_1(self, a, b, name)
    !! Assert specified rank-1 logical arrays are equal.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         call self%assert(all(a .eqv. b), name, self%failure(a, b))
      else
         call self%assert(na == nb, name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_logical_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_logical_array_2(self, a, b, name)
    !! Assert specified rank-2 logical arrays are equal.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a(:,:), b(:,:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         call self%assert(all(a .eqv. b), name, self%failure(a, b))
      else
         call self%assert(all(na == nb), name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_logical_array_2

!------------------------------------------------------------------------
! Integer assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer(self, a, b, name)
    !! Assert specified integers are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(a == b, name, self%failure(a, b))

  end subroutine unit_test_assert_equal_integer

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer_array_1(self, a, b, name)
    !! Assert specified rank-1 integer arrays are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         call self%assert(all(a == b), name, self%failure(a, b))
      else
         call self%assert(na == nb, name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_integer_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_integer_array_2(self, a, b, name)
    !! Assert specified rank-2 integer arrays are equal.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(:,:), b(:,:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         call self%assert(all(a == b), name, self%failure(a, b))
      else
         call self%assert(all(na == nb), name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_integer_array_2

!------------------------------------------------------------------------
! Real assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_real(self, a, b, name, tol)
    !! Assert specified real scalars are equal to within the specified
    !! relative tolerance. If no tolerance is specified, the test
    !! default value is used.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name
    real, intent(in), optional :: tol !! Tolerance

    call self%assert(self%equal_tol(a, b, tol), name, &
         self%failure(a, b))

  end subroutine unit_test_assert_equal_real

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_real_array_1(self, a, b, name, tol)
    !! Assert specified real rank-1 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name
    real, intent(in), optional :: tol !! Tolerance

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         call self%assert(all(self%equal_tol(a, b, tol)), name, &
              self%failure(a, b))
      else
         call self%assert(na == nb, name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_real_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_real_array_2(self, a, b, name, tol)
    !! Assert specified real rank-2 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a(:,:), b(:,:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name
    real, intent(in), optional :: tol !! Tolerance

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         call self%assert(all(self%equal_tol(a, b, tol)), &
              name, self%failure(a, b))
      else
         call self%assert(all(na == nb), name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_real_array_2

!------------------------------------------------------------------------
! Double precision assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_double(self, a, b, name, tol)
    !! Assert specified double precision scalars are equal to within
    !! the specified relative tolerance. If no tolerance is specified,
    !! the test default value is used.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name
    real(dp), intent(in), optional :: tol !! Tolerance

    call self%assert(self%equal_tol(a, b, tol), name, &
         self%failure(a, b))

  end subroutine unit_test_assert_equal_double

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_double_array_1(self, a, b, name, tol)
    !! Assert specified double precision rank-1 arrays are equal to
    !! within the specified relative tolerance. If no tolerance is
    !! specified, the test default value is used.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name
    real(dp), intent(in), optional :: tol !! Tolerance

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         call self%assert(all(self%equal_tol(a, b, tol)), name, &
              self%failure(a, b))
      else
         call self%assert(na == nb, name, self%failure(a, b))
      end if
    end associate

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

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         call self%assert(all(self%equal_tol(a, b, tol)), &
              name, self%failure(a, b))
      else
         call self%assert(all(na == nb), name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_double_array_2

!------------------------------------------------------------------------
! Complex assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_complex(self, a, b, name, tol)
    !! Assert specified complex scalars are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name
    real, intent(in), optional :: tol !! Tolerance

    call self%assert(self%equal_tol(a, b, tol), name, &
         self%failure(a, b))

  end subroutine unit_test_assert_equal_complex

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_complex_array_1(self, a, b, name, tol)
    !! Assert specified complex rank-1 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name
    real, intent(in), optional :: tol !! Tolerance

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         call self%assert(all(self%equal_tol(a, b, tol)), name, &
              self%failure(a, b))
      else
         call self%assert(na == nb, name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_complex_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_complex_array_2(self, a, b, name, tol)
    !! Assert specified complex rank-2 arrays are equal to within the
    !! specified relative tolerance. If no tolerance is specified, the
    !! test default value is used.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a(:,:), b(:,:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name
    real, intent(in), optional :: tol !! Tolerance

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         call self%assert(all(self%equal_tol(a, b, tol)), name, &
              self%failure(a, b))
      else
         call self%assert(all(na == nb), name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_complex_array_2

!------------------------------------------------------------------------
! String assertions:
!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string(self, a, b, name)
    !! Assert specified strings are equal.
    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a, b !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    call self%assert(str_equal(a, b), name, &
         self%failure(a, b))

  end subroutine unit_test_assert_equal_string

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string_array_1(self, a, b, name)
    !! Assert specified rank-1 string arrays are equal.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a(:), b(:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         call self%assert(all(str_equal(a, b)), name, &
              self%failure(a, b))
      else
         call self%assert(na == nb, name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_string_array_1

!------------------------------------------------------------------------

  subroutine unit_test_assert_equal_string_array_2(self, a, b, name)
    !! Assert specified rank-2 string arrays are equal.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a(:,:), b(:,:) !! Value to compare
    character(len = *), intent(in), optional :: name !! Assertion name

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         call self%assert(all(str_equal(a, b)), name, &
              self%failure(a, b))
      else
         call self%assert(all(na == nb), name, self%failure(a, b))
      end if
    end associate

  end subroutine unit_test_assert_equal_string_array_2

!------------------------------------------------------------------------
! String functions:
!------------------------------------------------------------------------

  function unit_test_str_logical(self, a) result(str)
    !! Return string representation of logical variable.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a
    character(:), allocatable :: str
    ! Locals:
    character(1) :: astr

    write(astr, self%format_logical) a
    str = astr

  end function unit_test_str_logical

!------------------------------------------------------------------------

  function unit_test_str_integer(self, a) result(str)
    !! Return string representation of integer variable.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a
    character(:), allocatable :: str
    ! Locals:
    character(32) :: astr

    write(astr, self%format_integer) a
    str = trim(adjustl(astr))

  end function unit_test_str_integer

!------------------------------------------------------------------------

  function unit_test_str_integer_pair(self, a) result(str)
    !! Return string representation of integer array variable of size 2.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(2)
    character(:), allocatable :: str

    str = '[' // self%str(a(1)) // ', ' // self%str(a(2)) // ']'

  end function unit_test_str_integer_pair

!------------------------------------------------------------------------

  function unit_test_str_real(self, a) result(str)
    !! Return string representation of real variable.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a
    character(:), allocatable :: str
    ! Locals:
    character(32) :: astr
    real, parameter :: small = 1.e-2, big = 1.e5

    associate(absa => abs(a))
      if ((small < absa) .and. (absa < big)) then
         write(astr, self%format_real) a
         str = str_strip_real_trailing_zeros(astr)
      else
         write(astr, self%format_real_scientific) a
         str = trim(adjustl(astr))
      end if
    end associate

  end function unit_test_str_real

!------------------------------------------------------------------------

  function unit_test_str_double(self, a) result(str)
    !! Return string representation of double precision variable.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a
    character(:), allocatable :: str

    str = self%str(sngl(a))

  end function unit_test_str_double

!------------------------------------------------------------------------

  function unit_test_str_complex(self, a) result(str)
    !! Return string representation of complex variable.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a
    character(:), allocatable :: str

    str = '[' // self%str(real(a)) // ', ' // self%str(aimag(a)) // ']'

  end function unit_test_str_complex

!------------------------------------------------------------------------

  function unit_test_str_string(self, a) result(str)
    !! Return string representation of string variable (trimming and
    !! adding quotes).

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a
    character(:), allocatable :: str

    str = self%quote_str // trim(adjustl(a)) // self%quote_str

  end function unit_test_str_string

!------------------------------------------------------------------------
! Failure functions:
!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_logical(self, &
       a, b) result(failure)
    !! Return failure for logical values.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a, b

    call failure%init(self%str(a), self%str(b))

  end function unit_test_failure_logical

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_logical_array_1(self, &
       a, b) result(failure)
    !! Return failure for rank-1 logical array values.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a(:), b(:)
    ! Locals:
    logical :: m(size(a))
    integer :: i, num

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         m = .not. (a .eqv. b)
         num = count(m)
         if (num > 0) then
            call first_false_index_1(m, i)
            call failure%init(self%str(a(i)), self%str(b(i)), &
                 scalar = .false., index = self%str(i), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_logical_array_1

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_logical_array_2(self, &
       a, b) result(failure)
    !! Return failure for rank-2 logical array values.

    class(unit_test_type), intent(in out) :: self
    logical, intent(in) :: a(:,:), b(:,:)
    ! Locals:
    logical :: m(size(a, 1), size(a, 2))
    integer :: ij(2), num

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         m = .not. (a .eqv. b)
         num = count(m)
         if (num > 0) then
            call first_false_index_2(m, ij)
            call failure%init(self%str(a(ij(1), ij(2))), self%str(b(ij(1), ij(2))), &
                 scalar = .false., index = self%str(ij), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_logical_array_2

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_integer(self, &
       a, b) result(failure)
    !! Return failure for integer values.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a, b

    call failure%init(self%str(a), self%str(b))
    
  end function unit_test_failure_integer

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_integer_array_1(self, &
       a, b) result(failure)
    !! Return failure for rank-1 integer array values.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(:), b(:)
    ! Locals:
    logical :: m(size(a))
    integer :: i, num

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         m = .not. (a == b)
         num = count(m)
         if (num > 0) then
            call first_false_index_1(m, i)
            call failure%init(self%str(a(i)), self%str(b(i)), &
                 scalar = .false., index = self%str(i), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_integer_array_1

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_integer_array_2(self, &
       a, b) result(failure)
    !! Return failure for rank-2 integer array values.

    class(unit_test_type), intent(in out) :: self
    integer, intent(in) :: a(:,:), b(:,:)
    ! Locals:
    logical :: m(size(a, 1), size(a, 2))
    integer :: ij(2), num

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         m = .not. (a == b)
         num = count(m)
         if (num > 0) then
            call first_false_index_2(m, ij)
            call failure%init(self%str(a(ij(1), ij(2))), self%str(b(ij(1), ij(2))), &
                 scalar = .false., index = self%str(ij), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_integer_array_2

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_real(self, &
       a, b) result(failure)
    !! Return failure for real values.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a, b

    call failure%init(self%str(a), self%str(b))
    
  end function unit_test_failure_real

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_real_array_1(self, &
       a, b, tol) result(failure)
    !! Return failure for rank-1 real array values.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a(:), b(:)
    real, intent(in), optional :: tol !! Tolerance
    ! Locals:
    logical :: m(size(a))
    integer :: i, num

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         m = .not. (self%equal_tol(a, b, tol))
         num = count(m)
         if (num > 0) then
            call first_false_index_1(m, i)
            call failure%init(self%str(a(i)), self%str(b(i)), &
                 scalar = .false., index = self%str(i), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_real_array_1

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_real_array_2(self, &
       a, b, tol) result(failure)
    !! Return failure for rank-2 real array values.

    class(unit_test_type), intent(in out) :: self
    real, intent(in) :: a(:,:), b(:,:)
    real, intent(in), optional :: tol !! Tolerance
    ! Locals:
    logical :: m(size(a, 1), size(a, 2))
    integer :: ij(2), num

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         m = .not. (self%equal_tol(a, b, tol))
         num = count(m)
         if (num > 0) then
            call first_false_index_2(m, ij)
            call failure%init(self%str(a(ij(1), ij(2))), self%str(b(ij(1), ij(2))), &
                 scalar = .false., index = self%str(ij), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_real_array_2

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_double(self, &
       a, b) result(failure)
    !! Return failure for double precision values.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a, b

    call failure%init(self%str(a), self%str(b))
    
  end function unit_test_failure_double

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_double_array_1(self, &
       a, b, tol) result(failure)
    !! Return failure for rank-1 double precision array values.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a(:), b(:)
    real(dp), intent(in), optional :: tol !! Tolerance
    ! Locals:
    logical :: m(size(a))
    integer :: i, num

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         m = .not. (self%equal_tol(a, b, tol))
         num = count(m)
         if (num > 0) then
            call first_false_index_1(m, i)
            call failure%init(self%str(a(i)), self%str(b(i)), &
                 scalar = .false., index = self%str(i), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_double_array_1

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_double_array_2(self, &
       a, b, tol) result(failure)
    !! Return failure for rank-2 double precision array values.

    class(unit_test_type), intent(in out) :: self
    real(dp), intent(in) :: a(:,:), b(:,:)
    real(dp), intent(in), optional :: tol !! Tolerance
    ! Locals:
    logical :: m(size(a, 1), size(a, 2))
    integer :: ij(2), num

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         m = .not. (self%equal_tol(a, b, tol))
         num = count(m)
         if (num > 0) then
            call first_false_index_2(m, ij)
            call failure%init(self%str(a(ij(1), ij(2))), self%str(b(ij(1), ij(2))), &
                 scalar = .false., index = self%str(ij), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_double_array_2

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_complex(self, &
       a, b) result(failure)
    !! Return failure for complex values.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a, b

    call failure%init(self%str(a), self%str(b))
    
  end function unit_test_failure_complex

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_complex_array_1(self, &
       a, b, tol) result(failure)
    !! Return failure for rank-1 complex array values.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a(:), b(:)
    real, intent(in), optional :: tol !! Tolerance
    ! Locals:
    logical :: m(size(a))
    integer :: i, num

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         m = .not. (self%equal_tol(a, b, tol))
         num = count(m)
         if (num > 0) then
            call first_false_index_1(m, i)
            call failure%init(self%str(a(i)), self%str(b(i)), &
                 scalar = .false., index = self%str(i), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_complex_array_1

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_complex_array_2(self, &
       a, b, tol) result(failure)
    !! Return failure for rank-2 complex array values.

    class(unit_test_type), intent(in out) :: self
    complex, intent(in) :: a(:,:), b(:,:)
    real, intent(in), optional :: tol !! Tolerance
    ! Locals:
    logical :: m(size(a, 1), size(a, 2))
    integer :: ij(2), num

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         m = .not. (self%equal_tol(a, b, tol))
         num = count(m)
         if (num > 0) then
            call first_false_index_2(m, ij)
            call failure%init(self%str(a(ij(1), ij(2))), self%str(b(ij(1), ij(2))), &
                 scalar = .false., index = self%str(ij), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_complex_array_2

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_string(self, &
       a, b) result(failure)
    !! Return failure for string values.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a, b

    call failure%init(self%str(a), self%str(b))
    
  end function unit_test_failure_string

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_string_array_1(self, &
       a, b) result(failure)
    !! Return failure for rank-1 string array values.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a(:), b(:)
    ! Locals:
    logical :: m(size(a))
    integer :: i, num

    associate(na => size(a), nb => size(b))
      if (na == nb) then
         m = .not. (str_equal(a, b))
         num = count(m)
         if (num > 0) then
            call first_false_index_1(m, i)
            call failure%init(self%str(a(i)), self%str(b(i)), &
                 scalar = .false., index = self%str(i), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_string_array_1

!------------------------------------------------------------------------

  type(assertion_failure_type) function unit_test_failure_string_array_2(self, &
       a, b) result(failure)
    !! Return failure for rank-2 string array values.

    class(unit_test_type), intent(in out) :: self
    character(len = *), intent(in) :: a(:,:), b(:,:)
    ! Locals:
    logical :: m(size(a, 1), size(a, 2))
    integer :: ij(2), num

    associate(na => shape(a), nb => shape(b))
      if (all(na == nb)) then
         m = .not. (str_equal(a, b))
         num = count(m)
         if (num > 0) then
            call first_false_index_2(m, ij)
            call failure%init(self%str(a(ij(1), ij(2))), self%str(b(ij(1), ij(2))), &
                 scalar = .false., index = self%str(ij), num = self%str(num))
         end if
      else
         call failure%init(self%str(na), self%str(nb), FAILURE_REASON_SHAPE)
      end if
    end associate

  end function unit_test_failure_string_array_2

!------------------------------------------------------------------------

end module zofu
