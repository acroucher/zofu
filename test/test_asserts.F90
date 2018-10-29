program test_asserts

  ! Test Zofu asserts with hand-coded driver program.

  use zofu
  implicit none
  type counts_type
     integer :: cases = 0
     integer :: assertions = 0
     integer :: passed = 0
     integer :: failed = 0
  end type counts_type
  type(unit_test_type) :: test
  type(counts_type) :: last
  logical :: OK
  integer, parameter :: dp = kind(0.d0) !! double precision kind

  call test%init()
  OK = .true.

  ! Logical tests:

  call test%run(assert_true)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(assert_false)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(assert_true_true)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(assert_false_false)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(assert_true_false)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_logical_array_1_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_logical_array_1_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_logical_array_2_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_logical_array_2_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  ! Integer tests:

  call test%run(test_integer_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_integer_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_integer_array_1_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_integer_array_1_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_integer_array_2_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_integer_array_2_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  ! Real tests:

  call test%run(test_real_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_real_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_real_both_zero)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_real_large_difference_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_real_small_difference_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_real_small_difference_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_real_small_difference_tol_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_real_array_1_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_real_array_1_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_real_array_2_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_real_array_2_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  ! Double precision tests:

  call test%run(test_double_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_double_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_double_both_zero)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_double_large_difference_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_double_small_difference_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_double_small_difference_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_double_small_difference_tol_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_double_array_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_double_array_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_double_array_2_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_double_array_2_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  ! String tests:

  call test%run(test_str_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_str_pass_unequal_lengths)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_str_pass_left_pad)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_str_array_1_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_str_array_1_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%run(test_str_array_2_pass)
  call check(test, last, 1, 1, 1, 0, OK)

  call test%run(test_str_array_2_fail)
  call check(test, last, 1, 1, 0, 1, OK)

  call test%summary()

  if (.not. OK) stop 1

contains

!------------------------------------------------------------------------

  subroutine check(test, last, cases, assertions, passed, failed, OK)
    !! Checks total numbers of cases, assertions, passes and fails
    !! against expected values, given the numbers expected for the
    !! last case run.

    type(unit_test_type), intent(in) :: test
    type(counts_type), intent(in out) :: last
    integer, intent(in) :: cases, assertions, passed, failed
    logical, intent(in out) :: OK

    OK = ( OK .and. &
         (test%num_cases == last%cases + cases) .and. &
         (test%num_assertions == last%assertions + assertions) .and. &
         (test%num_passed_assertions == last%passed + passed) .and. &
         (test%num_failed_assertions == last%failed + failed))

    last%cases = test%num_cases
    last%assertions = test%num_assertions
    last%passed = test%num_passed_assertions
    last%failed = test%num_failed_assertions

  end subroutine check

!------------------------------------------------------------------------
! Logical tests
!------------------------------------------------------------------------

  subroutine assert_true(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(.true.)
  end subroutine assert_true

  subroutine assert_false(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(.false.)
  end subroutine assert_false

  subroutine assert_true_true(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(.true., .true.)
  end subroutine assert_true_true

  subroutine assert_false_false(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(.false., .false.)
  end subroutine assert_false_false

  subroutine assert_true_false(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(.true., .false.)
  end subroutine assert_true_false

  subroutine test_logical_array_1_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert([.true., .false.], [.true., .false.])
  end subroutine test_logical_array_1_pass

  subroutine test_logical_array_1_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert([.true., .false., .true.], &
         [.true., .true., .true.])
  end subroutine test_logical_array_1_fail

  subroutine test_logical_array_2_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([.true., .false., .false., .true.], [2, 2]), &
         reshape([.true., .false., .false., .true.], [2, 2]))
  end subroutine test_logical_array_2_pass

  subroutine test_logical_array_2_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([.true., .false., .false., .true.], [2, 2]), &
         reshape([.true., .false., .true., .false.], [2, 2]))
  end subroutine test_logical_array_2_fail

!------------------------------------------------------------------------
! Integer tests
!------------------------------------------------------------------------

  subroutine test_integer_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(1, 1)
  end subroutine test_integer_pass

  subroutine test_integer_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(1, -2)
  end subroutine test_integer_fail

  subroutine test_integer_array_1_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert([1, -1], [1, -1])
  end subroutine test_integer_array_1_pass

  subroutine test_integer_array_1_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert([-1, 2, 7], [-1, 2, -7])
  end subroutine test_integer_array_1_fail

  subroutine test_integer_array_2_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([1, 0, 0, 1], [2, 2]), &
         reshape([1, 0, 0, 1], [2, 2]))
  end subroutine test_integer_array_2_pass

  subroutine test_integer_array_2_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([1, 2, 3, 4, 5, 6], [3, 2]), &
         reshape([1, 2, 2, 4, 5, 6], [3, 2]))
  end subroutine test_integer_array_2_fail
  
!------------------------------------------------------------------------
! Real tests
!------------------------------------------------------------------------

  subroutine test_real_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(2.7, 2.7)
  end subroutine test_real_pass

  subroutine test_real_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(2.7, 2.6)
  end subroutine test_real_fail

  subroutine test_real_both_zero(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(0.0, 0.0)
  end subroutine test_real_both_zero

  subroutine test_real_large_difference_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(1.e-11, -2.e16)
  end subroutine test_real_large_difference_fail

  subroutine test_real_small_difference_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(10., 10. + 1.e-6)
  end subroutine test_real_small_difference_pass

  subroutine test_real_small_difference_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(-10., -10. + 1.e-4)
  end subroutine test_real_small_difference_fail

  subroutine test_real_small_difference_tol_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(-10., -10. + 1.e-4, tol = 5.e-4)
  end subroutine test_real_small_difference_tol_pass

  subroutine test_real_array_1_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert([2.718, -3.142], [2.718, -3.142])
  end subroutine test_real_array_1_pass

  subroutine test_real_array_1_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert([2.718, -3.142, 1.618], [2.718, -3.141, 1.618])
  end subroutine test_real_array_1_fail

  subroutine test_real_array_2_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([2.718, -3.142, 1.618, 0.0], [2, 2]), &
         reshape([2.718, -3.142, 1.618, 0.0], [2, 2]))
  end subroutine test_real_array_2_pass

  subroutine test_real_array_2_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([2.718, -3.142, 1.618, 0., 1., -0.5], [2, 3]), &
         reshape([2.718, -3.141, 1.618, 1., 1., -0.5], [2, 3]))
  end subroutine test_real_array_2_fail

!------------------------------------------------------------------------
! Double tests
!------------------------------------------------------------------------

  subroutine test_double_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(2.7_dp, 2.7_dp)
  end subroutine test_double_pass

  subroutine test_double_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(2.7_dp, 2.6_dp)
  end subroutine test_double_fail

  subroutine test_double_both_zero(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(0.0_dp, 0.0_dp)
  end subroutine test_double_both_zero

  subroutine test_double_large_difference_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(1.e-11_dp, -2.e16_dp)
  end subroutine test_double_large_difference_fail

  subroutine test_double_small_difference_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(10._dp, 10._dp + 1.e-6_dp)
  end subroutine test_double_small_difference_pass

  subroutine test_double_small_difference_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(-10._dp, -10._dp + 1.e-4_dp)
  end subroutine test_double_small_difference_fail

  subroutine test_double_small_difference_tol_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(-10._dp, -10._dp + 1.e-4_dp, tol = 5.e-4_dp)
  end subroutine test_double_small_difference_tol_pass

  subroutine test_double_array_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert([2.718_dp, -3.142_dp], [2.718_dp, -3.142_dp])
  end subroutine test_double_array_pass

  subroutine test_double_array_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert([2.718_dp, -3.142_dp, 1.618_dp], &
         [2.718_dp, -3.141_dp, 1.618_dp])
  end subroutine test_double_array_fail

  subroutine test_double_array_2_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([2.718_dp, -3.142_dp, 1.618_dp, 0.0_dp], [2, 2]), &
         reshape([2.718_dp, -3.142_dp, 1.618_dp, 0.0_dp], [2, 2]))
  end subroutine test_double_array_2_pass

  subroutine test_double_array_2_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([2.718_dp, -3.142_dp, 1.618_dp, 0._dp, 1._dp, -0.5_dp], &
         [2, 3]), &
         reshape([2.718_dp, -3.141_dp, 1.618_dp, 1._dp, 1._dp, -0.5_dp], &
         [2, 3]))
  end subroutine test_double_array_2_fail

!------------------------------------------------------------------------
! String tests
!------------------------------------------------------------------------

  subroutine test_str_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert("foo", "foo")
  end subroutine test_str_pass

  subroutine test_str_pass_unequal_lengths(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert("foo", "foo   ")
  end subroutine test_str_pass_unequal_lengths

  subroutine test_str_pass_left_pad(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert("foo", "  foo")
  end subroutine test_str_pass_left_pad

  subroutine test_str_array_1_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(["a  ", "b  "], ["a  ", " b "])
  end subroutine test_str_array_1_pass

  subroutine test_str_array_1_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert(["a  ", "b  "], ["a  ", "ab "])
  end subroutine test_str_array_1_fail

  subroutine test_str_array_2_pass(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape(["a ", "b ", "c ", " d"], [2, 2]), &
         reshape(["a ", " b", "c ", "d "], [2, 2]))
  end subroutine test_str_array_2_pass

  subroutine test_str_array_2_fail(test)
    type(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape(["a ", "b ", "c ", " d"], [2, 2]), &
         reshape(["a ", "ab", "c ", "d "], [2, 2]))
  end subroutine test_str_array_2_fail

!------------------------------------------------------------------------

end program test_asserts
