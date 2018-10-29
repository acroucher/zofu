program test_asserts

  ! Test Zofu asserts with hand-coded driver program.

  use zofu
  implicit none
  type(unit_test_type) :: test
  logical :: OK
  integer, parameter :: dp = kind(0.d0) !! double precision kind

  call test%init()
  OK = .true.

  call test%run(assert_true)
  call check(test, 1, 1, 1, 0, OK)

  call test%run(assert_false)
  call check(test, 2, 2, 1, 1, OK)

  call test%run(assert_true_true)
  call check(test, 3, 3, 2, 1, OK)

  call test%run(assert_false_false)
  call check(test, 4, 4, 3, 1, OK)

  call test%run(assert_true_false)
  call check(test, 5, 5, 3, 2, OK)

  call test%run(test_logical_array_1_pass)
  call check(test, 6, 6, 4, 2, OK)

  call test%run(test_logical_array_1_fail)
  call check(test, 7, 7, 4, 3, OK)

  call test%run(test_integer_pass)
  call check(test, 8, 8, 5, 3, OK)

  call test%run(test_integer_fail)
  call check(test, 9, 9, 5, 4, OK)

  call test%run(test_integer_array_1_pass)
  call check(test, 10, 10, 6, 4, OK)

  call test%run(test_integer_array_1_fail)
  call check(test, 11, 11, 6, 5, OK)

  call test%run(test_real_pass)
  call check(test, 12, 12, 7, 5, OK)

  call test%run(test_real_fail)
  call check(test, 13, 13, 7, 6, OK)

  call test%run(test_real_both_zero)
  call check(test, 14, 14, 8, 6, OK)

  call test%run(test_real_large_difference_fail)
  call check(test, 15, 15, 8, 7, OK)

  call test%run(test_real_small_difference_pass)
  call check(test, 16, 16, 9, 7, OK)

  call test%run(test_real_small_difference_fail)
  call check(test, 17, 17, 9, 8, OK)

  call test%run(test_real_small_difference_tol_pass)
  call check(test, 18, 18, 10, 8, OK)

  call test%run(test_real_array_1_pass)
  call check(test, 19, 19, 11, 8, OK)

  call test%run(test_real_array_1_fail)
  call check(test, 20, 20, 11, 9, OK)

  call test%run(test_double_pass)
  call check(test, 21, 21, 12, 9, OK)

  call test%run(test_double_fail)
  call check(test, 22, 22, 12, 10, OK)

  call test%run(test_double_both_zero)
  call check(test, 23, 23, 13, 10, OK)

  call test%run(test_double_large_difference_fail)
  call check(test, 24, 24, 13, 11, OK)

  call test%run(test_double_small_difference_pass)
  call check(test, 25, 25, 14, 11, OK)

  call test%run(test_double_small_difference_fail)
  call check(test, 26, 26, 14, 12, OK)

  call test%run(test_double_small_difference_tol_pass)
  call check(test, 27, 27, 15, 12, OK)

  call test%run(test_double_array_pass)
  call check(test, 28, 28, 16, 12, OK)

  call test%run(test_double_array_fail)
  call check(test, 29, 29, 16, 13, OK)

  call test%run(test_logical_array_2_pass)
  call check(test, 30, 30, 17, 13, OK)
  
  call test%run(test_logical_array_2_fail)
  call check(test, 31, 31, 17, 14, OK)

  call test%run(test_integer_array_2_pass)
  call check(test, 32, 32, 18, 14, OK)

  call test%run(test_integer_array_2_fail)
  call check(test, 33, 33, 18, 15, OK)

  call test%run(test_real_array_2_pass)
  call check(test, 34, 34, 19, 15, OK)

  call test%run(test_real_array_2_fail)
  call check(test, 35, 35, 19, 16, OK)

  call test%run(test_double_array_2_pass)
  call check(test, 36, 36, 20, 16, OK)

  call test%run(test_double_array_2_fail)
  call check(test, 37, 37, 20, 17, OK)

  if (.not. OK) stop 1

contains

!------------------------------------------------------------------------

  subroutine check(test, expected_num_cases, expected_num_assertions, &
       expected_num_passed, expected_num_failed, OK)

    type(unit_test_type), intent(in) :: test
    integer, intent(in) :: expected_num_cases, expected_num_assertions, &
         expected_num_passed, expected_num_failed
    logical, intent(in out) :: OK

    OK = ( OK .and. &
         (test%num_cases == expected_num_cases) .and. &
         (test%num_assertions == expected_num_assertions) .and. &
         (test%num_passed_assertions == expected_num_passed) .and. &
         (test%num_failed_assertions == expected_num_failed))

  end subroutine check

!------------------------------------------------------------------------

  ! Logical tests

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

end program test_asserts
