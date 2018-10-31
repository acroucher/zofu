program test_double_asserts

  ! Test Zofu double precision asserts.

  use zofu
  use check_module

  implicit none
  type(unit_test_type) :: test
  type(test_counter_type) :: last_cases, last_assertions
  logical :: OK

  call test%init()
  call last_cases%init()
  call last_assertions%init()
  OK = .true.

  call test%run(test_double_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_double_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_double_both_zero)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_double_large_difference_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_double_small_difference_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_double_small_difference_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_double_small_difference_tol_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_double_array_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_double_array_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_double_array_2_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_double_array_2_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%summary()

  if (.not. OK) stop 1

contains

  subroutine test_double_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(2.7_dp, 2.7_dp)
  end subroutine test_double_pass

  subroutine test_double_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(2.7_dp, 2.6_dp)
  end subroutine test_double_fail

  subroutine test_double_both_zero(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(0.0_dp, 0.0_dp)
  end subroutine test_double_both_zero

  subroutine test_double_large_difference_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(1.e-11_dp, -2.e16_dp)
  end subroutine test_double_large_difference_fail

  subroutine test_double_small_difference_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(10._dp, 10._dp + 1.e-6_dp)
  end subroutine test_double_small_difference_pass

  subroutine test_double_small_difference_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(-10._dp, -10._dp + 1.e-4_dp)
  end subroutine test_double_small_difference_fail

  subroutine test_double_small_difference_tol_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(-10._dp, -10._dp + 1.e-4_dp, tol = 5.e-4_dp)
  end subroutine test_double_small_difference_tol_pass

  subroutine test_double_array_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([2.718_dp, -3.142_dp], [2.718_dp, -3.142_dp])
  end subroutine test_double_array_pass

  subroutine test_double_array_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([2.718_dp, -3.142_dp, 1.618_dp], &
         [2.718_dp, -3.141_dp, 1.618_dp])
  end subroutine test_double_array_fail

  subroutine test_double_array_2_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([2.718_dp, -3.142_dp, 1.618_dp, 0.0_dp], [2, 2]), &
         reshape([2.718_dp, -3.142_dp, 1.618_dp, 0.0_dp], [2, 2]))
  end subroutine test_double_array_2_pass

  subroutine test_double_array_2_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([2.718_dp, -3.142_dp, 1.618_dp, 0._dp, 1._dp, -0.5_dp], &
         [2, 3]), &
         reshape([2.718_dp, -3.141_dp, 1.618_dp, 1._dp, 1._dp, -0.5_dp], &
         [2, 3]))
  end subroutine test_double_array_2_fail

end program test_double_asserts
