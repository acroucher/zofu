program test_real_asserts

  ! Test Zofu real asserts.

  use zofu
  use check_module

  implicit none

  type(unit_test_type) :: test
  type(test_counter_type) :: last_cases, last_assertions
  logical :: OK

  continue

  call test%init()
  call last_cases%init()
  call last_assertions%init()
  OK = .true.

  call test%run(test_real_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_real_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_real_both_zero)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_real_large_difference_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_real_small_difference_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_real_small_difference_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_real_small_difference_tol_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_real_array_1_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_real_array_1_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_real_array_1_fail_different_sizes)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_real_array_2_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_real_array_2_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%summary()

  if (test%assertions%failed > 0) then
    stop 2
  else if (.not. OK) then
    stop 1
  end if

contains

  subroutine test_real_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(2.7, 2.7)
  end subroutine test_real_pass

  subroutine test_real_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(2.7, 2.6)
  end subroutine test_real_fail

  subroutine test_real_both_zero(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(0.0, 0.0)
  end subroutine test_real_both_zero

  subroutine test_real_large_difference_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(1.e-11, -2.e16)
  end subroutine test_real_large_difference_fail

  subroutine test_real_small_difference_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(10., 10. + 1.e-6)
  end subroutine test_real_small_difference_pass

  subroutine test_real_small_difference_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(-10., -10. + 1.e-4)
  end subroutine test_real_small_difference_fail

  subroutine test_real_small_difference_tol_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(-10., -10. + 1.e-4, tol = 5.e-4)
  end subroutine test_real_small_difference_tol_pass

  subroutine test_real_array_1_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([2.718, -3.142], [2.718, -3.142])
  end subroutine test_real_array_1_pass

  subroutine test_real_array_1_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([2.718, -3.142, 1.618], [2.718, -3.141, 1.618])
  end subroutine test_real_array_1_fail

  subroutine test_real_array_1_fail_different_sizes(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([2.718, -3.142], [2.718, -3.142, 1.01])
  end subroutine test_real_array_1_fail_different_sizes

  subroutine test_real_array_2_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([2.718, -3.142, 1.618, 0.0], [2, 2]), &
         reshape([2.718, -3.142, 1.618, 0.0], [2, 2]))
  end subroutine test_real_array_2_pass

  subroutine test_real_array_2_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([2.718, -3.142, 1.618, 0., 1., -0.5], [2, 3]), &
         reshape([2.718, -3.141, 1.618, 1., 1., -0.5], [2, 3]))
  end subroutine test_real_array_2_fail

end program test_real_asserts
