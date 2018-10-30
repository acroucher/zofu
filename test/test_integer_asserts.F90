program test_integer_asserts

  ! Test Zofu integer asserts.

  use zofu
  use check_module

  implicit none
  type(unit_test_type) :: test
  type(counts_type) :: last
  logical :: OK

  call test%init()
  OK = .true.

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

  call test%summary()

  if (.not. OK) stop 1

contains

  subroutine test_integer_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(1, 1)
  end subroutine test_integer_pass

  subroutine test_integer_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(1, -2)
  end subroutine test_integer_fail

  subroutine test_integer_array_1_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([1, -1], [1, -1])
  end subroutine test_integer_array_1_pass

  subroutine test_integer_array_1_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([-1, 2, 7], [-1, 2, -7])
  end subroutine test_integer_array_1_fail

  subroutine test_integer_array_2_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([1, 0, 0, 1], [2, 2]), &
         reshape([1, 0, 0, 1], [2, 2]))
  end subroutine test_integer_array_2_pass

  subroutine test_integer_array_2_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([1, 2, 3, 4, 5, 6], [3, 2]), &
         reshape([1, 2, 2, 4, 5, 6], [3, 2]))
  end subroutine test_integer_array_2_fail
  
end program test_integer_asserts
