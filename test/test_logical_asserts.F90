program test_logical_asserts

  ! Test Zofu logical asserts.

  use zofu
  use check_module

  implicit none
  type(unit_test_type) :: test
  type(counts_type) :: last
  logical :: OK

  call test%init()
  OK = .true.

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

  call test%summary()

  if (.not. OK) stop 1

contains

  subroutine assert_true(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(.true.)
  end subroutine assert_true

  subroutine assert_false(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(.false.)
  end subroutine assert_false

  subroutine assert_true_true(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(.true., .true.)
  end subroutine assert_true_true

  subroutine assert_false_false(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(.false., .false.)
  end subroutine assert_false_false

  subroutine assert_true_false(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(.true., .false.)
  end subroutine assert_true_false

  subroutine test_logical_array_1_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([.true., .false.], [.true., .false.])
  end subroutine test_logical_array_1_pass

  subroutine test_logical_array_1_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert([.true., .false., .true.], &
         [.true., .true., .true.])
  end subroutine test_logical_array_1_fail

  subroutine test_logical_array_2_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([.true., .false., .false., .true.], [2, 2]), &
         reshape([.true., .false., .false., .true.], [2, 2]))
  end subroutine test_logical_array_2_pass

  subroutine test_logical_array_2_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape([.true., .false., .false., .true.], [2, 2]), &
         reshape([.true., .false., .true., .false.], [2, 2]))
  end subroutine test_logical_array_2_fail

end program test_logical_asserts
