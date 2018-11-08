program test_string_asserts

  ! Test Zofu string asserts.

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

  call test%run(test_str_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_str_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_str_pass_unequal_lengths)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_str_pass_left_pad)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_str_array_1_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_str_array_1_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_str_array_1_fail_different_lengths)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%run(test_str_array_2_pass)
  call check(test, last_cases, last_assertions, 1, 1, 1, 0, OK)

  call test%run(test_str_array_2_fail)
  call check(test, last_cases, last_assertions, 1, 1, 0, 1, OK)

  call test%summary()

  if (.not. OK) stop 1

contains

  subroutine test_str_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert("foo", "foo")
  end subroutine test_str_pass

  subroutine test_str_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert("foo", "foobar")
  end subroutine test_str_fail

  subroutine test_str_pass_unequal_lengths(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert("foo", "foo   ")
  end subroutine test_str_pass_unequal_lengths

  subroutine test_str_pass_left_pad(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert("foo", "  foo")
  end subroutine test_str_pass_left_pad

  subroutine test_str_array_1_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(["a  ", "b  "], ["a  ", " b "])
  end subroutine test_str_array_1_pass

  subroutine test_str_array_1_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(["a  ", "b  "], ["a  ", "ab "])
  end subroutine test_str_array_1_fail

  subroutine test_str_array_1_fail_different_lengths(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert(["a  ", "b  "], ["a ", "b ", "ab"])
  end subroutine test_str_array_1_fail_different_lengths

  subroutine test_str_array_2_pass(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape(["a ", "b ", "c ", " d"], [2, 2]), &
         reshape(["a ", " b", "c ", "d "], [2, 2]))
  end subroutine test_str_array_2_pass

  subroutine test_str_array_2_fail(test)
    class(unit_test_type), intent(in out) :: test
    call test%assert( &
         reshape(["a ", "b ", "c ", " d"], [2, 2]), &
         reshape(["a ", "ab", "c ", "d "], [2, 2]))
  end subroutine test_str_array_2_fail

end program test_string_asserts
