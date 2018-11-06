module adder_test_with_setup

  use zofu
  use adder_module
  use adder_tests, only: adder_test

  implicit none

  real :: y = 0.0

contains

!------------------------------------------------------------------------

  subroutine setup()

    y = 2.718

  end subroutine setup

!------------------------------------------------------------------------

  subroutine teardown

    real, parameter :: end_y = 0.0
    y = end_y

  end subroutine teardown

!------------------------------------------------------------------------

  subroutine test_1(test)

    class(unit_test_type), intent(in out) :: test

    call adder_test(test, 1., y, 3.718)

  end subroutine test_1

!------------------------------------------------------------------------

  subroutine test_2()

    ! Test 2 with setup

    class(unit_test_type), intent(in out) :: test

    call adder_test(test, -1., y, 1.718)

  end subroutine test_2

!------------------------------------------------------------------------

end module adder_test_with_setup
