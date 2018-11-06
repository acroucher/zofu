module adder_tests

  use adder_module
  use zofu

  implicit none

  contains

!------------------------------------------------------------------------

    subroutine adder_test(test, a, b, c)

      class(unit_test_type), intent(in out) :: test
      real, intent(in) :: a, b, c
      ! Locals:
      type(adder_type) :: adder

      call adder%init(a, b)
      call test%assert(a, adder%a, name = 'adder a')
      call test%assert(b, adder%b, name = 'adder b')
      call adder%add()
      call test%assert(c, adder%result, name = 'adder result')

    end subroutine adder_test

!------------------------------------------------------------------------

    subroutine test_add1(test)

      class(unit_test_type), intent(in out) :: test

      call adder_test(test, 1., 2., 3.)

    end subroutine test_add1

!------------------------------------------------------------------------

    subroutine test_add2(test)

      ! Adder test with comment

      class(unit_test_type), intent(in out) :: test

      call adder_test(test, -1., 3., 2.)

    end subroutine test_add2

!------------------------------------------------------------------------

    subroutine test_add3_setup(test)
      ! Adder test with setup in title
      ! and a second comment line (should not be part of description)

      class(unit_test_type), intent(in out) :: test

      call adder_test(test, -2., 1., -1.)

    end subroutine test_add3_setup

!------------------------------------------------------------------------

    subroutine test_teardown_test(test)

      ! Adder test with teardown in title

      class(unit_test_type), intent(in out) :: test

      call adder_test(test, 3., 2., 5.)

    end subroutine test_teardown_test

!------------------------------------------------------------------------

    SUBROUTINE TEST_OLDSCHOOL(TEST)
    ! TEST ALL CAPS, MISSING END NAME
    CLASS(UNIT_TEST_TYPE), INTENT(IN OUT) :: TEST
    CALL ADDER_TEST(TEST, 5., 6., 11.)
    END SUBROUTINE

!------------------------------------------------------------------------

end module adder_tests
