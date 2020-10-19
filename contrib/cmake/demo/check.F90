module check_module

  !! Utilities for checking numbers of cases, assertions in tests.

  use zofu
  implicit none
  
contains

!------------------------------------------------------------------------

  subroutine check(test, last_cases, last_assertions, &
       cases, assertions, passed, failed, OK)
    !! Checks total numbers of cases, assertions, passes and fails
    !! against expected values, given the numbers expected for the
    !! last case run.

    class(unit_test_type), intent(in) :: test
    type(test_counter_type), intent(in out) :: last_cases, last_assertions
    integer, intent(in) :: cases, assertions, passed, failed
    logical, intent(in out) :: OK

    OK = ( OK .and. &
         (test%cases%count == last_cases%count + cases) .and. &
         (test%assertions%count == last_assertions%count + assertions) .and. &
         (test%assertions%passed == last_assertions%passed + passed) .and. &
         (test%assertions%failed == last_assertions%failed + failed))

    last_cases = test%cases
    last_assertions = test%assertions

  end subroutine check

!------------------------------------------------------------------------

end module check_module
