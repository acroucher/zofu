module check_module

  !! Utilities for checking numbers of cases, assertions in tests.

  use zofu
  implicit none

  type counts_type
     integer :: cases = 0
     integer :: assertions = 0
     integer :: passed = 0
     integer :: failed = 0
  end type counts_type
  
contains

!------------------------------------------------------------------------

  subroutine check(test, last, cases, assertions, passed, failed, OK)
    !! Checks total numbers of cases, assertions, passes and fails
    !! against expected values, given the numbers expected for the
    !! last case run.

    class(unit_test_type), intent(in) :: test
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

end module check_module
