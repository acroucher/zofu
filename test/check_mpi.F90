module check_mpi_module

  !! Utilities for checking numbers of cases, assertions in MPI tests.

  use mpi
  use zofu, only: test_counter_type
  use zofu_mpi
  implicit none

contains

!------------------------------------------------------------------------

  subroutine check(test, last_cases, last_assertions, &
       cases, assertions, passed, failed, OK)
    !! Checks total numbers of cases, assertions, passes and fails
    !! against expected values, given the numbers expected for the
    !! last case run.

    class(unit_test_mpi_type), intent(in) :: test
    type(test_counter_type), intent(in out) :: last_cases, last_assertions
    integer, intent(in) :: cases, assertions, passed, failed
    logical, intent(in out) :: OK
    ! Locals:
    integer :: global_num_assertions
    integer :: global_num_passed, global_num_failed
    integer :: rank, ierr

    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
    
    call mpi_reduce(test%assertions%count, global_num_assertions, 1, &
           MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call mpi_reduce(test%assertions%passed, global_num_passed, 1, &
           MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call mpi_reduce(test%assertions%failed, global_num_failed, 1, &
           MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
       OK = ( OK .and. &
            (test%cases%count == last_cases%count + cases) .and. &
            (global_num_assertions == last_assertions%count + assertions) .and. &
            (global_num_passed == last_assertions%passed + passed) .and. &
            (global_num_failed == last_assertions%failed + failed))

       last_cases%count = test%cases%count
       last_assertions%count = global_num_assertions
       last_assertions%passed = global_num_passed
       last_assertions%failed = global_num_failed
    end if

  end subroutine check

!------------------------------------------------------------------------

end module check_mpi_module
