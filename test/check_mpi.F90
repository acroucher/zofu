module check_mpi_module

  !! Utilities for checking numbers of cases, assertions in MPI tests.

  use mpi
  use zofu_mpi
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

    class(unit_test_mpi_type), intent(in) :: test
    type(counts_type), intent(in out) :: last
    integer, intent(in) :: cases, assertions, passed, failed
    logical, intent(in out) :: OK
    ! Locals:
    integer :: global_num_assertions
    integer :: global_num_passed, global_num_failed
    integer :: rank, ierr

    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
    
    call mpi_reduce(test%num_assertions, global_num_assertions, 1, &
           MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call mpi_reduce(test%num_passed_assertions, global_num_passed, 1, &
           MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call mpi_reduce(test%num_failed_assertions, global_num_failed, 1, &
           MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
       OK = ( OK .and. &
            (test%num_cases == last%cases + cases) .and. &
            (global_num_assertions == last%assertions + assertions) .and. &
            (global_num_passed == last%passed + passed) .and. &
            (global_num_failed == last%failed + failed))

       last%cases = test%num_cases
       last%assertions = global_num_assertions
       last%passed = global_num_passed
       last%failed = global_num_failed
    end if

  end subroutine check

!------------------------------------------------------------------------

end module check_mpi_module
