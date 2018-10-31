program test_mpi

  ! Test Zofu MPI asserts.

  use mpi
  use zofu
  use zofu_mpi
  use check_mpi_module

  implicit none
  type(unit_test_mpi_type) :: test
  type(counts_type) :: last
  logical :: OK
  integer :: rank, size, ierr

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
  call mpi_comm_size(MPI_COMM_WORLD, size, ierr)
  
  call test%init()
  OK = .true.

  call test%run(test_integer_pass)
  call check(test, last, 1, size, size, 0, OK)

  call test%run(test_integer_fail)
  call check(test, last, 1, size, size - 1, 1, OK)

  call test%summary()

  call mpi_finalize(ierr)
  if (.not. OK) stop 1

contains

  subroutine test_integer_pass(test)
    class(unit_test_type), intent(in out) :: test
    select type(test)
    type is (unit_test_mpi_type)
       call test%assert(rank, rank)
    end select
  end subroutine test_integer_pass

  subroutine test_integer_fail(test)
    class(unit_test_type), intent(in out) :: test
    ! Locals:
    integer :: expected
    select type(test)
    type is (unit_test_mpi_type)
       if (rank == size - 1) then
          expected = rank + 1
       else
          expected = rank
       end if
       call test%assert(expected, rank)
    end select
  end subroutine test_integer_fail
  
end program test_mpi
