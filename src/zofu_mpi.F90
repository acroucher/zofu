!   Copyright 2018 University of Auckland.

!   This file is part of Zofu.

!   Zofu is free software: you can redistribute it and/or modify
!   it under the terms of the GNU Lesser General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.

!   Zofu is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU Lesser General Public License for more details.

!   You should have received a copy of the GNU Lesser General Public License
!   along with Zofu.  If not, see <http://www.gnu.org/licenses/>.

module zofu_mpi

  !! Zofu (Zofu is Object-oriented Fortran Unit-testing) unit test
  !! type for tests parallelized with MPI.

  use zofu
  use mpi

  implicit none
  private

  type, public, extends(unit_test_type) :: unit_test_mpi_type
     !! Type for unit test parallelized using MPI.
   contains
     procedure :: mpi_reduce => unit_test_mpi_reduce
     procedure, public :: summary => unit_test_mpi_summary
     procedure :: fail_assertion_message => unit_test_mpi_fail_assertion_message
  end type unit_test_mpi_type

contains

!------------------------------------------------------------------------

  subroutine unit_test_mpi_reduce(self)
    !! Adds up test statistics from all MPI ranks.

    class(unit_test_mpi_type), intent(in out) :: self

    call reduce_update(self%assertions%count)
    call reduce_update(self%assertions%passed)
    call reduce_update(self%assertions%failed)

  contains

    subroutine reduce_update(count)
      integer, intent(in out) :: count
      ! Locals:
      integer :: total_count, ierr
      call mpi_reduce(count, total_count, 1, &
           MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
      count = total_count
    end subroutine reduce_update

  end subroutine unit_test_mpi_reduce

!------------------------------------------------------------------------

  subroutine unit_test_mpi_summary(self)
    !! Writes YAML summary of test statistics (from all ranks) to stdout.

    class(unit_test_mpi_type), intent(in out) :: self
    ! Locals:
    integer :: rank, ierr

    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
    call self%mpi_reduce()
    if (rank == 0) then
       call self%unit_test_type%summary()
    end if

  end subroutine unit_test_mpi_summary

!------------------------------------------------------------------------

  function unit_test_mpi_fail_assertion_message(self, name) result(msg)
    !! Return YAML string for failed assertion message, including MPI
    !! rank.

    class(unit_test_mpi_type), intent(in) :: self
    character(len = *), intent(in), optional :: name
    character(:), allocatable :: msg
    ! Locals:
    integer :: rank, ierr
    character(len = 32) :: rank_str

    msg = self%unit_test_type%fail_assertion_message(name)

    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
    write(rank_str, '(i0)') rank
    msg = msg // ', "rank": ' // trim(rank_str)

  end function unit_test_mpi_fail_assertion_message

!------------------------------------------------------------------------  

end module zofu_mpi
