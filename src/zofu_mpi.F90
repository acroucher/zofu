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
     procedure :: end_case => unit_test_mpi_end_case
     procedure :: global_assertions => unit_test_global_assertions
     procedure, public :: summary => unit_test_mpi_summary
     procedure :: failure_yaml => unit_test_mpi_failure_yaml
  end type unit_test_mpi_type

contains

!------------------------------------------------------------------------

  subroutine unit_test_mpi_end_case(self)
    !! Ends MPI test case. A case fails if there are failed assertions
    !! on any rank.

    class(unit_test_mpi_type), intent(in out) :: self
    ! Locals:
    logical :: global_case_failure
    integer :: ierr

    call mpi_reduce(self%case_assertions%failed > 0, &
         global_case_failure, 1, &
         MPI_LOGICAL, MPI_LOR, 0, MPI_COMM_WORLD, ierr)

    if (global_case_failure) then
       call self%cases%fail()
       self%failed = .true.
       self%passed = .not. (self%failed)
    else
       call self%cases%pass()
    end if

  end subroutine unit_test_mpi_end_case

!------------------------------------------------------------------------

  type(test_counter_type) function unit_test_global_assertions(self) &
       result(global_assertions)
    !! Returns assertions counter on rank 0 with data summed from all
    !! MPI ranks.

    class(unit_test_mpi_type), intent(in) :: self
    ! Locals:
    integer :: ierr

    call mpi_reduce(self%assertions%count, global_assertions%count, &
         1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call mpi_reduce(self%assertions%passed, global_assertions%passed, &
         1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    call mpi_reduce(self%assertions%failed, global_assertions%failed, &
         1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  end function unit_test_global_assertions

!------------------------------------------------------------------------

  subroutine unit_test_mpi_summary(self)
    !! Writes YAML summary of test statistics (from all ranks) to stdout.

    class(unit_test_mpi_type), intent(in) :: self
    ! Locals:
    integer :: rank, ierr
    type(test_counter_type) :: global_assertions

    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
    global_assertions = self%global_assertions()
    if (rank == 0) then
       write(*, '(a)') self%yaml(global_assertions)
    end if

  end subroutine unit_test_mpi_summary

!------------------------------------------------------------------------

  function unit_test_mpi_failure_yaml(self, name) result(msg)
    !! Return YAML string for failed assertion message, including MPI
    !! rank.

    class(unit_test_mpi_type), intent(in) :: self
    character(len = *), intent(in), optional :: name
    character(:), allocatable :: msg
    ! Locals:
    integer :: rank, ierr
    character(len = 32) :: rank_str

    msg = self%unit_test_type%failure_yaml(name)

    call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
    write(rank_str, '(i0)') rank
    msg = msg // ', "rank": ' // trim(rank_str)

  end function unit_test_mpi_failure_yaml

!------------------------------------------------------------------------  

end module zofu_mpi
